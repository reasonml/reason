/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open Core.Std;

open Async.Std;

open Jenga_lib.Api;

/* general helpers */
let ( *>>| ) = Dep.map;

let ( *>>= ) = Dep.bind;

let rel = Path.relative;

let ts = Path.to_string;

let root = Path.the_root;

let bash dir::dir command => Action.process dir::dir prog::"bash" args::["-c", command] ();

let bashf dir::dir fmt => ksprintf (fun str => bash dir::dir str) fmt;

let nonBlank s =>
  switch (String.strip s) {
  | "" => false
  | _ => true
  };

/* assumes there is a suffix to chop. Throws otherwise */
let chopSuffixExn str => String.slice str 0 (String.rindex_exn str '.');

let fileNameNoExtNoDir path => Path.basename path |> chopSuffixExn;

fileNameNoExtNoDir;

/* testing/logging helpers */
let tap n a =>
  if (n == 0) {
    print_endline "tap---------";
    print_endline a;
    a
  } else {
    a
  };

tap;

let tapl n a =>
  if (n == 0) {
    print_endline "tapl---------";
    List.iter f::print_endline a;
    a
  } else {
    a
  };

tapl;

let taplp n a =>
  if (n == 0) {
    print_endline "taplp---------";
    List.iter f::(fun a => print_endline (ts a)) a;
    a
  } else {
    a
  };

taplp;

let tapp n a =>
  if (n == 0) {
    print_endline "tapp---------";
    print_endline @@ ts a;
    a
  } else {
    a
  };

tapp;

let tapAssocList n a =>
  if (n == 0) {
    print_endline "tapAssocList---------";
    List.iter
      a f::(fun (path, deps) => print_endline @@ (path ^ ": " ^ String.concat sep::" " deps));
    a
  } else {
    a
  };

tapAssocList;

let topLibName = "top";

let nodeModulesRoot = rel dir::root "node_modules";

let buildDirRoot = rel dir::root "_build";

let topSrcDir = rel dir::root "src";

/* revert these to String.capitalize and String.uncapitalize to recover *from* our new assumption that file
   names are capitalized */
let stringCapitalize a => a;

let stringUncapitalize a => a;

stringCapitalize;

stringUncapitalize;

/* let allThirdPartyDeps = Dep.subdirs dir::nodeModulesRoot *>>| (
     fun paths =>
       List.map paths f::(fun path => fileNameNoExtNoDir  path |> String.uppercase)
   );

   allThirdPartyDeps; */
let ocamlDepModules sourcePath::sourcePath => Dep.subdirs dir::nodeModulesRoot *>>= (
  fun subdirs => {
    let execDir = Path.dirname sourcePath;
    let thirdPartyBuildRoots =
      List.map subdirs f::(fun subdir => rel dir::buildDirRoot (Path.basename subdir));
    Dep.action_stdout (
      Dep.all_unit [
        Dep.path sourcePath,
        Dep.all_unit (
          List.map
            thirdPartyBuildRoots
            f::(fun buildRoot => rel dir::buildRoot (Path.basename buildRoot ^ ".cmi") |> Dep.path)
        )
      ]
        *>>| (
        fun () =>
          bashf
            dir::execDir
            "ocamldep -pp refmt %s -ml-synonym .re -mli-synonym .rei -one-line %s"
            (
              thirdPartyBuildRoots |>
                List.map f::(fun path => Path.reach_from dir::execDir path) |>
                List.map f::(fun path => "-I " ^ path) |>
                String.concat sep::" "
            )
            (Path.basename sourcePath)
      )
    )
      *>>| (
      fun string =>
        switch (
          String.strip string |> String.split on::'\n' |> List.hd_exn |> String.split on::':'
        ) {
        | [original, deps] =>
          String.split deps on::' ' |>
            List.filter f::nonBlank |>
            List.map f::chopSuffixExn |>
            /* TODO: mother of all fragile logic. */
            /* node_modules/bookshop/src/Index.cmo : node_modules/bookshop/src/Index.cmi */
            /* ^ this means that there's a rei interface file for it. Filter it out */
            List.filter f::(fun m => m != chopSuffixExn original) |>
            /* TODO: fragile logic. The result might be ../_build/bookshop/bookshop.cmo so this is how we
               temporarily detect it */
            List.map
              f::(
                fun m =>
                  switch (String.rindex m '/') {
                  | None => m
                  | Some idx => String.slice m (idx + 1) (String.length m) |> String.capitalize
                  }
              ) |>
            tapl 1
        | _ => failwith "expected exactly one ':' in ocamldep output line"
        }
    )
  }
);

ocamlDepModules;

let getThirdPartyDepsForLib srcDir::srcDir => Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
  fun sourcePaths =>
    Dep.all (List.map sourcePaths f::(fun sourcePath => ocamlDepModules sourcePath::sourcePath)) *>>| (
    fun sourcePathsDeps => {
      let internalDeps =
        List.map sourcePaths f::(fun path => fileNameNoExtNoDir path |> stringCapitalize);
      List.concat sourcePathsDeps |>
        List.dedup |>
        List.filter f::(fun dep => not (List.exists internalDeps f::(fun dep' => dep == dep')))
    }
  )
);

getThirdPartyDepsForLib;

/* this isn't completely generic. mainNode is the start node. We potentially don't traverse the whole graph,
   only the subgraph connected to mainNode (aka dangling dependencies not used by mainNode are ignored) */
let topologicalSort mainNode::mainNode assocListGraph => {
  ignore @@ tapAssocList 1 assocListGraph;
  let rec topologicalSort' currNode assocListGraph accum => {
    let nodeDeps =
      switch (List.find assocListGraph f::(fun (n, _) => n == currNode)) {
      /* node not found: presume to be third-party dep. This is slightly dangerous because it might also mean
         we didn't construct the graph correctly. */
      | None => []
      | Some (_, nodeDeps') => nodeDeps'
      };
    List.iter nodeDeps f::(fun dep => topologicalSort' dep assocListGraph accum);
    if (not @@ List.exists accum.contents f::(fun n => n == currNode)) {
      accum := [currNode, ...accum.contents]
    }
  };
  let accum = {contents: []};
  topologicalSort' mainNode assocListGraph accum;
  List.rev accum.contents
};

topologicalSort;

let sortTransitiveThirdParties
    topLibName::topLibName
    nodeModulesRoot::nodeModulesRoot
    buildDirRoot::buildDirRoot => {
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  getThirdPartyDepsForLib srcDir::topSrcDir *>>= (
    fun topThirdPartyDeps => {
      let thirdPartiesSrcDirs =
        List.map
          topThirdPartyDeps
          f::(fun dep => rel dir::(rel dir::nodeModulesRoot (String.uncapitalize dep)) "src");
      let thirdPartiesThirdPartyDepsD = Dep.all (
        List.map thirdPartiesSrcDirs f::(fun srcDir => getThirdPartyDepsForLib srcDir::srcDir)
      );
      let topLibModuleName = String.capitalize topLibName;
      thirdPartiesThirdPartyDepsD *>>| (
        fun thirdPartiesThirdPartyDeps =>
          List.zip_exn
            [topLibModuleName, ...topThirdPartyDeps]
            [topThirdPartyDeps, ...thirdPartiesThirdPartyDeps] |>
            topologicalSort mainNode::topLibModuleName |>
            /* remove the top lib node itself from the sorted result */
            List.filter f::(fun m => m != topLibModuleName)
      )
    }
  )
};

sortTransitiveThirdParties;

let sortPathsTopologically buildDirRoot::buildDirRoot libName::libName dir::dir paths::paths => {
  ignore buildDirRoot;
  ignore libName;
  /* don't remove this piece of code! This is the `ocamldep -sort`-less version. Except it doesn't traverse
     the whole graph (see comment on topologicalSort) so we get free dead code elimination. But we do want
     merlin to pick up newly added files, and DCE means they're not even compiled. We'll change
     topologicalSort to traverse the whole graph soon. */
  /* let pathsAsModules =
       List.map paths f::(fun path => fileNameNoExtNoDir  path |> stringCapitalize);
     let sourcePathsDepsD = Dep.all (List.map paths f::(fun path => ocamlDepModules sourcePath::path));
     sourcePathsDepsD *>>| (
       fun sourcePathsDeps =>
         List.zip_exn pathsAsModules sourcePathsDeps |>
           topologicalSort mainNode::"Main" |>
           List.filter f::(fun m => List.exists pathsAsModules f::(fun m' => m' == m)) |>
           List.map f::(fun m => rel dir::dir (stringUncapitalize m ^ ".re")) |>
           taplp 0
     ) */
  Dep.action_stdout (
    Dep.all_unit (List.map paths f::Dep.path) *>>| (
      fun () => {
        let pathsString = List.map (taplp 1 paths) f::Path.basename |> String.concat sep::" ";
        bashf
          dir::dir
          "ocamldep -pp refmt -ml-synonym .re -mli-synonym .rei -sort -one-line %s"
          (tap 1 pathsString)
      }
    )
  )
    *>>| (
    fun string =>
      String.split string on::' ' |>
        List.filter f::nonBlank |> List.map f::(rel dir::dir) |> taplp 1
  )
};

sortPathsTopologically;

let compileLibScheme
    isTopLevelLib::isTopLevelLib=true
    srcDir::srcDir
    libName::libName
    buildDir::buildDir
    nodeModulesRoot::nodeModulesRoot
    buildDirRoot::buildDirRoot => {
  ignore isTopLevelLib;
  let moduleAliasFilePath = rel dir::buildDir (libName ^ ".re");
  let moduleAliasCmoPath = rel dir::buildDir (libName ^ ".cmo");
  let moduleAliasCmiPath = rel dir::buildDir (libName ^ ".cmi");
  let moduleAliasCmtPath = rel dir::buildDir (libName ^ ".cmt");
  Scheme.dep (
    Dep.glob_listing (Glob.create dir::srcDir "*.re") *>>= (
      fun unsortedPaths => {
        ignore 1;
        sortPathsTopologically
          buildDirRoot::buildDirRoot libName::libName dir::srcDir paths::unsortedPaths
          *>>| (
          fun sortedPaths => {
            /* module alias file generation */
            let moduleAliasContent =
              List.map
                unsortedPaths
                f::(
                  fun path => {
                    let name = fileNameNoExtNoDir path;
                    Printf.sprintf
                      "let module %s = %s__%s;\n"
                      (stringCapitalize name)
                      (String.capitalize libName)
                      name
                  }
                ) |>
                String.concat sep::"";
            let moduleAliasContentRules = [
              Rule.create
                targets::[moduleAliasFilePath]
                (Dep.return (Action.save moduleAliasContent target::moduleAliasFilePath))
            ];
            let moduleAliasCompileRules = [
              Rule.create
                targets::[moduleAliasCmoPath, tapp 1 moduleAliasCmiPath, moduleAliasCmtPath]
                (
                  Dep.path moduleAliasFilePath *>>| (
                    fun () =>
                      bashf
                        dir::buildDir
                        /* TODO: explain the warning suppression */
                        "ocamlc -pp refmt %s -bin-annot -g -no-alias-deps -w -49 -w -30 -w -40 -c -impl %s -o %s"
                        /* some jsoo includes were here. I don't think they're needed */
                        ""
                        (Path.basename moduleAliasFilePath)
                        (Path.basename moduleAliasCmoPath)
                  )
                )
            ];
            let sourcesCompileRules =
              Scheme.rules_dep @@
                Dep.all @@
                List.map
                  unsortedPaths
                  f::(
                    fun path => ocamlDepModules sourcePath::path *>>| (
                      fun modules => {
                        let firstPartyModules =
                          List.filter
                            (tapl 1 modules)
                            f::(
                              fun m =>
                                List.exists
                                  unsortedPaths
                                  f::(
                                    fun path => (fileNameNoExtNoDir path |> stringCapitalize) == m
                                  )
                            );
                        let thirdPartyModules =
                          List.filter
                            modules
                            f::(
                              fun m => not (
                                List.exists
                                  unsortedPaths
                                  f::(
                                    fun path => (fileNameNoExtNoDir path |> stringCapitalize) == m
                                  )
                              )
                            );
                        let firstPartyModuleDeps =
                          List.map
                            (tapl 1 firstPartyModules)
                            /* compiling here only needs cmi */
                            f::(
                              fun m => Dep.path (
                                rel dir::buildDir (libName ^ "__" ^ stringUncapitalize m ^ ".cmi")
                              )
                            );
                        let outNameNoExtNoDir = libName ^ "__" ^ fileNameNoExtNoDir path;
                        let outCmi = rel dir::buildDir (outNameNoExtNoDir ^ ".cmi") |> tapp 1;
                        let outCmo = rel dir::buildDir (outNameNoExtNoDir ^ ".cmo");
                        let outCmt = rel dir::buildDir (outNameNoExtNoDir ^ ".cmt");
                        Rule.create
                          targets::[outCmi, outCmo, outCmt]
                          (
                            Dep.all_unit (
                              [
                                Dep.path path,
                                Dep.path moduleAliasCmiPath,
                                Dep.path moduleAliasCmoPath,
                                Dep.path moduleAliasCmtPath,
                                Dep.path moduleAliasFilePath
                              ] @
                                firstPartyModuleDeps @
                                [
                                  Dep.all_unit (
                                    List.map
                                      (tapl 1 thirdPartyModules)
                                      f::(
                                        fun m => {
                                          let libName = String.uncapitalize m;
                                          Dep.glob_listing (
                                            Glob.create
                                              dir::(
                                                rel dir::(rel dir::nodeModulesRoot libName) "src"
                                              )
                                              "*.re"
                                          )
                                            *>>= (
                                            fun sources => Dep.all_unit (
                                              List.map
                                                sources
                                                f::(
                                                  fun sourcePath => Dep.path (
                                                    rel
                                                      dir::(rel dir::buildDirRoot libName)
                                                      (
                                                        libName ^
                                                          "__" ^
                                                          fileNameNoExtNoDir sourcePath ^
                                                          ".cmi"
                                                      )
                                                  )
                                                )
                                            )
                                          )
                                        }
                                      )
                                  )
                                ]
                            )
                              *>>| (
                              fun () =>
                                bashf
                                  dir::buildDir
                                  /* TODO: explain the warning suppression */
                                  "ocamlc -pp refmt -bin-annot -g -w -30 -w -40 -open %s -I `ocamlfind query js_of_ocaml` `ocamlfind query js_of_ocaml`/js_of_ocaml.cma -I %s %s -o %s -intf-suffix rei -c -impl %s"
                                  (String.capitalize libName)
                                  /* "-I /Users/chenglou/.opam/4.02.3/lib/js_of_ocaml /Users/chenglou/.opam/4.02.3/lib/js_of_ocaml/js_of_ocaml.cma" */
                                  (ts buildDir)
                                  (
                                    List.map
                                      thirdPartyModules
                                      f::(
                                        fun m => "-I " ^ (
                                          stringUncapitalize m |>
                                            rel dir::buildDirRoot |> Path.reach_from dir::buildDir
                                        )
                                      ) |>
                                      String.concat sep::" "
                                  )
                                  outNameNoExtNoDir
                                  (Path.reach_from dir::buildDir path)
                            )
                          )
                      }
                    )
                  );
            let cmos =
              List.map
                /* This needs to be sorted */
                sortedPaths
                f::(
                  fun path => {
                    let outNameNoExtNoDir = libName ^ "__" ^ fileNameNoExtNoDir path;
                    rel dir::buildDir (outNameNoExtNoDir ^ ".cmo")
                  }
                );
            let cmaPath = rel dir::buildDir "lib.cma";
            let cmaCompileRulesScheme =
              if isTopLevelLib {
                Scheme.dep @@
                  sortTransitiveThirdParties
                    topLibName::libName nodeModulesRoot::nodeModulesRoot buildDirRoot::buildDirRoot
                    *>>| (
                    fun thirdPartyTransitiveModules => {
                      let transitiveCmaPaths =
                        List.map
                          thirdPartyTransitiveModules
                          f::(
                            fun t =>
                              rel dir::(rel dir::buildDirRoot (String.uncapitalize t)) "lib.cma"
                          );
                      Scheme.rules [
                        Rule.simple
                          targets::[rel dir::buildDir "lib.cma"]
                          deps::(
                            /* TODO: what about cmi and cmt? */
                            [Dep.path moduleAliasCmoPath] @
                              List.map cmos f::Dep.path @ List.map transitiveCmaPaths f::Dep.path
                          )
                          action::(
                            bashf
                              dir::buildDir
                              "ocamlc -g -I `ocamlfind query js_of_ocaml` `ocamlfind query js_of_ocaml`/js_of_ocaml.cma -open %s -a -o %s %s %s %s"
                              /* "-I /Users/chenglou/.opam/4.02.3/lib/js_of_ocaml /Users/chenglou/.opam/4.02.3/lib/js_of_ocaml/js_of_ocaml.cma" */
                              (String.capitalize libName)
                              (Path.basename cmaPath)
                              (
                                taplp 1 transitiveCmaPaths |>
                                  List.map f::(Path.reach_from dir::buildDir) |>
                                  String.concat sep::" "
                              )
                              (Path.basename moduleAliasCmoPath)
                              (List.map cmos f::Path.basename |> String.concat sep::" ")
                          )
                      ]
                    }
                  )
              } else {
                Scheme.rules [
                  Rule.simple
                    targets::[rel dir::buildDir "lib.cma"]
                    deps::[Dep.path moduleAliasCmoPath, ...List.map cmos f::Dep.path]
                    action::(
                      bashf
                        dir::buildDir
                        "ocamlc -g -open %s -a -o %s %s %s"
                        (String.capitalize libName)
                        (Path.basename cmaPath)
                        (Path.basename moduleAliasCmoPath)
                        (List.map cmos f::Path.basename |> String.concat sep::" ")
                    )
                ]
              };
            /* exec output and jsoo output */
            let finalOutputRules =
              if isTopLevelLib {
                let topOutputPath = rel dir::buildDir "app.out";
                let topOutputPathJsoo = rel dir::buildDir "app.js";
                [
                  /* ocamlc -g -o _build/hi/entry.out _build/hi/lib.cma _build/hi/hi__main.cmo */
                  Rule.simple
                    targets::[topOutputPath]
                    deps::[
                      Dep.path cmaPath,
                      Dep.path (rel dir::buildDir (topLibName ^ "__Index.cmo"))
                    ]
                    action::(
                      bashf
                        dir::buildDir
                        "ocamlc -g -o %s %s %s"
                        (Path.basename topOutputPath)
                        (Path.basename cmaPath)
                        (topLibName ^ "__Index.cmo")
                    ),
                  Rule.simple
                    targets::[topOutputPathJsoo]
                    deps::[Dep.path topOutputPath]
                    action::(
                      bashf
                        dir::buildDir
                        "js_of_ocaml --source-map --no-inline --debug-info --pretty --linkall %s"
                        (Path.basename topOutputPath)
                    )
                ]
              } else {
                []
              };
            Scheme.all [
              Scheme.rules (moduleAliasContentRules @ moduleAliasCompileRules @ finalOutputRules),
              sourcesCompileRules,
              cmaCompileRulesScheme
            ]
          }
        )
      }
    )
  )
};

let generateDotMerlinScheme
    nodeModulesRoot::nodeModulesRoot
    buildDirRoot::buildDirRoot
    isTopLevelLib::isTopLevelLib
    libName::libName
    dir::dir
    root::root => {
  ignore nodeModulesRoot;
  ignore buildDirRoot;
  ignore isTopLevelLib;
  ignore libName;
  ignore dir;
  ignore root;
  let dotMerlinContent =
    Printf.sprintf
      {|%s
S %s

B %s

PKG js_of_ocaml

FLG -w -30 -w -40 -open %s
|}
      (isTopLevelLib ? "S src" : "")
      (Path.reach_from dir::dir (rel dir::nodeModulesRoot "**/src"))
      (Path.reach_from dir::dir (rel dir::buildDirRoot "*"))
      (String.capitalize libName);
  Scheme.rules [
    Rule.simple
      targets::[rel dir::dir ".merlin"]
      deps::[]
      action::(Action.save dotMerlinContent target::(rel dir::dir ".merlin"))
  ]
};

let scheme dir::dir => {
  ignore dir;
  /* print_endline @@ (ts dir ^ "<<<<<<<<<<<<<<<"); */
  /* all the third party .merlin files, generated into their own node_modules/bla dir. We're making an
     exception here to generate artifact into somewhere else than _build/. Maybe one day merlin will
     support our dir structure */
  if (dir == root) {
    let dotMerlinScheme = Scheme.rules_dep (
      getThirdPartyDepsForLib srcDir::topSrcDir *>>| (
        fun deps => {
          let thirdPartyNodeModulesRoots =
            List.map deps f::(fun dep => rel dir::nodeModulesRoot (String.uncapitalize dep));
          List.map
            thirdPartyNodeModulesRoots
            f::(fun path => Rule.default dir::dir [Dep.path (rel dir::path ".merlin")])
        }
      )
    );
    Scheme.all [
      generateDotMerlinScheme
        buildDirRoot::buildDirRoot
        isTopLevelLib::true
        nodeModulesRoot::nodeModulesRoot
        dir::dir
        root::root
        libName::topLibName,
      Scheme.rules [
        Rule.default
          dir::dir
          [
            Dep.path (rel dir::(rel dir::buildDirRoot topLibName) "app.out"),
            Dep.path (rel dir::(rel dir::buildDirRoot topLibName) "app.js"),
            Dep.path (rel dir::root ".merlin")
          ]
      ],
      dotMerlinScheme
    ]
  } else if (
    Path.is_descendant dir::buildDirRoot dir
  ) {
    let libName = Path.basename dir;
    let srcDir =
      if (libName == topLibName) {
        topSrcDir
      } else {
        rel dir::(rel dir::nodeModulesRoot libName) "src"
      };
    compileLibScheme
      srcDir::srcDir
      isTopLevelLib::(libName == topLibName)
      libName::libName
      buildDir::(rel dir::buildDirRoot libName)
      buildDirRoot::buildDirRoot
      nodeModulesRoot::nodeModulesRoot
  } else if (
    Path.dirname dir == nodeModulesRoot
  ) {
    let libName = Path.basename dir;
    generateDotMerlinScheme
      buildDirRoot::buildDirRoot
      isTopLevelLib::false
      nodeModulesRoot::nodeModulesRoot
      dir::dir
      root::root
      libName::libName
  } else {
    Scheme.no_rules
  }
};

let env = Env.create
  /* TODO: this doesn't traverse down to _build so I can't ask it to clean files there? */
  /* artifacts::(
       fun dir::dir => {
         print_endline @@ (ts dir ^ "00000000000000000");
         /* if (dir == buildDir || Path.is_descendant dir::dir buildDir) {
           Dep.glob_listing (Glob.create dir::buildDir "*.cmi")
         } else { */
           Dep.return []
         /* } */
       }
     ) */
  scheme;

let setup () => Deferred.return env;
