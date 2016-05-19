/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Core.Std;

open Async.Std;

open Jenga_lib.Api;

/* general helpers */
let mapD = Dep.map;

let bindD = Dep.bind;

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

let cap = String.capitalize;

let uncap = String.uncapitalize;

let relD dir::dir str => Dep.path (rel dir::dir str);

/* assumes there is a suffix to chop. Throws otherwise */
let chopSuffixExn str => String.slice str 0 (String.rindex_exn str '.');

let fileNameNoExtNoDir path => Path.basename path |> chopSuffixExn;

/* this jengaroot-specific helpers */
let topLibName = "top";

let finalOutputName = "app";

let libraryFileName = "lib.cma";

let nodeModulesRoot = rel dir::root "node_modules";

let buildDirRoot = rel dir::root "_build";

let topSrcDir = rel dir::root "src";

/* Wrapper for the CLI `ocamldep`. Take the output, process it a bit, and pretend we've just called a regular
   ocamldep OCaml function. Note: the `ocamldep` utility doesn't give us enough info for fine, accurate */
let ocamlDep sourcePath::sourcePath => {
  let srcDir = Path.dirname sourcePath;
  let action = Dep.action_stdout (
    mapD
      (Dep.path sourcePath)
      (
        fun () =>
          bashf
            dir::srcDir "ocamldep -pp refmt -modules -one-line -impl %s" (Path.basename sourcePath)
      )
  );
  let processRawString string =>
    switch (String.strip string |> String.split on::':') {
    | [original, deps] => (original, String.split deps on::' ' |> List.filter f::nonBlank)
    | _ => failwith "expected exactly one ':' in ocamldep output line"
    };
  mapD action processRawString
};

/* Get only the dependencies on sources in the current library. */
let ocamlDepCurrentSources sourcePath::sourcePath => {
  let srcDir = Path.dirname sourcePath;
  bindD
    (ocamlDep sourcePath::sourcePath)
    (
      fun (original, deps) =>
        mapD
          (Dep.glob_listing (Glob.create dir::srcDir "*.re"))
          (
            fun sourcePaths => {
              let sourceModules = List.map sourcePaths f::fileNameNoExtNoDir;
              /* If the current file's Foo.re, and it depend on Foo, then it's certainly not depending on
                 itself, which means that Foo either comes from a third-party module (which we can ignore
                 here), or is a nested module from an `open`ed module, which ocamldep would have detected and
                 returned in this list. */
              List.filter deps f::(fun m => m != chopSuffixExn original) |>
                List.filter f::(fun m => List.exists sourceModules f::(fun m' => m == m'))
            }
          )
    )
};

/* Like above, but include third-party deps too. */
let ocamlDepIncludingThirdParty sourcePath::sourcePath => {
  let srcDir = Path.dirname sourcePath;
  bindD
    (ocamlDep sourcePath::sourcePath)
    (
      fun (original, deps) =>
        mapD
          (
            Dep.both
              (Dep.subdirs dir::nodeModulesRoot)
              (Dep.glob_listing (Glob.create dir::srcDir "*.re"))
          )
          (
            fun (thirdPartyRoots, sourcePaths) => {
              let sourceModules = List.map sourcePaths f::fileNameNoExtNoDir;
              let thirdPartyModules = [
                /* Special-case js_of_ocaml as a magical global. */
                "Js",
                ...List.map thirdPartyRoots f::(fun r => Path.basename r |> String.capitalize)
              ];
              /* See comment in `ocamlDepCurrentSources` for this first filter. */
              List.filter deps f::(fun m => m != chopSuffixExn original) |>
                List.filter
                  f::(
                    fun m =>
                      List.exists sourceModules f::(fun m' => m == m') ||
                        List.exists thirdPartyModules f::(fun m' => m == m')
                  )
            }
          )
    )
};

/* Basically (ocamlDepIncludingThirdParty - ocamlDepCurrentSources) for all source files in the library in
   question */
let getThirdPartyDepsForLib srcDir::srcDir => {
  let getThirdPartyDepsForLib' sourcePaths =>
    mapD
      (
        Dep.all (
          List.map
            sourcePaths f::(fun sourcePath => ocamlDepIncludingThirdParty sourcePath::sourcePath)
        )
      )
      (
        fun sourcePathsDeps => {
          let internalDeps = List.map sourcePaths f::fileNameNoExtNoDir;
          List.concat sourcePathsDeps |>
            List.dedup |>
            List.filter f::(fun dep => List.for_all internalDeps f::(fun dep' => dep != dep'))
        }
      );
  bindD (Dep.glob_listing (Glob.create dir::srcDir "*.re")) getThirdPartyDepsForLib'
};

/* Generic sorting algorithm on directed acyclic graph. Example: [(a, [b, c, d]), (b, [c]), (d, [c])] will be
   sorted into [c, d, b, a] or [c, b, d, a], aka the ones being depended on will always come before the
   dependent */
let topologicalSort graph => {
  let graph = {contents: graph};
  let rec topologicalSort' currNode accum => {
    let nodeDeps =
      switch (List.Assoc.find graph.contents currNode) {
      /* node not found: presume to be third-party dep. This is slightly dangerous because it might also mean
         we didn't construct the graph correctly. */
      | None => []
      | Some nodeDeps' => nodeDeps'
      };
    List.iter nodeDeps f::(fun dep => topologicalSort' dep accum);
    if (List.for_all accum.contents f::(fun n => n != currNode)) {
      accum := [currNode, ...accum.contents];
      graph := List.Assoc.remove graph.contents currNode
    }
  };
  let accum = {contents: []};
  while (not (List.is_empty graph.contents)) {
    topologicalSort' (fst (List.hd_exn graph.contents)) accum
  };
  List.rev accum.contents
};

/* Figure out the order in which third-party libs should be compiled, based on their dependencies (the
   depended is compiled before the dependent). */
let sortTransitiveThirdParties =
  bindD
    (Dep.subdirs dir::nodeModulesRoot)
    (
      fun thirdPartyRoots => {
        let thirdPartySrcDirs = List.map thirdPartyRoots f::(fun r => rel dir::r "src");
        let thirdPartiesThirdPartyDepsD = Dep.all (
          List.map thirdPartySrcDirs f::(fun srcDir => getThirdPartyDepsForLib srcDir::srcDir)
        );
        mapD
          thirdPartiesThirdPartyDepsD
          (
            fun thirdPartiesThirdPartyDeps =>
              List.zip_exn
                (List.map thirdPartyRoots f::(fun a => Path.basename a |> cap))
                thirdPartiesThirdPartyDeps |> topologicalSort
          )
      }
    );

let sortPathsTopologically dir::dir paths::paths => {
  let pathsAsModules = List.map paths f::(fun path => fileNameNoExtNoDir path);
  let depsForPathsD = Dep.all (
    List.map paths f::(fun path => ocamlDepCurrentSources sourcePath::path)
  );
  mapD
    depsForPathsD
    (
      fun depsForPaths =>
        List.zip_exn pathsAsModules depsForPaths |>
          topologicalSort |> List.map f::(fun m => rel dir::dir (m ^ ".re"))
    )
};

/* the module alias file takes the current library foo's first-party sources, e.g. A.re, B.re, and turn them
   into a foo.re file whose content is:
   let module A = Foo__A;
   let module B = Foo__B;
   */
/* We'll then compile this file into foo.cmi/cmo/cmt, and have it opened by default when compiling A.re and
   B.re (into foo_A and foo_B respectively) later. The effect is that, inside A.re, we can refer to B instead
   of Foo__B thanks to the pre-opened foo.re. But when these files are used by other libraries (which aren't
   compiled with foo.re pre-opened of course), they won't see module A or B, only Foo__A and Foo__B, aka in
   practice, they simply won't see them. This effectively means we've implemented namespacing! */
let moduleAliasFileScheme buildDir::buildDir sourceModules::sourceModules libName::libName => {
  let name extension => rel dir::buildDir (libName ^ "." ^ extension);
  let sourcePath = name "re";
  let cmo = name "cmo";
  let cmi = name "cmi";
  let cmt = name "cmt";
  let fileContent =
    List.map
      sourceModules
      f::(
        fun moduleName =>
          Printf.sprintf "let module %s = %s__%s;\n" moduleName (cap libName) moduleName
      ) |>
      String.concat sep::"";
  let action =
    bashf
      dir::buildDir
      /* We suppress a few warnings here through -w.
         - 49: Absent cmi file when looking up module alias. Aka Foo__A and Foo__B's compiled cmis
         can't be found at the moment this module alias file is compiled. This is normal, since the
         module alias file is the first thing that's compiled (so that we can open it during
         compilation of A.re and B.re into Foo__A and Foo__B). Think of this as forward declaration.

         - 30: Two labels or constructors of the same name are defined in two mutually recursive
         types. I forgot...

         - 40: Constructor or label name used out of scope. I forgot too. Great comment huh?

         More flags:
         -pp refmt option makes ocamlc take our reason syntax source code and pass it through our
         refmt parser first, before operating on the AST.

         -bin-annot: generates cmt files that contains info such as types and bindings, for use with
         Merlin.

         -g: add debugging info. You don't really ever compile without this flag.

         -impl: source file. This flag's needed if the source extension isn't ml. I think.

         -o: output name
         */
      "ocamlc -pp refmt -bin-annot -g -no-alias-deps -w -49 -w -30 -w -40 -c -impl %s -o %s"
      (Path.basename sourcePath)
      (Path.basename cmo);
  let compileRule =
    Rule.create targets::[cmo, cmi, cmt] (mapD (Dep.path sourcePath) (fun () => action));
  let contentRule =
    Rule.create targets::[sourcePath] (Dep.return (Action.save fileContent target::sourcePath));
  Scheme.rules [contentRule, compileRule]
};

let jsooLocationD =
  mapD (Dep.action_stdout (Dep.return (bash dir::root "ocamlfind query js_of_ocaml"))) String.strip;

/* We compile each file in the current library (say, foo). If a file's Bar.re, it'll be compiled to
   foo__Bar.{cmi, cmo, cmt}. As to why we're namespacing compiled outputs like this, see
   `moduleAliasFileScheme`. */
let compileSourcesScheme buildDir::buildDir libName::libName sourcePaths::sourcePaths => {
  let compileSourcesScheme' jsooLocation => {
    /* This is the module alias file generated through `moduleAliasFileScheme`, that we said we're gonna `-open`
       during `ocamlc` */
    let moduleAliasDep extension => relD dir::buildDir (libName ^ "." ^ extension);
    let compileEachSourcePath path =>
      mapD
        (ocamlDepIncludingThirdParty sourcePath::path)
        (
          fun modules => {
            let thirdPartyModules =
              List.filter
                modules
                f::(
                  fun m => List.for_all sourcePaths f::(fun path => fileNameNoExtNoDir path != m)
                );
            let firstPartyModules =
              List.filter
                modules
                f::(fun m => List.exists sourcePaths f::(fun path => fileNameNoExtNoDir path == m));
            let firstPartyModuleDeps =
              List.map
                firstPartyModules
                /* compiling here only needs cmi */
                f::(fun m => relD dir::buildDir (libName ^ "__" ^ m ^ ".cmi"));
            let outNameNoExtNoDir = libName ^ "__" ^ fileNameNoExtNoDir path;
            /* Compiling the current source file depends on all of the cmis of all its third-party libraries'
               source files being compiled. This is very coarse since in reality, we only depend on a few source
               files of these third-party libs. But ocamldep isn't granular enough to give us this information
               yet. */
            let thirdPartiesCmisDep = Dep.all_unit (
              List.map
                thirdPartyModules
                f::(
                  fun m => {
                    /* if one of a third party library foo's source is Hi.re, then it resides in
                       `node_modules/foo/src/Hi.re`, and its cmi artifacts in `_build/foo/Foo__Hi.cmi` */
                    let libName = uncap m;
                    bindD
                      (
                        Dep.glob_listing (
                          Glob.create
                            dir::(rel dir::(rel dir::nodeModulesRoot libName) "src") "*.re"
                        )
                      )
                      (
                        fun thirdPartySources => Dep.all_unit (
                          List.map
                            thirdPartySources
                            f::(
                              fun sourcePath =>
                                relD
                                  dir::(rel dir::buildDirRoot libName)
                                  (libName ^ "__" ^ fileNameNoExtNoDir sourcePath ^ ".cmi")
                            )
                        )
                      )
                  }
                )
            );
            let cmi = rel dir::buildDir (outNameNoExtNoDir ^ ".cmi");
            let cmo = rel dir::buildDir (outNameNoExtNoDir ^ ".cmo");
            let cmt = rel dir::buildDir (outNameNoExtNoDir ^ ".cmt");
            let deps = Dep.all_unit [
              Dep.path path,
              moduleAliasDep "cmi",
              moduleAliasDep "cmo",
              moduleAliasDep "cmt",
              moduleAliasDep "re",
              thirdPartiesCmisDep,
              ...firstPartyModuleDeps
            ];
            /* Only include js_of_ocaml in the modules search path if the current source mentions that Js
               module. Might speed up some things? */
            let jsooIncludeString =
              List.exists thirdPartyModules f::(fun m => m == "Js") ?
                Printf.sprintf "-I %s %s/js_of_ocaml.cma" jsooLocation jsooLocation : "";
            let action =
              bashf
                dir::buildDir
                /* Most of the flags here have been explained previously in `moduleAliasFileScheme`.
                   -intf-suffix: tells ocamlc what the interface file's extension is.

                   -c: compile only, don't link yet.
                   */
                /* Example command: ocamlc -pp refmt -bin-annot -g -w -30 -w -40 -open Foo -I \
                   path/to/js_of_ocaml path/to/js_of_ocaml/js_of_ocaml.cma -I ./ -I ../fooDependsOnMe -I \
                   ../fooDependsOnMeToo -o foo__CurrentSourcePath -intf-suffix rei -c -impl \
                   path/to/CurrentSourcePath.re */
                "ocamlc -pp refmt -bin-annot -g -w -30 -w -40 -open %s %s -I %s %s -o %s -intf-suffix rei -c -impl %s"
                (cap libName)
                jsooIncludeString
                (ts buildDir)
                (
                  List.map
                    thirdPartyModules
                    f::(
                      fun m => "-I " ^ (
                        uncap m |> rel dir::buildDirRoot |> Path.reach_from dir::buildDir
                      )
                    ) |>
                    String.concat sep::" "
                )
                outNameNoExtNoDir
                (Path.reach_from dir::buildDir path);
            Rule.create targets::[cmi, cmo, cmt] (mapD deps (fun () => action))
          }
        );
    Scheme.rules_dep (Dep.all (List.map sourcePaths f::compileEachSourcePath))
  };
  Scheme.dep (mapD jsooLocationD compileSourcesScheme')
};

/* This function assumes we're not using it at the top level.
   Cma is a library file for the current library which bundles up all the lib's first-party compiled sources.
   This way, it's much easier, at the end, at the top level, to include each library's cma file to compile the
   final executable, than to tediously pass every single source file from every lib in order.

   There's a caveat though. We said we're only bundling up the current lib's first-party code; We _could_ have
   bundled up its third-party deps' cma too, but then we might get into duplicate artifact problem caused by
   e.g. library A and B both requiring and bundling C. So we can only bundle first-party code, and then, at
   the top, take all the transitive dependencies (libraries) cmas, figure out their relative order, and pass
   them in that order to the ocamlc command (this logic is in `finalOutputsScheme` below). Still tedious, but
   at least we're not passing individual source files in order. */
let compileCmaScheme sortedSourcePaths::sortedSourcePaths libName::libName buildDir::buildDir => {
  let cmaPath = rel dir::buildDir libraryFileName;
  let moduleAliasCmoPath = rel dir::buildDir (libName ^ ".cmo");
  let cmos =
    List.map
      /* To compile one cma file, we need to pass the compiled first-party sources in order to ocamlc */
      sortedSourcePaths
      f::(fun path => rel dir::buildDir (libName ^ "__" ^ fileNameNoExtNoDir path ^ ".cmo"));
  let cmosString = List.map cmos f::Path.basename |> String.concat sep::" ";
  /* Final bundling. Time to get all the transitive dependencies... */
  Scheme.rules [
    Rule.simple
      targets::[cmaPath]
      deps::(List.map [moduleAliasCmoPath, ...cmos] f::Dep.path)
      action::(
        bashf
          dir::buildDir
          /* Flags:
             -open: compile the file as if [file being opened] was opened at the top of the file. In
             our case, we open our module alias file generated with `moduleAliasFileScheme`. See that
             function for more comment.

             -a: flag for building a library.

             -o: output file name.
             */
          /* Example command: ocamlc -g -open Foo -a -o lib.cma foo.cmo aDependsOnMe.cmo a.cmo b.cmo */
          "ocamlc -g -open %s -a -o %s %s %s"
          (cap libName)
          (Path.basename cmaPath)
          (Path.basename moduleAliasCmoPath)
          cmosString
      )
  ]
};

/* This function assumes we're using it only at the top level.
   We'll output an executable, plus a js_of_ocaml JavaScript file. Throughout the compilation of the source
   files, we've already mingled in the correctly jsoo search paths in ocamlc to make this final compilation
   work. */
let finalOutputsScheme sortedSourcePaths::sortedSourcePaths => {
  let buildDir = rel dir::buildDirRoot topLibName;
  let binaryPath = rel dir::buildDir (finalOutputName ^ ".out");
  let jsooPath = rel dir::buildDir (finalOutputName ^ ".js");
  let moduleAliasCmoPath = rel dir::buildDir (topLibName ^ ".cmo");
  let cmos =
    List.map
      /* To compile one cma file, we need to pass the compiled first-party sources in order to ocamlc */
      sortedSourcePaths
      f::(fun path => rel dir::buildDir (topLibName ^ "__" ^ fileNameNoExtNoDir path ^ ".cmo"));
  let cmosString = List.map cmos f::Path.basename |> String.concat sep::" ";
  Scheme.dep (
    mapD
      (Dep.both jsooLocationD sortTransitiveThirdParties)
      (
        fun (jsooLocation, thirdPartyTransitiveDeps) => {
          let transitiveCmaPaths =
            List.map
              thirdPartyTransitiveDeps
              f::(fun dep => rel dir::(rel dir::buildDirRoot (uncap dep)) libraryFileName);
          let action =
            bashf
              dir::buildDir
              /* For ease of coding, we'll blindly include js_of_ocaml in the -I search path here, in case
                 the module invokes some jsoo's Js module-related stuff. */
              /* Example command: ocamlc -g -I path/to/js_of_ocaml path/to/js_of_ocaml/js_of_ocaml.cma \
                 -open Top -o app.out  ../barDependsOnMe/lib.cma ../bar/lib.cma ../baz/lib.cma \
                 top.cmo aDependsOnMe.cmo a.cmo moreFirstPartyCmo.cmo */
              /* Flags:
                 -I: search path(s), when ocamlc looks for modules referenced inside the file.

                 -open: compile the file as if [file being opened] was opened at the top of the file. In
                 our case, we open our module alias file generated with `moduleAliasFileScheme`. See that
                 function for more comment.

                 -o: output file name.
                 */
              "ocamlc -g -I %s %s/js_of_ocaml.cma -open %s -o %s %s %s %s"
              jsooLocation
              jsooLocation
              (cap topLibName)
              (Path.basename binaryPath)
              (
                transitiveCmaPaths |>
                  List.map f::(Path.reach_from dir::buildDir) |> String.concat sep::" "
              )
              (Path.basename moduleAliasCmoPath)
              cmosString;
          Scheme.rules [
            Rule.simple
              targets::[binaryPath]
              deps::(
                /* TODO: I don't think cmis and cmts are being read here, so we don't need to include them. */
                [moduleAliasCmoPath] @ cmos @ transitiveCmaPaths |>
                  List.map f::Dep.path
              )
              action::action,
            Rule.simple
              targets::[jsooPath]
              deps::[Dep.path binaryPath]
              action::(
                bashf
                  dir::buildDir
                  /* I don't know what the --linkall flag does, and does the --pretty flag work? Because the
                     output is still butt ugly. Just kidding I love you guys. */
                  "js_of_ocaml --source-map --no-inline --debug-info --pretty --linkall %s"
                  (Path.basename binaryPath)
              )
          ]
        }
      )
  )
};

/* The function that ties together all the previous steps and compiles a given library, whether it be our top
   level library or a third-party one. */
let compileLibScheme
    isTopLevelLib::isTopLevelLib=true
    srcDir::srcDir
    libName::libName
    buildDir::buildDir => Scheme.dep (
  bindD
    (Dep.glob_listing (Glob.create dir::srcDir "*.re"))
    (
      fun unsortedPaths =>
        mapD
          (sortPathsTopologically dir::srcDir paths::unsortedPaths)
          (
            fun sortedPaths => Scheme.all [
              moduleAliasFileScheme
                buildDir::buildDir
                libName::libName
                sourceModules::(List.map unsortedPaths f::fileNameNoExtNoDir),
              compileSourcesScheme buildDir::buildDir libName::libName sourcePaths::unsortedPaths,
              isTopLevelLib ?
                /* if we're at the final, top level compilation, there's no need to build a cma output (and
                   then generate an executable from it). We can cut straight to generating the executable. See
                   `finalOutputsScheme`. */
                finalOutputsScheme sortedSourcePaths::sortedPaths :
                compileCmaScheme buildDir::buildDir libName::libName sortedSourcePaths::sortedPaths
            ]
          )
    )
);

/* See comment in the `sprintf` */
let dotMerlinScheme isTopLevelLib::isTopLevelLib libName::libName dir::dir => {
  let dotMerlinContent =
    Printf.sprintf
      {|# [merlin](https://github.com/the-lambda-church/merlin) is a static analyser for
# OCaml that provides autocompletion, jump-to-location, recoverable syntax
# errors, type errors detection, etc., that your editor can use. To activate it,
# one usually provides a .merlin file at the root of a project, describing where
# the sources and artifacts are. Since we dictated the project structure, we can
# auto generate .merlin files!

# S is the merlin flag for source files
%s

# Include all the third-party sources too.
S %s

# B stands for build (artifacts). We generate ours into _build

B %s

# PKG lists packages found through ocamlfind (findlib), a utility for finding
# the location of third-party dependencies. For us, all third-party deps reside
# in `node_modules/`. One of the exceptions being js_of_ocaml. So we pass it to
# PKG here and let ocamlfind find its source instead.
PKG js_of_ocaml

# FLG is the set of flags to pass to Merlin, as if it used ocamlc to compile and
# understand our sources. You don't have to understand what these flags are for
# now; but if you're curious, go check the jengaroot.ml that generated this
# .merlin.
FLG -w -30 -w -40 -open %s
|}
      (isTopLevelLib ? "S src" : "")
      (Path.reach_from dir::dir (rel dir::nodeModulesRoot "**/src"))
      (Path.reach_from dir::dir (rel dir::buildDirRoot "*"))
      (cap libName);
  let dotMerlinPath = rel dir::dir ".merlin";
  Scheme.rules [
    Rule.simple
      targets::[dotMerlinPath]
      deps::[]
      action::(Action.save dotMerlinContent target::dotMerlinPath)
  ]
};

let scheme dir::dir => {
  ignore dir;
  /* We generate many .merlin files, one per third-party library (and on at the top). Additionally, this is
     the only case where we generate some artifacts outside of _build/. Most of this is so that Merlin's
     jump-to-location could work correctly when we jump into a third-party source file. As to why exactly we
     generate .merlin with the content that it is, call 1-800-chenglou-plz-help. */
  if (dir == root) {
    let dotMerlinDefaultScheme = Scheme.rules_dep (
      mapD
        (Dep.subdirs dir::nodeModulesRoot)
        (
          fun thirdPartyRoots =>
            List.map
              thirdPartyRoots f::(fun path => Rule.default dir::dir [relD dir::path ".merlin"])
        )
    );
    Scheme.all [
      dotMerlinScheme isTopLevelLib::true dir::dir libName::topLibName,
      Scheme.rules [
        Rule.default
          dir::dir
          [
            relD dir::(rel dir::buildDirRoot topLibName) (finalOutputName ^ ".out"),
            relD dir::(rel dir::buildDirRoot topLibName) (finalOutputName ^ ".js"),
            relD dir::root ".merlin"
          ]
      ],
      dotMerlinDefaultScheme
    ]
  } else if (
    Path.is_descendant dir::buildDirRoot dir
  ) {
    let libName = Path.basename dir;
    let srcDir =
      libName == topLibName ? topSrcDir : rel dir::(rel dir::nodeModulesRoot libName) "src";
    compileLibScheme
      srcDir::srcDir
      isTopLevelLib::(libName == topLibName)
      libName::libName
      buildDir::(rel dir::buildDirRoot libName)
  } else if (
    Path.dirname dir == nodeModulesRoot
  ) {
    let libName = Path.basename dir;
    dotMerlinScheme isTopLevelLib::false dir::dir libName::libName
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
