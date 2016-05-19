open Core.Std
open Async.Std
open Jenga_lib.Api
let mapD = Dep.map
let bindD = Dep.bind
let rel = Path.relative
let ts = Path.to_string
let root = Path.the_root
let bash ~dir  command =
  Action.process ~dir ~prog:"bash" ~args:["-c"; command] ()
let bashf ~dir  fmt = ksprintf (fun str  -> bash ~dir str) fmt
let nonBlank s = match String.strip s with | "" -> false | _ -> true
let cap = String.capitalize
let uncap = String.uncapitalize
let relD ~dir  str = Dep.path (rel ~dir str)
let chopSuffixExn str = String.slice str 0 (String.rindex_exn str '.')
let fileNameNoExtNoDir path = (Path.basename path) |> chopSuffixExn
let topLibName = "top"
let finalOutputName = "app"
let libraryFileName = "lib.cma"
let nodeModulesRoot = rel ~dir:root "node_modules"
let buildDirRoot = rel ~dir:root "_build"
let topSrcDir = rel ~dir:root "src"
let ocamlDep ~sourcePath  =
  let srcDir = Path.dirname sourcePath in
  let action =
    Dep.action_stdout
      (mapD (Dep.path sourcePath)
         (fun ()  ->
            bashf ~dir:srcDir
              "ocamldep -pp refmt -modules -one-line -impl %s"
              (Path.basename sourcePath))) in
  let processRawString string =
    match (String.strip string) |> (String.split ~on:':') with
    | original::deps::[] ->
        (original,
          ((String.split deps ~on:' ') |> (List.filter ~f:nonBlank)))
    | _ -> failwith "expected exactly one ':' in ocamldep output line" in
  mapD action processRawString
let ocamlDepCurrentSources ~sourcePath  =
  let srcDir = Path.dirname sourcePath in
  bindD (ocamlDep ~sourcePath)
    (fun (original,deps)  ->
       mapD (Dep.glob_listing (Glob.create ~dir:srcDir "*.re"))
         (fun sourcePaths  ->
            let sourceModules = List.map sourcePaths ~f:fileNameNoExtNoDir in
            (List.filter deps ~f:(fun m  -> m <> (chopSuffixExn original)))
              |>
              (List.filter
                 ~f:(fun m  ->
                       List.exists sourceModules ~f:(fun m'  -> m = m')))))
let ocamlDepIncludingThirdParty ~sourcePath  =
  let srcDir = Path.dirname sourcePath in
  bindD (ocamlDep ~sourcePath)
    (fun (original,deps)  ->
       mapD
         (Dep.both (Dep.subdirs ~dir:nodeModulesRoot)
            (Dep.glob_listing (Glob.create ~dir:srcDir "*.re")))
         (fun (thirdPartyRoots,sourcePaths)  ->
            let sourceModules = List.map sourcePaths ~f:fileNameNoExtNoDir in
            let thirdPartyModules = "Js" ::
              (List.map thirdPartyRoots
                 ~f:(fun r  -> (Path.basename r) |> String.capitalize)) in
            (List.filter deps ~f:(fun m  -> m <> (chopSuffixExn original)))
              |>
              (List.filter
                 ~f:(fun m  ->
                       (List.exists sourceModules ~f:(fun m'  -> m = m')) ||
                         (List.exists thirdPartyModules
                            ~f:(fun m'  -> m = m'))))))
let getThirdPartyDepsForLib ~srcDir  =
  let getThirdPartyDepsForLib' sourcePaths =
    mapD
      (Dep.all
         (List.map sourcePaths
            ~f:(fun sourcePath  -> ocamlDepIncludingThirdParty ~sourcePath)))
      (fun sourcePathsDeps  ->
         let internalDeps = List.map sourcePaths ~f:fileNameNoExtNoDir in
         ((List.concat sourcePathsDeps) |> List.dedup) |>
           (List.filter
              ~f:(fun dep  ->
                    List.for_all internalDeps ~f:(fun dep'  -> dep <> dep')))) in
  bindD (Dep.glob_listing (Glob.create ~dir:srcDir "*.re"))
    getThirdPartyDepsForLib'
let topologicalSort graph =
  let graph = { contents = graph } in
  let rec topologicalSort' currNode accum =
    let nodeDeps =
      match List.Assoc.find graph.contents currNode with
      | None  -> []
      | ((Some (nodeDeps'))) -> nodeDeps' in
    List.iter nodeDeps ~f:(fun dep  -> topologicalSort' dep accum);
    if List.for_all accum.contents ~f:(fun n  -> n <> currNode)
    then
      (accum := (currNode :: (accum.contents));
       graph := (List.Assoc.remove graph.contents currNode)) in
  let accum = { contents = [] } in
  while not (List.is_empty graph.contents) do
    topologicalSort' (fst (List.hd_exn graph.contents)) accum done;
  List.rev accum.contents
let sortTransitiveThirdParties =
  bindD (Dep.subdirs ~dir:nodeModulesRoot)
    (fun thirdPartyRoots  ->
       let thirdPartySrcDirs =
         List.map thirdPartyRoots ~f:(fun r  -> rel ~dir:r "src") in
       let thirdPartiesThirdPartyDepsD =
         Dep.all
           (List.map thirdPartySrcDirs
              ~f:(fun srcDir  -> getThirdPartyDepsForLib ~srcDir)) in
       mapD thirdPartiesThirdPartyDepsD
         (fun thirdPartiesThirdPartyDeps  ->
            (List.zip_exn
               (List.map thirdPartyRoots
                  ~f:(fun a  -> (Path.basename a) |> cap))
               thirdPartiesThirdPartyDeps)
              |> topologicalSort))
let sortPathsTopologically ~dir  ~paths  =
  let pathsAsModules =
    List.map paths ~f:(fun path  -> fileNameNoExtNoDir path) in
  let depsForPathsD =
    Dep.all
      (List.map paths
         ~f:(fun path  -> ocamlDepCurrentSources ~sourcePath:path)) in
  mapD depsForPathsD
    (fun depsForPaths  ->
       ((List.zip_exn pathsAsModules depsForPaths) |> topologicalSort) |>
         (List.map ~f:(fun m  -> rel ~dir (m ^ ".re"))))
let moduleAliasFileScheme ~buildDir  ~sourceModules  ~libName  =
  let name extension = rel ~dir:buildDir (libName ^ ("." ^ extension)) in
  let sourcePath = name "re" in
  let cmo = name "cmo" in
  let cmi = name "cmi" in
  let cmt = name "cmt" in
  let fileContent =
    (List.map sourceModules
       ~f:(fun moduleName  ->
             Printf.sprintf "let module %s = %s__%s;\n" moduleName
               (cap libName) moduleName))
      |> (String.concat ~sep:"") in
  let action =
    bashf ~dir:buildDir
      "ocamlc -pp refmt -bin-annot -g -no-alias-deps -w -49 -w -30 -w -40 -c -impl %s -o %s"
      (Path.basename sourcePath) (Path.basename cmo) in
  let compileRule =
    Rule.create ~targets:[cmo; cmi; cmt]
      (mapD (Dep.path sourcePath) (fun ()  -> action)) in
  let contentRule =
    Rule.create ~targets:[sourcePath]
      (Dep.return (Action.save fileContent ~target:sourcePath)) in
  Scheme.rules [contentRule; compileRule]
let jsooLocationD =
  mapD
    (Dep.action_stdout
       (Dep.return (bash ~dir:root "ocamlfind query js_of_ocaml")))
    String.strip
let compileSourcesScheme ~buildDir  ~libName  ~sourcePaths  =
  let compileSourcesScheme' jsooLocation =
    let moduleAliasDep extension =
      relD ~dir:buildDir (libName ^ ("." ^ extension)) in
    let compileEachSourcePath path =
      mapD (ocamlDepIncludingThirdParty ~sourcePath:path)
        (fun modules  ->
           let thirdPartyModules =
             List.filter modules
               ~f:(fun m  ->
                     List.for_all sourcePaths
                       ~f:(fun path  -> (fileNameNoExtNoDir path) <> m)) in
           let firstPartyModules =
             List.filter modules
               ~f:(fun m  ->
                     List.exists sourcePaths
                       ~f:(fun path  -> (fileNameNoExtNoDir path) = m)) in
           let firstPartyModuleDeps =
             List.map firstPartyModules
               ~f:(fun m  ->
                     relD ~dir:buildDir (libName ^ ("__" ^ (m ^ ".cmi")))) in
           let outNameNoExtNoDir =
             libName ^ ("__" ^ (fileNameNoExtNoDir path)) in
           let thirdPartiesCmisDep =
             Dep.all_unit
               (List.map thirdPartyModules
                  ~f:(fun m  ->
                        let libName = uncap m in
                        bindD
                          (Dep.glob_listing
                             (Glob.create
                                ~dir:(rel
                                        ~dir:(rel ~dir:nodeModulesRoot
                                                libName) "src") "*.re"))
                          (fun thirdPartySources  ->
                             Dep.all_unit
                               (List.map thirdPartySources
                                  ~f:(fun sourcePath  ->
                                        relD
                                          ~dir:(rel ~dir:buildDirRoot libName)
                                          (libName ^
                                             ("__" ^
                                                ((fileNameNoExtNoDir
                                                    sourcePath)
                                                   ^ ".cmi")))))))) in
           let cmi = rel ~dir:buildDir (outNameNoExtNoDir ^ ".cmi") in
           let cmo = rel ~dir:buildDir (outNameNoExtNoDir ^ ".cmo") in
           let cmt = rel ~dir:buildDir (outNameNoExtNoDir ^ ".cmt") in
           let deps =
             Dep.all_unit ((Dep.path path) :: (moduleAliasDep "cmi") ::
               (moduleAliasDep "cmo") :: (moduleAliasDep "cmt") ::
               (moduleAliasDep "re") :: thirdPartiesCmisDep ::
               firstPartyModuleDeps) in
           let jsooIncludeString =
             match List.exists thirdPartyModules ~f:(fun m  -> m = "Js") with
             | true  ->
                 Printf.sprintf "-I %s %s/js_of_ocaml.cma" jsooLocation
                   jsooLocation
             | false  -> "" in
           let action =
             bashf ~dir:buildDir
               "ocamlc -pp refmt -bin-annot -g -w -30 -w -40 -open %s %s -I %s %s -o %s -intf-suffix rei -c -impl %s"
               (cap libName) jsooIncludeString (ts buildDir)
               ((List.map thirdPartyModules
                   ~f:(fun m  ->
                         "-I " ^
                           (((uncap m) |> (rel ~dir:buildDirRoot)) |>
                              (Path.reach_from ~dir:buildDir))))
                  |> (String.concat ~sep:" ")) outNameNoExtNoDir
               (Path.reach_from ~dir:buildDir path) in
           Rule.create ~targets:[cmi; cmo; cmt]
             (mapD deps (fun ()  -> action))) in
    Scheme.rules_dep
      (Dep.all (List.map sourcePaths ~f:compileEachSourcePath)) in
  Scheme.dep (mapD jsooLocationD compileSourcesScheme')
let compileCmaScheme ~sortedSourcePaths  ~libName  ~buildDir  =
  let cmaPath = rel ~dir:buildDir libraryFileName in
  let moduleAliasCmoPath = rel ~dir:buildDir (libName ^ ".cmo") in
  let cmos =
    List.map sortedSourcePaths
      ~f:(fun path  ->
            rel ~dir:buildDir
              (libName ^ ("__" ^ ((fileNameNoExtNoDir path) ^ ".cmo")))) in
  let cmosString =
    (List.map cmos ~f:Path.basename) |> (String.concat ~sep:" ") in
  Scheme.rules
    [Rule.simple ~targets:[cmaPath]
       ~deps:(List.map (moduleAliasCmoPath :: cmos) ~f:Dep.path)
       ~action:(bashf ~dir:buildDir "ocamlc -g -open %s -a -o %s %s %s"
                  (cap libName) (Path.basename cmaPath)
                  (Path.basename moduleAliasCmoPath) cmosString)]
let finalOutputsScheme ~sortedSourcePaths  =
  let buildDir = rel ~dir:buildDirRoot topLibName in
  let binaryPath = rel ~dir:buildDir (finalOutputName ^ ".out") in
  let jsooPath = rel ~dir:buildDir (finalOutputName ^ ".js") in
  let moduleAliasCmoPath = rel ~dir:buildDir (topLibName ^ ".cmo") in
  let cmos =
    List.map sortedSourcePaths
      ~f:(fun path  ->
            rel ~dir:buildDir
              (topLibName ^ ("__" ^ ((fileNameNoExtNoDir path) ^ ".cmo")))) in
  let cmosString =
    (List.map cmos ~f:Path.basename) |> (String.concat ~sep:" ") in
  Scheme.dep
    (mapD (Dep.both jsooLocationD sortTransitiveThirdParties)
       (fun (jsooLocation,thirdPartyTransitiveDeps)  ->
          let transitiveCmaPaths =
            List.map thirdPartyTransitiveDeps
              ~f:(fun dep  ->
                    rel ~dir:(rel ~dir:buildDirRoot (uncap dep))
                      libraryFileName) in
          let action =
            bashf ~dir:buildDir
              "ocamlc -g -I %s %s/js_of_ocaml.cma -open %s -o %s %s %s %s"
              jsooLocation jsooLocation (cap topLibName)
              (Path.basename binaryPath)
              ((transitiveCmaPaths |>
                  (List.map ~f:(Path.reach_from ~dir:buildDir)))
                 |> (String.concat ~sep:" "))
              (Path.basename moduleAliasCmoPath) cmosString in
          Scheme.rules
            [Rule.simple ~targets:[binaryPath]
               ~deps:(([moduleAliasCmoPath] @ (cmos @ transitiveCmaPaths)) |>
                        (List.map ~f:Dep.path)) ~action;
            Rule.simple ~targets:[jsooPath] ~deps:[Dep.path binaryPath]
              ~action:(bashf ~dir:buildDir
                         "js_of_ocaml --source-map --no-inline --debug-info --pretty --linkall %s"
                         (Path.basename binaryPath))]))
let compileLibScheme ?(isTopLevelLib= true)  ~srcDir  ~libName  ~buildDir  =
  Scheme.dep
    (bindD (Dep.glob_listing (Glob.create ~dir:srcDir "*.re"))
       (fun unsortedPaths  ->
          mapD (sortPathsTopologically ~dir:srcDir ~paths:unsortedPaths)
            (fun sortedPaths  ->
               Scheme.all
                 [moduleAliasFileScheme ~buildDir ~libName
                    ~sourceModules:(List.map unsortedPaths
                                      ~f:fileNameNoExtNoDir);
                 compileSourcesScheme ~buildDir ~libName
                   ~sourcePaths:unsortedPaths;
                 (match isTopLevelLib with
                  | true  ->
                      finalOutputsScheme ~sortedSourcePaths:sortedPaths
                  | false  ->
                      compileCmaScheme ~buildDir ~libName
                        ~sortedSourcePaths:sortedPaths)])))
let dotMerlinScheme ~isTopLevelLib  ~libName  ~dir  =
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
      (match isTopLevelLib with | true  -> "S src" | false  -> "")
      (Path.reach_from ~dir (rel ~dir:nodeModulesRoot "**/src"))
      (Path.reach_from ~dir (rel ~dir:buildDirRoot "*")) (cap libName) in
  let dotMerlinPath = rel ~dir ".merlin" in
  Scheme.rules
    [Rule.simple ~targets:[dotMerlinPath] ~deps:[]
       ~action:(Action.save dotMerlinContent ~target:dotMerlinPath)]
let scheme ~dir  =
  ignore dir;
  if dir = root
  then
    (let dotMerlinDefaultScheme =
       Scheme.rules_dep
         (mapD (Dep.subdirs ~dir:nodeModulesRoot)
            (fun thirdPartyRoots  ->
               List.map thirdPartyRoots
                 ~f:(fun path  ->
                       Rule.default ~dir [relD ~dir:path ".merlin"]))) in
     Scheme.all
       [dotMerlinScheme ~isTopLevelLib:true ~dir ~libName:topLibName;
       Scheme.rules
         [Rule.default ~dir
            [relD ~dir:(rel ~dir:buildDirRoot topLibName)
               (finalOutputName ^ ".out");
            relD ~dir:(rel ~dir:buildDirRoot topLibName)
              (finalOutputName ^ ".js");
            relD ~dir:root ".merlin"]];
       dotMerlinDefaultScheme])
  else
    if Path.is_descendant ~dir:buildDirRoot dir
    then
      (let libName = Path.basename dir in
       let srcDir =
         match libName = topLibName with
         | true  -> topSrcDir
         | false  -> rel ~dir:(rel ~dir:nodeModulesRoot libName) "src" in
       compileLibScheme ~srcDir ~isTopLevelLib:(libName = topLibName)
         ~libName ~buildDir:(rel ~dir:buildDirRoot libName))
    else
      if (Path.dirname dir) = nodeModulesRoot
      then
        (let libName = Path.basename dir in
         dotMerlinScheme ~isTopLevelLib:false ~dir ~libName)
      else Scheme.no_rules
let env = Env.create scheme
let setup () = Deferred.return env
