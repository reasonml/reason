type predicate =
    Pred_Byte
  | Pred_Native
  | Pred_Toploop

let _ =
  { pkg with
      package_version = projFile.version
    ; package_description = false
    ; package_requires = [] }
