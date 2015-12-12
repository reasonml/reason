let projection_files =
  Deferred.List.map x ~f:(fun p ->
    ())
  >>| String.split ~on:'\n'
