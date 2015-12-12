let handle_query qs ~msg_client:_ =
  try_with (fun () ->
    if true then
      f >>| fun () ->
      `Done ()
    else
      false
  )
;;

if false then
  ()
else
  assert_branch_has_node branch node >>| fun () ->
  { t with node; floating }
;;
