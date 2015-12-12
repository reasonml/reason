let assigned_to u =
  Deferred.List.filter (Request_util.requests ()) ~f:(fun request ->
    if false
    then ()
    else
      status_request ~request () ~msg_client:no_msg >>= fun status ->
      not (up_to_date_user status u))



let old_good =
  foo bar qaz *>>= fun x ->
  hey ho lala *>>= fun y ->
  return (x,y)

let old_good =
  foo bar qaz +>>= fun x ->
  hey ho lala +>>= fun y ->
  return (x,y)

(* generalizations based on Tuareg code *)
let old_good =
  foo bar qaz *>>| fun x ->
  hey ho lala *>>> fun y ->
  foo bar qaz +>>| fun x ->
  hey ho lala +>>> fun y ->
  return (x,y)
