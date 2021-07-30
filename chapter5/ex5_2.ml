(* 時間tを受け取って、午前か午後かを返す *)
(* jikan: int -> string *)
let jikan t =
  if (t mod 24) < 12 then "am"
                   else "pm";;

let test0 = jikan 0 = "am";;
let test1 = jikan 12 = "pm";;
let test2 = jikan 24 = "am";;
let test3 = jikan 56 = "am";;
