(* 身長h, 体重w 受け取って bmiを返す *)
let bmi h w = w /. h ** 2.;;

(* 身長h, 体重w 受け取って bmiを返す *)
let taikei h w = if bmi h w < 18.5 then "slim"
                 else if 18.5 <= bmi h w && bmi h w < 25. then "normal"
                 else if 25. <= bmi h w && bmi h w < 30. then "fat"
                 else "super fat";;

let test0 = taikei 1.70 50. = "slim";;
let test1 = taikei 1.70 62. = "normal";;
let test2 = taikei 1.70 80. = "fat";;
let test3 = taikei 1.70 200. = "super fat";;
