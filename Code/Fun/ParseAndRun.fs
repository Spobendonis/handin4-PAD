(* File Fun/ParseAndRun.fs *)

module ParseAndRun

let fromString = Parse.fromString;;

let eval = Fun.eval;;

let run e = eval e [];;

let prog1 = "let x = 7 in let y = 13 in x + y end end";;
let prog2 = "let number = 16 in if 4 < number then number else 4 + number end";;
let prog3 = "let earth = 10 in let wind = 45 in let and fire = fire + earth * wind in and 15 end end end";;

prog1 |> fromString |> run;; // Returns 20
prog2 |> fromString |> run;; // Returns 16
prog3 |> fromString |> run;; // Returns 465

let sum1000 = "let sum x = if x = 0 then 0 else x + sum (x-1) in sum 1000 end";;
let pow3to8 = "let pow3 x = if x = 0 then 1 else 3 * pow3 (x-1) in pow3 8 end"
let powwow = "let pow3 x = if x = 0 then 1 else 3 * pow3 (x-1) in let powsum x = if x = 0 then pow3 0 else pow3 x + powsum (x-1) in powsum 11 end end"
let powwowow = "let topow8 x = x * x * x * x * x * x * x * x in let powsum x = if x = 0 then 0 else topow8 x + powsum (x-1) in powsum 10 end end";;

sum1000 |> fromString |> run;; // Returns 500500
pow3to8 |> fromString |> run;; // Returns 6561
powwow |> fromString |> run;; // Returns 265720
powwowow |> fromString |> run;; // Returns 167731333

