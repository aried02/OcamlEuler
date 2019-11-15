let rec is_palin (a, len) =
  if len = 1 || len = 0 then true else
  let last_dig = a mod 10 in 
  let length_var = int_of_float (10.**(float(len-1))) in
  let first_dig = (a/length_var) in 
  let new_num = (a mod length_var)/10 in
  if first_dig = last_dig then is_palin (new_num, len-2) else
  false
;;

let rec length a b = 
  if a < 10 then b+1 else
  length (a/10) (b+1)
;;

let palin a = 
  is_palin (a, length a 0)
;;

let rec sol largest a b = 
  if b = 1000 then largest else
  if a = 1000 then sol largest 100 (b+1) else
  let prod = a*b in
  if palin prod && prod > largest then sol prod (a+1) b else
  sol largest (a+1) b
;;

Printf.printf "%d" (sol 0 100 100)