let rec largest_factor a num =
  if num == 1 then a else
  if num mod a = 0 then 
  largest_factor a (num/a) else 
  largest_factor (a+2) num
;;

Printf.printf "%d" (largest_factor 3 600851475143)