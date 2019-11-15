let rec fib a b =
  if a > 4000000 then 0 else
  if a mod 2 = 0 then a + (fib b (a+b)) else
  fib b (a+b)
  ;;
Printf.printf "%d" (fib 2 3)
