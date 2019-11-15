let rec projeulerone a =
  if a = 1000 then 0 else
  if a mod 3 = 0 || a mod 5 = 0 then a + projeulerone (a+1) else projeulerone (a+1)
  ;;

Printf.printf "%d" (projeulerone 1)
