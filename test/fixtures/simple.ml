let rec fib n =
  match n with
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

let fact n =
  let rec impl n acc = if n = 0 then acc else impl (n - 1) (acc * n) in
  impl n 1
;;
