let rec f0 n =
  match n with
  | 0 | 1 -> 1
  | n -> f0 (n - 1) + f0 (n - 2)
;;

let rec f1 n =
  match n with
  | 0 | 1 -> 2
  | n -> f1 (n - 1) + f1 (n - 2)
;;

let rec f2 n =
  match n with
  | 0 | 1 | 2 -> 2
  | n -> f2 (n - 2) + f2 (n - 3)
;;
