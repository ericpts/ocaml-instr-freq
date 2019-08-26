open Core

let hash_fold_array f state array = Array.fold array ~init:state ~f

type basic_color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

let basic_color_to_int = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7
;;

let color basic_color text =
  let number = basic_color_to_int basic_color in
  Printf.sprintf "\027[38;5;%dm%s\027[0m" number text
;;
