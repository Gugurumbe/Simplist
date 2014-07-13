type nombre =
| Entier of int
| Flottant of float ;;

let evalb = function
  | Entier k -> k <> 0
  | Flottant f -> f <> 0.
;;
let add x y =
  match (x, y) with
  | (Entier n, Entier p) -> Entier (n + p)
  | (Entier p, Flottant n)
  | (Flottant n, Entier p) ->
    let p_ = float_of_int p in
    Flottant (p_ +. n)
  | (Flottant n, Flottant p) -> Flottant (n +. p)
;;
let substract x y =
  match (x, y) with
  | (Entier n, Entier p) -> Entier (n - p)
  | (Flottant n, Entier p) -> 
    let p_ = float_of_int p in
    Flottant (n -. p_)
  | (Entier p, Flottant n) -> 
    let p_ = float_of_int p in
    Flottant (p_ -. n)
  | (Flottant n, Flottant p) ->
    Flottant (n -. p)
;;
let multiply x y =
  match (x, y) with
  | (Entier n, Entier p) -> Entier (n * p)
  | (Entier p, Flottant n)
  | (Flottant n, Entier p) ->
    let p_ = float_of_int p in
    Flottant (p_ *. n)
  | (Flottant n, Flottant p) -> Flottant (n *. p)
;;
(* Division "à la C" : 3 / 2 donne 1 *)
let divide x y =
  match (x, y) with
  | (Entier n, Entier p) -> Entier (n / p)
  | (Flottant n, Entier p) -> 
    let p_ = float_of_int p in
    Flottant (n /. p_)
  | (Entier p, Flottant n) -> 
    let p_ = float_of_int p in
    Flottant (p_ /. n)
  | (Flottant n, Flottant p) ->
    Flottant (n /. p)
;;
let nombre_of_string str =
  try
    Entier (int_of_string str)
  with
  | _ -> Flottant (float_of_string str)
;;
let positive = function
  | Flottant f when f >= 0. -> true
  | Entier f when f >= 0 -> true
  | _ -> false
;;
