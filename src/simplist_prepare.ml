open Simplist_read ;;
type champ =
| Alias of string
| String of string
| Int of int
| Float of float
;;
type 'a arbre =
| Feuille of 'a
| Noeud of 'a arbre list
;;
let rec deduire_type str =
  (* nombre ou mot *)
  try
    Int (int_of_string str)
  with
  | _ ->
    try
      Float (float_of_string str)
    with
    | _ -> Alias str
;;
let rec preparer = function
  | [] -> ([], [])
  | PO :: t ->
    let (fils, fin) = preparer t in
    let (autres, finfin) = preparer fin in
    ((Noeud fils) :: autres, finfin)
  | PF :: t -> ([], t)
  | (Mot s) :: t ->
    let (autres, fin) = preparer t in
    ((Feuille (deduire_type s)) :: autres, fin)
  | (Texte s) :: t ->
    let (autres, fin) = preparer t in
    ((Feuille (String s)) :: autres, fin)
;;
let preparer liste =
  match preparer liste with
  | (arbres, _) ->
      arbres
;;
