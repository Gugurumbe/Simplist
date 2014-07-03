open Simplist_evaluer ;;
let mem = new memoire ;;
let plus liste =
  let aux a b =
    match (a, b) with
    | (Nombre (Int ia), Nombre (Int ib)) -> Nombre (Int (ia + ib))
    | (Nombre (Int ia), Nombre (Float fb))
    | (Nombre (Float fb), Nombre (Int ia)) -> 
      Nombre (Float ((float_of_int ia) +. fb))
    | (Nombre (Float fa), Nombre (Float fb)) ->
      Nombre (Float (fa +. fb))
    | (Nombre (Booleen false), Nombre (Int 0))
    | (Nombre (Booleen false), Nombre (Float 0.))
    | (Nombre (Int 0), Nombre (Booleen false))
    | (Nombre (Float 0.), Nombre (Booleen false)) ->
      Nombre (Booleen false)
    | (Nombre (Booleen _), Nombre _)
    | (Nombre _, Nombre (Booleen _)) -> 
      Nombre (Booleen true)
    | _ -> failwith "Mauvais arguments pour plus"
  in
  List.fold_left (aux) (Nombre (Int 0)) liste
;;
let print = function
  | [String str] -> 
    begin
      print_endline str ;
      Liste []
    end
  | _ -> failwith "Mauvais arguments pour print_endline"
;;
mem#ecraser "+" (Fonction (plus)) ;;
mem#ecraser "print_endline" (Fonction (print)) ;;
let chaine1 = 
  "(if (+ 1 -1) (print_endline \"Échec\") (print_endline \"Succès\"))" ;;
let chaine2 =
  "(global deux (local un 1 (+ un un)))"
;;
let (valeur, mem) = evaluer [mem] (List.hd (Simplist_prepare.preparer (Simplist_read.lire chaine2 0))) ;;
match (valeur, mem) with
| (Simplist_evaluer.Liste [], [mem]) 
    when mem#mem "deux" && not (mem#mem "un") ->
  print_endline "RÉUSSI !"
| _ -> print_endline "Échec..."
;;
