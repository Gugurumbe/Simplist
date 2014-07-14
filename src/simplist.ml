open Nombre ;;
type valeur =
| Nombre of nombre
| Chaine of string 
| Liste of valeur list
| Fonction of (valeur list -> valeur)
;;
type instruction =
| Valeur of valeur
| Implicite of string
| Application of instruction list
;;
type memoire = (string, valeur) Hashtbl.t ;;
let add mem cle v =
  if Hashtbl.mem mem cle then Hashtbl.remove mem cle ;
  Hashtbl.add mem cle v
;;
let rem mem cle =
  Hashtbl.remove mem cle
;;
let mem mem cle = Hashtbl.mem mem cle ;;
let make_mem () = Hashtbl.create 100 ;;
let evalb = function
  | Nombre x -> evalb x
  | Chaine str -> failwith ("Cannot convert string "^str^" to boolean.")
  | Liste _ -> failwith ("Cannot convert list to boolean.")
  | Fonction _ -> failwith ("Cannot convert function to boolean.")
;;
let rec argnames = function
  | [] -> []
  | (Implicite nom) :: reste -> nom :: (argnames reste)
  | _ -> failwith " not a name."
;;
let rec copier_dependances source dest = function
  | Application liste ->
    List.iter (copier_dependances source dest) liste
  | Implicite k when Hashtbl.mem source k ->
    Hashtbl.add dest k (Hashtbl.find source k)
  | Valeur _ 
  | Implicite _ -> ()
;;
let lier_args noms valeurs memoire =
  let rec aux = function
    | ([], []) -> ()
    | (nom :: a, valeur :: b) ->
      begin
	if Hashtbl.mem memoire nom then Hashtbl.remove memoire nom ;
	Hashtbl.add memoire nom valeur ;
	aux (a, b)
      end
    | _ -> failwith " wrong number of arguments."
  in
  aux (noms, valeurs)
;;
let rec evaluer_single mem = function
  | Valeur v -> v
  | Implicite str when Hashtbl.mem mem str -> Hashtbl.find mem str
  | Implicite str -> failwith (str^" unbound.")
  | Application liste ->
    match liste with
    | [] -> Liste []
    | (Valeur _) :: _ -> failwith ("Syntax error.")
    | [Application truc] -> evaluer_single mem (Application truc)
    | (Application effet1) :: reste ->
      begin
	match evaluer_single mem (Application effet1) with
	| Liste [] -> evaluer_single mem (Application reste)
	| _ -> failwith ("Should have type unit.")
      end
    | (Implicite a) :: b ->
      match a with
      | "if" -> evaluer_if mem liste
      | "cond" -> evaluer_cond mem liste
      | "defun" -> evaluer_defun mem liste
      | _ ->
	if Hashtbl.mem mem a  then
	  match Hashtbl.find mem a with
	  | Fonction f -> f (List.map (evaluer_single mem) b)
	  | _ -> failwith (a^" is not a function.")
	else failwith (a^" : unbound function.")
and evaluer_if mem = function
  | [Implicite "if" ; test ; effet ; _] when evalb (evaluer_single mem test) -> 
    evaluer_single mem effet
  | [Implicite "if" ; _ ; _ ; effet] -> 
    evaluer_single mem effet
  | _ -> failwith ("if : syntax error.")
and evaluer_cond mem = function
  | (Implicite "cond") :: paires ->
    let fonction_iteree (fait, resultat) = function
      | Application [test ; cas] 
	  when not (fait) && evalb (evaluer_single mem test) 
	    -> (true, evaluer_single mem cas)
      | Application _ when not (fait) -> (false, Liste [])
      | Application _ -> (true, resultat)
      | _ -> failwith "cond : syntax error"
    in
    let (ok, res) = List.fold_left (fonction_iteree) (false, Liste []) paires in
    if ok then res
    else failwith "cond : cas non répertorié"
  | _ -> failwith "cond : syntax error"
and evaluer_defun mem = function
  | [Implicite "defun" ; Implicite nom ; Application liste_args ; resultat] ->
    begin
      try
	let noms = argnames liste_args in
	let memoire_locale = Hashtbl.create 20 in
	copier_dependances mem memoire_locale resultat ;
	let rec fonction arguments =
	  try
	    lier_args noms arguments memoire_locale ;
	    if Hashtbl.mem memoire_locale nom 
	    then Hashtbl.remove memoire_locale nom ;
	    Hashtbl.add memoire_locale nom (Fonction (fonction)) ;
	    evaluer_single memoire_locale resultat
	  with
	  | Failure str -> failwith ("Call of "^nom^" : "^str)
	in
	if Hashtbl.mem mem nom then Hashtbl.remove mem nom ;
	Hashtbl.add mem nom (Fonction (fonction)) ;
	Liste []
      with
      | Failure str -> failwith ("Definition of "^nom^" : "^str)
    end
  | (Implicite "defun") :: (Implicite nom) :: (Application liste) :: resultat :: reste ->
    failwith ("defun : args ("^(string_of_int (List.length reste))^")")
  | _ -> failwith "defun : syntax error"
;;

let rec evaluer mem = function
  | [] -> Liste []
  | [seule] -> evaluer_single mem seule
  | unitaire :: reste ->
    match evaluer_single mem unitaire with
    | Liste [] -> evaluer mem reste
    | _ -> failwith "Vous évaluez plusieurs choses. Seule la dernière doit renvoyer une valeur."
;;
