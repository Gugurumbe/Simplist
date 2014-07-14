open Simplist ;;
type word =
| Word of string
| Quote of string
;;
(* mot * prochaine initiale *)
let read_word chaine debut =
  let rec aux i en_guillemets =
    if i < String.length chaine then
      match chaine.[i] with
      | '\\' when i + 1 < String.length chaine -> 
	let (fin, i_fin) = aux (i + 2) en_guillemets in
	(chaine.[i + 1] :: fin, i_fin)
      | '('
      | ')'
      |'\''
      | ' '
      | '\n'
      | '\t' when not en_guillemets -> ([], i)
      | '\"' -> ([], i)
      | c ->
	let (fin, i_fin) = aux (i + 1) en_guillemets in
	(c :: fin, i_fin)
    else ([], i)
  in
  let rec avancer i =
    if i < String.length chaine then
      match chaine.[i] with
      | ' ' 
      | '\n'
      | '\t' -> avancer (i + 1)
      | _ -> i
    else i
  in
  if debut < String.length chaine then
    match chaine.[debut] with
    | '('
    | ')'
    | '\'' -> (Word (String.make 1 chaine.[debut]), avancer (debut + 1))
    | '\"' ->
      let (lettres, i_fin) = aux (debut + 1) true in
      (Quote (String.concat "" (List.map (String.make 1) lettres)), 
       avancer (i_fin + 1))
    | _ ->
      let (lettres, i_fin) = aux debut false in
      (Word (String.concat "" (List.map (String.make 1) lettres)), 
       avancer i_fin)
  else (Word "", debut)
;;

let decouper chaine =
  let rec aux i =
    if i < String.length chaine then
      let (mot_suivant, i_fin) = read_word chaine i in
      mot_suivant :: (aux i_fin)
    else []
  in
  aux 0
;;

let rec remplacer_quote = function
  | (Word "'") :: (Word "(") :: reste -> 
    (Word "(") :: (Word "quote") :: reste
  | (Word "'") :: _ ->
    failwith "Analyse syntaxique : votre ' n'a rien à faire ici."
  | [] -> []
  | a :: b ->
    a :: (remplacer_quote b)
;;

type objet =
| Donnee of valeur
| Mot of string
| PO
| PF
;;

let simparser chaine =
  let liste_mots = remplacer_quote (decouper chaine) in
  let reconnaitre = function
    | Quote truc -> Donnee (Chaine truc)
    | Word "(" -> PO
    | Word ")" -> PF
    | Word machin ->
      begin
	try
	  Donnee (Nombre (Nombre.nombre_of_string machin))
	with
	| _ -> Mot machin
      end
  in
  let liste_objets = List.map (reconnaitre) liste_mots in
  let rec aux = function
    | [] -> (*failwith "Analyse syntaxique : fin inattendue." Non : 
	    il faut pouvoir parser "()()" !*) ([], [])
    | PO :: suite ->
      begin
	let (liste, fin) = aux suite in
	let (reste_niveau, finfin) = aux fin in
	((Application liste) :: reste_niveau, finfin)
      end
    | PF :: suite ->
      ([], suite)
    | (Donnee truc) :: suite ->
      let (reste_niveau, fin) = aux suite in
      ((Valeur truc) :: reste_niveau, fin)
    | (Mot bidule) :: suite ->
      let (reste_niveau, fin) = aux suite in
      ((Implicite bidule) :: reste_niveau, fin)
  in
  fst (aux liste_objets)
;;
