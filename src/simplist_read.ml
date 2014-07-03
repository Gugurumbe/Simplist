type donnee = 
| PO
| PF
| Mot of string
| Texte of string ;;
let rec dechapper chaine i = (* Enlève des '\' trop nombreux *)
  if i < String.length chaine then
    match chaine.[i] with
    | '\"' -> ([], i + 1)
    | '\\' when i + 1 < String.length chaine -> 
      let fin_mot, indice_fin = dechapper chaine (i + 2) in
      (chaine.[i + 1] :: fin_mot, indice_fin)
    | x -> 
      let (fin_mot, indice_fin) = dechapper chaine (i + 1) in
      (x :: fin_mot, indice_fin)
  else ([], i)
;;
let rec end_of_word chaine i = (* Donne la position de la fin du mot *)
  if i < String.length chaine then 
    match chaine.[i] with
    | '(' 
    | ')'
    | ' ' -> i
    | '\\' -> end_of_word chaine (i + 2)
    | _ -> end_of_word chaine (i + 1)
  else i
;;
let mkstring liste_chars = 
  String.concat "" (List.map (String.make 1) liste_chars) 
;;
let rec lire chaine i =
  if i < String.length chaine then
    match chaine.[i] with
    | '\"' ->
      let (mot, i_fin) = dechapper chaine (i + 1) in
      Texte (mkstring mot) :: (lire chaine i_fin) 
    | '(' ->
      PO :: (lire chaine (i + 1))
    | ')' ->
      PF :: (lire chaine (i + 1))
    | '\'' ->
      begin
	match lire chaine (i + 1) with
	| PO :: fin ->
	  PO :: (Mot "quote") :: fin
	| _ -> failwith ((string_of_int i)^"Symbole quote non accolé à une parenthèse" )
      end
    | '\n' | '\t' | ' ' ->
      lire chaine (i + 1)
    | _ ->
      let fin_mot = end_of_word chaine i in
      let mot = String.sub chaine i (fin_mot - i) in
      let (mot, _) = dechapper mot 0 in
      let mot = mkstring mot in
      (Mot mot) :: (lire chaine fin_mot)
  else []
;;
