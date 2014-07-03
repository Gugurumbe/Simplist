type nombre =
| Int of int
| Float of float
| Booleen of bool
;;
type valeur =
| Nombre of nombre
| String of string
| Liste of valeur list
| Fonction of (valeur list -> valeur)
;;
class memoire = 
object(self)
  val mem : (string, valeur) Hashtbl.t = Hashtbl.create 100
  method mem truc = Hashtbl.mem mem truc
  method find truc = Hashtbl.find mem truc
  method ecraser nom nouvelle =
    if self#mem nom then Hashtbl.remove mem nom ;
    Hashtbl.add mem nom nouvelle
  method copier dep =
    let m2 = new memoire in
    let rec aux = function
      | [] -> m2
      | nom :: reste ->
	begin
	  let valeur = self#find nom in
	  m2#ecraser nom valeur ;
	  aux reste
	end
    in
    aux dep
end ;;
let rec find str = function
  | [] -> 
    failwith (" Erreur : "^str^" non défini.")
  | a :: _ when a#mem str -> a#find str
  | _ :: b -> find str b
;;
let rec evaluer memlist = function
  | Simplist_prepare.Feuille f ->
    begin
      match f with
      | Simplist_prepare.Alias str ->
	(find str memlist, memlist)
      | Simplist_prepare.String str -> (String str, memlist)
      | Simplist_prepare.Int i -> (Nombre (Int i), memlist)
      | Simplist_prepare.Float f -> (Nombre (Float f), memlist)
    end
  | Simplist_prepare.Noeud [] -> (Liste [], memlist)
  | Simplist_prepare.Noeud ([
    Simplist_prepare.Feuille (Simplist_prepare.Alias "if") ; 
    test ; c1 ; c2]) -> 
    begin
      match evaluer memlist test with
      | (Nombre (Int 0), memlist)
      | (Nombre (Booleen false), memlist)
      | (Nombre (Float 0.), memlist) -> 
	 evaluer memlist c2
      | _ -> evaluer memlist c1
    end
  | Simplist_prepare.Noeud (
    (Simplist_prepare.Feuille (Simplist_prepare.Alias "if")) 
    :: _) ->
    failwith ("Erreur de syntaxe pour if.")
  | Simplist_prepare.Noeud ([
    Simplist_prepare.Feuille (Simplist_prepare.Alias "global") ;
    Simplist_prepare.Feuille (Simplist_prepare.Alias nom) ;
    resultat]) ->
    begin
      let (valeur, _) = evaluer memlist resultat in
      match memlist with
      | [] -> 
	let mem = new memoire in
	(mem#ecraser nom valeur ; (Liste [], [mem]))
      | a :: b ->
	(a#ecraser nom valeur ; (Liste [], a :: b))
    end
  | Simplist_prepare.Noeud ([
    Simplist_prepare.Feuille (Simplist_prepare.Alias "local") ;
    Simplist_prepare.Feuille (Simplist_prepare.Alias nom) ; 
    resultat ; suite ]) ->
    begin
      let (valeur, _) = evaluer memlist resultat in
      let mem = new memoire in 
      mem#ecraser nom valeur ; 
      let (valeur, _) = evaluer (mem :: memlist) suite in
      (valeur, memlist)
    end
  | Simplist_prepare.Noeud (
    (Simplist_prepare.Feuille (Simplist_prepare.Alias str)) :: b)  ->
    begin
      let fonction = find str memlist in
      let arguments = List.map (fun arg -> fst (evaluer memlist arg)) b in
      match fonction with
      | Fonction f -> (f arguments, memlist)
      | _ -> failwith ("Erreur : "^str^
			  " n'est pas une fonction.")
    end
  | _ -> failwith ("Erreur de syntaxe.")
;;
