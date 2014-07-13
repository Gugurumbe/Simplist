open Simplist ;;
open Stl ;;
let memoire = make_mem () ;;
load_stl memoire ;;
let resultat = evaluer memoire 
  (Application
     [
       Implicite "+" ;
       Valeur (Nombre (Nombre.nombre_of_string "1")) ;
       Valeur (Nombre (Nombre.nombre_of_string "2")) 
     ]) ;;
let nombre_trois = match resultat with
  | Nombre n -> n
  | _ -> 
    begin
      print_endline "Échec : n'est même pas un nombre !" ;
      Nombre.nombre_of_string "5698.45"
    end
;;
if Nombre.evalb (Nombre.substract nombre_trois (Nombre.nombre_of_string "3"))
then print_endline "Échec" 
else print_endline "Succès !"
;;

let resultat2 = evaluer memoire 
  (Application
     [
       Implicite "defun" ;
       Implicite "factorielle" ;
       Application [Implicite "n"] ;
       Application 
	 [
	   Implicite "cond" ;
	   Application
	     [Application 
		 [Implicite "<=" ; Implicite "n" ; Valeur (Nombre (Nombre.nombre_of_string "0"))] ;
	      Valeur (Nombre (Nombre.nombre_of_string "1"))] ;
	   Application
	     [Implicite "t" ; 
	      Application [ Implicite "*" ; Implicite "n" ; 
			    Application [Implicite "factorielle" ; Application 
			      [Implicite "-" ; Implicite "n" ; Valeur (Nombre (Nombre.nombre_of_string "1"))]
					]]]
	 ]
     ]) ;;
match resultat2 with
| Liste [] -> print_endline "La fonction defun a peut-être fonctionné..." 
| _ -> print_endline "Euh..." ;;
let resultat3 = evaluer memoire
  (Application [ Implicite "factorielle" ; Valeur (Nombre (Nombre.nombre_of_string "3"))]) ;;
match resultat3 with
| Nombre k when not (Nombre.evalb (Nombre.substract k (Nombre.nombre_of_string "6"))) -> print_endline "Encore un succès ! "
| _ -> print_endline "Échec..."
;;
