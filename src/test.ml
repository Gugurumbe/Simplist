open Simplist ;;
let interprete = Simplist.make_interp () ;;
let separer k = [k] ;;
let rec print = function
  |[Float f] -> 
    begin
      print_string ("(flottant "^(string_of_float f)^")") ;
      List []
    end
  |[Int i] -> 
    begin
      print_string ("(entier "^(string_of_int   i)^")") ;
      List []
    end
  |[String s] -> 
    begin
      print_string ("(chaîne "^s^")") ;
      List []
    end
  |[List liste] ->
    begin
      print_string "(liste [" ;
      let _ = List.map (print) (List.map (separer) liste) in
      print_string "])" ;
      List []
    end
  | [Function _] ->
    begin
      print_string "(fonction)" ;
      List []
    end
  | _ -> failwith "print ne peut afficher qu'un seul élément !"
;;
interprete.add "print" (Function (print)) ;;
let rec read_all fichier =
  try
    let ligne = input_line fichier in
    ligne^"\n"^(read_all fichier)
  with
  | End_of_file -> ""
;;
match Sys.argv with
| [|nom_prog|] -> failwith "Vous ne me donnez rien à interpréter, je boude !"
| [|nom_prog ; nom_fichier|] when Sys.file_exists nom_fichier ->
  begin
    let fichier = open_in nom_fichier in
    let texte = read_all fichier in
    close_in fichier ;
    print_endline "J'interprète : " ;
    print_endline texte ;
    match interprete.eval texte with
    | List [] -> print_endline "Ok."
    | autre -> 
      begin
	  print_endline "ATTENTION : un résultat a été retourné !" ;
	ignore (print [autre])
      end 
  end
| [| _ ; texte |] ->
  begin
    print_endline "J'interprète : " ;
    print_endline texte ;
    match interprete.eval texte with
    | List [] -> print_endline "Ok."
    | autre -> 
      begin
	print_endline "ATTENTION : un résultat a été retourné !" ;
	ignore (print [autre])
      end       
  end
| _ ->  print_endline "Usage : <nom du programme> nom_de_fichier ou <nom> texte_a_interpreter. Vous avez droit à print."
;;
