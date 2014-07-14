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
print_endline "Vous avez droit à print !" ;;
let texte = read_line () ;;
match interprete.eval texte with
| List [] -> print_endline "Ok."
| autre -> 
  begin
    print_endline "ATTENTION : un résultat a été retourné !" ;
    ignore (print [autre])
  end ;;
