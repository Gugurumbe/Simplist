open Simplist ;;
let fonction_plus liste =
  let aux acc = function
    | Nombre k -> Nombre.add acc k
    | _ -> failwith "Wrong arg type for function +"
  in
  Nombre (List.fold_left (aux) (Nombre.nombre_of_string "0") liste)
;;
let fonction_moins = function
  | [Nombre gros ; Nombre petit] -> Nombre (Nombre.substract gros petit)
  | _ -> failwith "Wrong args for function -"
;;
let fonction_fois liste =
  let aux acc = function
    | Nombre k -> Nombre.multiply acc k
    | _ -> failwith "Wrong arg type for function *"
  in
  Nombre (List.fold_left (aux) (Nombre.nombre_of_string "1") liste)
;;
let fonction_diviser = function
  | [Nombre gros ; Nombre petit] -> Nombre (Nombre.divide gros petit)
  | _ -> failwith "Wrong args for function /"
;;
let fonction_superieur = function
  | [Nombre gros ; Nombre petit] ->
    let difference = Nombre.substract gros petit in
    Nombre (Nombre.nombre_of_string (
      if Nombre.positive difference then "1" 
      else "0"))
  | _ -> failwith "Wrong args for function >="
;;
let fonction_supstrict = function
  | [Nombre gros ; Nombre petit] ->
    let difference = Nombre.substract gros petit in
    Nombre (Nombre.nombre_of_string (
      if Nombre.evalb difference && Nombre.positive difference then "1"
      else "0"))
  | _ -> failwith "Wrong args for function >"
;;
let fonction_egal = function
  | [Nombre gros ; Nombre petit] ->
    let difference = Nombre.substract gros petit in
    Nombre (Nombre.nombre_of_string (
      if Nombre.evalb difference then "0"
      else "1"))
  | _ -> failwith "Wrong args for function ="
;;
let fonction_different = function
  | [Nombre gros ; Nombre petit] ->
    let difference = Nombre.substract gros petit in
    Nombre (Nombre.nombre_of_string (
      if Nombre.evalb difference then "1"
      else "0"))
  | _ -> failwith "Wrong args for function ="
;;
let fonction_inferieur = function
  | [Nombre gros ; Nombre petit] ->
    let difference = Nombre.substract gros petit in
    Nombre (Nombre.nombre_of_string (
      if Nombre.evalb difference && Nombre.positive difference then "0"
      else "1"))
  | _ -> failwith "Wrong args for function >"
;;
let fonction_infstrict = function
  | [Nombre gros ; Nombre petit] ->
    let difference = Nombre.substract gros petit in
    Nombre (Nombre.nombre_of_string (
      if Nombre.positive difference then "0" 
      else "1"))
  | _ -> failwith "Wrong args for function >="
;;
let load_stl memoire =
  add memoire "t" (Nombre (Nombre.nombre_of_string "1")) ;
  add memoire "quote" (Fonction (fun liste -> Liste liste)) ;
  add memoire "+" (Fonction (fonction_plus)) ;
  add memoire "-" (Fonction (fonction_moins)) ;
  add memoire "*" (Fonction (fonction_fois)) ;
  add memoire "/" (Fonction (fonction_diviser)) ;
  add memoire ">=" (Fonction (fonction_superieur)) ;
  add memoire ">" (Fonction (fonction_supstrict)) ;
  add memoire "<=" (Fonction (fonction_inferieur)) ;
  add memoire "<" (Fonction (fonction_infstrict)) ;
  add memoire "=" (Fonction (fonction_egal)) ;
  add memoire "!=" (Fonction (fonction_different)) ; ()
;;
