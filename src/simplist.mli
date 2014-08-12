type valeur =
| Bool of bool
| Float of float
| Int of int
| String of string
| List of valeur list
| Function of (valeur list -> valeur)
| Enregistrement of (string, valeur) Hashtbl.t
type interprete =
{
  eval : string -> valeur ;
  add : string -> valeur -> unit ;
} 
val make_interp : (string -> unit) -> interprete
(** make_interp (que_faire_lorsque_l_utilisateur_dit_debug) **)
