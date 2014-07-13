type valeur =
| Nombre of Nombre.nombre
| Chaine of string
| Liste of valeur list
| Fonction of (valeur list -> valeur)
type instruction =
| Valeur of valeur
| Implicite of string
| Application of instruction list
type memoire 
val make_mem : unit -> memoire
val add : memoire -> string -> valeur -> unit
(** Supprime l'ancienne valeur affectée à cette clé et la remplace **)
(** par la nouvelle **)
val rem : memoire -> string -> unit
val mem : memoire -> string -> bool
val evaluer : memoire -> instruction -> valeur
