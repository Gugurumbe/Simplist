type nombre =
| Int of int
| Float of float
| Booleen of bool
type valeur =
| Nombre of nombre
| String of string
| Liste of valeur list
| Fonction of (valeur list -> valeur)
class memoire : 
object
  method mem : string -> bool
  method find : string -> valeur
  method ecraser : string -> valeur -> unit
  method copier : string list -> memoire
end
val evaluer : 
  memoire list -> 
  Simplist_prepare.champ Simplist_prepare.arbre ->
  (valeur * memoire list)
(** ATTENTION : l'instruction "defun" n'existe pas encore, utiliser "global/local <nom> (lambda (<arg1> <arg2> <...>) (<resultat>)))" **)
(** Plus tard, je chargerai le préprocesseur (simplist_read) de le faire automatiquement, comme '(a b c d) pour (quote a b c d) **)
(** (global deux (+ 1 1))                 **) (** -> liste : [] **)
(** (+ deux 1)                            **) (** -> nombre : 3 **)
(** (local trois (+ 2 1) (+ trois 1))     **) (** -> nombre : 4 **)
(** (+ trois 1)                           **) (** -> erreur **)
