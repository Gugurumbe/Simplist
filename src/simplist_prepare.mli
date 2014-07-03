type champ =
| Alias of string
| String of string
| Int of int
| Float of float
type 'a arbre =
| Feuille of 'a
| Noeud of 'a arbre list
val preparer : Simplist_read.donnee list -> champ arbre list
