type donnee =
| PO
| PF
| Mot of string
| Texte of string
(** En analysant un texte, on renvoie la liste des éléments. **)
(** Mot : un mot du texte. Exemples : quote, defun, 56, +, ... **)
(** Texte : une chaîne de caractères à utiliser dans le programme. **)
(** Exemples : "toto", "/!\\ Je dis : \"Toto !\"" pour les chaînes  **)
(** toto et /!\ Je dis : "Toto !" **)
(** NB : les mots peuvent contenir des espaces échappées. **)
(** Exemple : (defun ma\ super\ fonction ... **)
(** NB : les caractères PO et PF sont ( et ). **)
(** NB : les caractères ' non échappés seront traités de la sorte : **)
(** ' (a b c d) devient (quote a b c d) **)
(** ' truc génère une erreur **)
val lire : string -> int -> donnee list
