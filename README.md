# Quoridor en OCaml

Le [Quoridor](https://fr.wikipedia.org/wiki/Quoridor) se joue à 2 ou 4 joueurs, sur un plateau de dimensions 9×9.

Chaque joueur débute d'un côté différent du plateau, et a pour but d'atteindre le côté opposé.

Pour ce faire, 20 murs sont distribués équitablement entre les joueurs, et chaque joueur dispose lors de son tour de deux options : se déplacer d'une case (seulement orthogonalement) ou placer un mur sur le plateau.

Un mur permet d'empêcher les déplacements des joueurs, et bloque deux cases à la fois. Ils peuvent être placés verticalement ou horizontalement, mais ne peuvent pas être déplacés ou retirés

Il est interdit de placer un mur si celui-ci bloque tous les chemins possible pour atteindre l'autre côté du plateau (chaque joueur doit toujours être capable d'atteindre son but par une série de déplacements).

Lorsque deux joueurs se retrouvent face-à-face, le joueur dont c'est le tour peut \"sauter\" par dessus l'autre joueur, sauf si un mur se trouve derrière le joueur sauté.

La partie se termine lorsqu'un joueur atteint le bord opposé de celui de son point de départ.
