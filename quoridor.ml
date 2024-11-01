type wall = (* une case ne peut pas contenir à la fois un mur vertical et un mur horizontal : en effet, les murs étant étdendus sur deux cases, avoir deux murs sur une même case entraineraît une collision entre un mur vertical et un horizontal *)
  | V (* vertical *)
  | H (* horizontal *)

type move =
  | U (* haut *)
  | D (* bas *)
  | L (* gauche *)
  | R (* droite *)

type players = ((int * int) * int) array (* tableau des joueurs : couple position (x, y) * murs restants *)
type board = (wall option) array array (* plateau de jeu : tableau 2D des murs *)
type game = board * players

let dim = 9 (* dimensions du jeu *)

(* * Initialisation du jeu * *)
let init_game nb_p : game =
  if nb_p <> 2 && nb_p <> 4 then failwith "Seulement 2 ou 4 joueurs."
  else begin
    let nb_w = 20/nb_p in (* 20 murs distribués équitablements *)
    let players = Array.make nb_p ((0, dim/2), nb_w) in
    players.(1) <- (dim-1, dim/2), nb_w;
    if nb_p = 4 then begin
      players.(2) <- (dim/2, 0), nb_w;
      players.(3) <- (dim/2, dim-1), nb_w;
    end;
    Array.make_matrix (dim - 1) (dim - 1) None, players
  end

(* * Affichage * *)
let char_of_player n =
  match n with
  | 0 -> "🨯"
  | 1 -> "🨅"
  | 2 -> "🨚"
  | 3 -> "🩄"
  | _ -> failwith "Le joueur n'existe pas"

let player_pos (game: game) p =
  fst (snd game).(p)

let player_walls (game: game) p =
  snd (snd game).(p)

let find_index f t =
  let n = Array.length t in
  let rec find i =
    if i = n then None
    else if f t.(i) then Some i
    else find (i + 1)
  in find 0

let show_game (game: game) =
  let b, plyrs = game in
  Printf.printf "Joueur %s : %d murs\tJoueur %s : %d murs\n" (char_of_player 0) (player_walls game 0) (char_of_player 1) (player_walls game 1);
  if Array.length plyrs = 4 then Printf.printf "Joueur %s : %d murs\tJoueur %s : %d murs\n" (char_of_player 2) (player_walls game 2) (char_of_player 3) (player_walls game 3);
  print_string "    ";
  for y = 0 to dim - 2 do
    print_char (char_of_int (y+97));
    print_string "    "
  done;
  print_newline ();
  for i = 0 to dim - 1 do
    print_string "  ";
    for j = 0 to dim - 1 do
      (match find_index (fun a -> fst a = (i, j)) plyrs with
      | Some n_p -> print_string (char_of_player n_p)
      | None -> print_string "◌");
      if (j < dim - 1) && ((i < dim - 1 && b.(i).(j) = Some V) || (i > 0 && b.(i-1).(j) = Some V))
        then print_string " ▕▏ "
      else print_string "    "
    done;
    print_newline ();
    if i < dim - 1 then begin
      print_int (i + 1);
      print_char ' ';
      for j = 0 to dim - 2 do
        if b.(i).(j) = Some H then print_string "━━━━━"
        else if j > 0 && b.(i).(j-1) = Some H then print_string "━"
        else print_string " ";

        if b.(i).(j) = Some V then print_string " ▕▏ "
        else if b.(i).(j) <> Some H then print_string "    "
      done;
      if b.(i).(dim - 2) = Some H then print_string "━";
      print_newline ()
    end
  done;

(* * Déplacement * *)
exception Wrong_move

let is_blocked x y dir b =
  match dir with
  | -1, 0 -> (x < 1) || (y < dim - 1 && b.(x-1).(y) = Some H) || (y >= 1 && b.(x-1).(y-1) = Some H)
  | 1, 0 -> (x > dim - 2) || (y < dim - 1 && b.(x).(y) = Some H) || (y >= 1 && b.(x).(y-1) = Some H)
  | 0, -1 -> (y < 1) || (x < dim - 1 && b.(x).(y-1) = Some V) || (x >= 1 && b.(x-1).(y-1) = Some V)
  | 0, 1 -> (y > dim - 2) || (x < dim - 1 && b.(x).(y) = Some V) || (x >= 1 && b.(x-1).(y) = Some V)
  | _ -> raise Wrong_move

let player_exists_at pos jrs = Array.exists (fun j -> fst j = pos) jrs

let coords_of_move m =
  match m with
  | U -> -1, 0
  | D -> 1, 0
  | L -> 0, -1
  | R -> 0, 1

let rec try_jump game j m (x, y) dir plyrs =
  let dx, dy = dir in
  let new_x, new_y = x + dx, y + dy in
  if player_exists_at (new_x, new_y) plyrs then begin
    let jump_x, jump_y = x + 2 * dx, y + 2 * dy in
    (* Empêche de sauter par dessus + de 2 joueurs *)
    if player_exists_at (jump_x, jump_y) plyrs then raise Wrong_move;
      plyrs.(j) <- (new_x, new_y), snd plyrs.(j);
      (try move game j m with Wrong_move -> (plyrs.(j) <- (x, y), snd plyrs.(j); raise Wrong_move));
      jump_x, jump_y
    end
  else new_x, new_y
and move (game: game) j m =
  let b, plyrs = game in
  let (x, y), n = plyrs.(j) in
  let dir = coords_of_move m in
  if is_blocked x y dir b then raise Wrong_move;
  let new_x, new_y = try_jump game j m (x, y) dir plyrs in
  plyrs.(j) <- (new_x, new_y), n

(* * Entrées utilisateurs * *)
let coords_of_input s =
  if String.length s <> 2 then raise (Invalid_argument "Entrée utilisateur trop longue/courte !")
  else begin
    let l, n = int_of_char s.[0], int_of_char s.[1] in
    let x =
      if n >= 49 && n <= 57 then n - 49
      else raise (Invalid_argument "Chiffre erroné !") in
    let y =
      if l >= 97 && l <= 105 then l - 97 (* minuscules *)
      else if l >= 65 && l <= 73 then l - 65(* majuscules *)
      else raise (Invalid_argument "Lettre erronée !") in
      x,y
    end

let move_of_input s =
  if String.length s <> 1 then raise (Invalid_argument "Déplacement invalide !")
  else begin
    match s.[0] with
    | 'h' | 'H' -> U
    | 'b' | 'B' -> D
    | 'g' | 'G' -> L
    | 'd' | 'D' -> R
    | _ -> raise (Invalid_argument "Déplacement invalide !")
  end

(* * Placement de mur * *)
exception Obstructed
exception Insufficient_walls

let place_wall (game: game) x y w =
  if x > dim - 2 || y > dim - 2 || x < 0 || y < 0 then failwith "Coordonnées du mur hors du cadre"
  else let b, _ = game in
  if b.(x).(y) <> None then raise Obstructed
  else match w with
  | V ->
    if (x + 1 < dim - 1 && b.(x+1).(y) = Some V) || (x - 1 >= 0 && b.(x-1).(y) = Some V) then raise Obstructed
    else b.(x).(y) <- Some V
  | H ->
    if (y + 1 < dim - 1 && b.(x).(y+1) = Some H) || (y - 1 >= 0 && b.(x).(y-1) = Some H) then raise Obstructed
    else b.(x).(y) <- Some H

let possible_wall (game: game) x y w =
  let b, plyrs = game in
  let temp_b = Array.map Array.copy b in
  temp_b.(x).(y) <- Some w;
  let rec explore g p explored n =
    if n = 25 then false
    else begin
      let (px, py), _ = (snd g).(p) in
      show_game g;
      (List.for_all (fun p -> p <> (px - 1, py)) explored && try
        move g p U;
        let (x, y), _ = (snd g).(p) in
        if p = 1 && x = 0 then true
        else explore g p ((x, y)::explored) (n+1)
      with Wrong_move -> false)
      || (List.for_all (fun p -> p <> (px + 1, py)) explored && try
        move g p D;
        let (x, y), _ = (snd g).(p) in
        if p = 0 && x = dim - 1 then true
        else explore g p ((x, y)::explored) (n+1)
      with Wrong_move -> false)
      || (List.for_all (fun p -> p <> (px, py - 1)) explored && try
        move g p L;
        let (x, y), _ = (snd g).(p) in
        if p = 3 && y = 0 then true
        else explore g p ((x, y)::explored) (n+1)
      with Wrong_move -> false)
      || (List.for_all (fun p -> p <> (px, py + 1)) explored && try
        move g p R;
        let (x, y), _ = (snd g).(p) in
        if p = 2 && y = dim - 1 then true
        else explore g p ((x, y)::explored) (n+1)
      with Wrong_move -> false)
    end
    in explore (temp_b, Array.copy plyrs) 0 [] 0 && explore (temp_b, Array.copy plyrs) 1 [] 0 && explore (temp_b, Array.copy plyrs) 2 [] 0 && explore (temp_b, Array.copy plyrs) 3 [] 0

let check_winner (game: game) p =
  let (x, y), _ = (snd game).(p) in
  match p with
  | 0 -> x = dim - 1
  | 1 -> x = 0
  | 2 -> y = dim - 1
  | 3 -> y = 0
  | _ -> false

let print_rules () =
  print_string "Le Quoridor se joue à 2 ou 4 joueurs, sur un plateau de dimensions 9×9.
Chaque joueur débute d'un côté différent du plateau, et a pour but d'atteindre le côté opposé.
Pour ce faire, 20 murs sont distribués équitablement entre les joueurs, et chaque joueur dispose lors de son tour de deux options : se déplacer d'une case (seulement orthogonalement) ou placer un mur sur le plateau.
Un mur permet d'empêcher les déplacements des joueurs, et bloque deux cases à la fois. Ils peuvent être placés verticalement ou horizontalement, mais ne peuvent pas être déplacés ou retirés
Il est interdit de placer un mur si celui-ci bloque tous les chemins possible pour atteindre l'autre côté du plateau (chaque joueur doit toujours être capable d'atteindre son but par une série de déplacements).
Lorsque deux joueurs se retrouvent face-à-face, le joueur dont c'est le tour peut \"sauter\" par dessus l'autre joueur, sauf si un mur se trouve derrière le joueur sauté.
La partie se termine lorsqu'un joueur atteint le bord opposé de celui de son point de départ."

let next_p p nb_p =
    match nb_p, p with
    | 2, p -> 1 - p
    | 4, 0 -> 3 (* dans       *)
    | 4, 1 -> 2 (* le         *)
    | 4, 2 -> 0 (* sens       *)
    | 4, 3 -> 1 (* anti-trigo *)
    | _ -> failwith "Nombre de joueurs incorrect"

let start () =
  print_string "Bienvenue dans le jeu du Quoridor\nPour un rappel des règles, entrer `r`, sinon entrer `2` ou `4` selon le nombre de joueurs.";
  let nb_p = let rec prompt () =
    print_string "\n> ";
    match read_line () with
    | "r" | "R" -> (print_rules (); prompt ())
    | "2" -> 2
    | "4" -> 4
    | _ -> (print_string "\nEntrée invalide ! Réessayer.\n> "; prompt ())
  in prompt () in
  let g = init_game nb_p in
  let rec game_loop p =
    show_game g;
    Printf.printf "C'est au tour du joueur %s.\nQuelle action effectuer ?\n - `m` : placer un mur\n - `h` : déplacer le piont vers le haut\n - `b` : déplacer le piont vers le bas\n - `d` : déplacer le piont vers la droite\n - `g` : déplacer le piont vers la gauche\n" (char_of_player p);
    let rec ask_user () =
      print_string "\n> ";
      try match read_line () with
        | "m" | "M" ->
          if snd (snd g).(p) < 1 then begin
            print_string "Pas assez de murs restant, vous ne pouvez que vous déplacer.";
            ask_user ()
          end
          else let (x, y), n = (snd g).(p) in (snd g).(p) <- (x, y), (n-1);
          print_string "\nEntrer les coordonnées du mur (ex : `a1`)";
          let rec ask_coords () =
            print_string "\n> ";
            try let x, y = coords_of_input (read_line ()) in
            print_string "\nEntrer le type de mur : `h` pour horizontal, `v` pour vertical\n> ";
            let rec wall () = match read_line () with
              | "h" | "H" -> place_wall g x y H
              | "v" | "V" -> place_wall g x y V
              | _ -> print_string "Entrée invalide, réessayer.\n> "; wall ()
            in wall ()
            with Invalid_argument s -> Printf.printf "Entrée invalide : \"%s\", réessayer." s; ask_coords ()
          in ask_coords ()
        | "h" | "H" -> move g p U
        | "b" | "B" -> move g p D
        | "g" | "G" -> move g p L
        | "d" | "D" -> move g p R
        | _ -> print_string "Entrée invalide, réessayer."; ask_user ()
      with
      | Wrong_move -> print_string "Déplacement impossible, réessayer."; ask_user ()
      | Obstructed -> print_string "Placement du mur impossible, réessayer."; ask_user ()
    in ask_user ();
    if check_winner g p then Printf.printf "Bravo, le joueur %s a gagné !" (char_of_player p)
    else game_loop (next_p p nb_p)
  in game_loop 0

let () = start ()
