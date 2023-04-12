let print_list lst =
  List.iter (fun x -> print_endline x) lst;;


let prenom_nom_to_string prenom nom =
  String.lowercase_ascii (prenom ^ nom);;



let load_names filename =
  let names = ref [] in
  let file = open_in filename in
  try
    while true do
      let name = input_line file in
      names := name :: !names
    done;
    !names
  with End_of_file ->
    close_in file;
    List.rev !names;;



let same_letters s1 s2 =
  (* convertit les deux chaînes en listes de caractères *)
  let l1 = List.sort Char.compare (String.to_seq s1 |> List.of_seq) in
  let l2 = List.sort Char.compare (String.to_seq s2 |> List.of_seq) in
  (* compare les deux listes de caractères *)
  l1 = l2;;


let rec all_combinations str =
  match str with
  | "" -> [""]
  | _ ->
    let rest = all_combinations (String.sub str 1 (String.length str - 1)) in
    let first = String.sub str 0 1 in
    let with_first = List.map (fun s -> first ^ s) rest in
    let without_first = List.map (fun s -> s) rest in
    with_first @ without_first;;


let lst_prenoms : string list ref = ref [];;  (* On utilise une référence pour modifier la liste lst_prenoms *)
let lst_lettres_restantes : string list ref = ref [];;
let lst_noms : string list ref = ref [];;

let check_letters lettres lst =
  List.iter (fun prenom ->
    List.iter (fun ler -> 
      if same_letters prenom ler then
        lst_prenoms := prenom :: !lst_prenoms  (* On ajoute le prénom à lst_prenoms s'ils ont les mêmes lettres *)
    ) (all_combinations lettres)
  ) lst;;


let prenom_possible str lst =
  let n = String.length str in
  for i = 1 to n do
    check_letters (String.sub str 0 i) lst
  done;
  !lst_prenoms;; (* On retourne la liste lst_prenoms modifiée *)

let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | x::xs -> x :: remove_duplicates (List.filter (fun y -> y <> x) xs);;


let compter_occurrences c str =
  let rec loop i count =
    if i >= String.length str then count
    else if String.get str i = c then loop (i+1) (count+1)
    else loop (i+1) count
  in loop 0 0;;

let rec lettres_non_utilisees str1 str2 =
  match str1 with
  | "" -> ""
  | _ ->
      let c = String.get str1 0 in
      let count1 = compter_occurrences c str1 in
      let count2 = compter_occurrences c str2 in
      if count1 <= count2 then
        lettres_non_utilisees (String.sub str1 1 (String.length str1 - 1)) str2
      else
        Char.escaped c ^ lettres_non_utilisees (String.sub str1 1 (String.length str1 - 1)) str2;;


let appliquer_chaine str lst =
  List.map (fun x -> lettres_non_utilisees str x) lst








(* fonction pour lire les lignes d'un fichier et les ajouter à une liste *)
let read_lines file_name =
  let in_channel = open_in file_name in
  let rec read_lines_aux acc =
    try
      let line = input_line in_channel in
      read_lines_aux (line :: acc)
    with End_of_file ->
      close_in in_channel;
      List.rev acc
  in
  read_lines_aux []

(* fonction pour ajouter les lettres virtuelles de début et de fin à chaque prénom *)
let add_virtual_letters prenoms =
  List.map (fun prenom -> "^" ^ prenom ^ "$") prenoms

(* fonction pour calculer les comptages des paires de lettres *)
let count_letter_pairs prenoms_with_virtual_letters =
  let count_pairs = Hashtbl.create 256 in
  let update_count a b =
    let key = (a, b) in
    let count = try Hashtbl.find count_pairs key with Not_found -> 0 in
    Hashtbl.replace count_pairs key (count + 1)
  in
  List.iter (fun prenom ->
    let len = String.length prenom in
    for i = 0 to len - 2 do
      update_count prenom.[i] prenom.[i+1]
    done
  ) prenoms_with_virtual_letters;
  count_pairs

(* fonction pour calculer les probabilités de transition *)
let calc_transition_probabilities count_pairs =
  let total_counts = Hashtbl.create 64 in
  let increment_total key count acc =
    let a, _ = key in
    let total = try Hashtbl.find total_counts a with Not_found -> 0 in
    Hashtbl.replace total_counts a (total + count)
  in
  let () = Hashtbl.fold increment_total count_pairs () in

  let transition_probabilities = Hashtbl.create 256 in
  let calc_probability key count =
    let a, b = key in
    let total = Hashtbl.find total_counts a in
    let probability = float_of_int count /. float_of_int total in
    Hashtbl.replace transition_probabilities (a, b) probability
  in
  Hashtbl.iter calc_probability count_pairs;
  transition_probabilities


(* fonction pour sélectionner une lettre en fonction des probabilités de transition *)
let select_next_letter current_letter transition_probabilities =
  let candidates = Hashtbl.fold
    (fun (a, b) p acc -> if a = current_letter then (b, p) :: acc else acc)
    transition_probabilities []
  in
  let sorted_candidates = List.sort (fun (_, p1) (_, p2) -> compare p2 p1) candidates in
  let total_probability = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 sorted_candidates in
  let rec choose_letter candidates threshold =
    match candidates with
    | (letter, p) :: rest ->
      if threshold <= p then letter
      else choose_letter rest (threshold -. p)
    | [] -> failwith "No candidates found"
  in
  choose_letter sorted_candidates (Random.float total_probability)

(* fonction pour générer un nom plausible en utilisant les probabilités de transition *)
let generate_plausible_name transition_probabilities =
  let rec generate_name current_letter acc =
    let next_letter = select_next_letter current_letter transition_probabilities in
    if next_letter = '$' then acc
    else generate_name next_letter (acc ^ (String.make 1 next_letter))
  in
  generate_name '^' ""


(* fonction pour calculer toutes les permutations d'une liste *)
let rec permutations = function
  | [] -> [[]]
  | xs ->
    let rec insert_all_positions x = function
      | [] -> [[x]]
      | (y :: ys) as l -> (x :: l) :: List.map (fun r -> y :: r) (insert_all_positions x ys)
    in
    List.concat (List.map (fun x -> insert_all_positions x (List.filter ((<>) x) xs)) xs)

(* fonction pour calculer la probabilité d'une chaîne de caractères en fonction des probabilités de transition *)
let calc_string_probability s transition_probabilities =
  let len = String.length s in
  let rec calc_probability_aux i acc =
    if i >= len - 1 then acc
    else
      let a = s.[i] in
      let b = s.[i + 1] in
      let p = try Hashtbl.find transition_probabilities (a, b) with Not_found -> 0.0 in
      calc_probability_aux (i + 1) (acc *. p)
  in
  calc_probability_aux 0 1.0

(* fonction pour générer un nom plausible en utilisant les lettres données *)
let generate_plausible_name_using_given_letters transition_probabilities letters =
  let letters_list = List.init (String.length letters) (String.get letters) in
  let letter_permutations = List.map (String.concat "") (List.map (List.map (String.make 1)) (permutations letters_list)) in
  let with_probabilities = List.map (fun s -> (s, calc_string_probability s transition_probabilities)) letter_permutations in
  let sorted_permutations = List.sort (fun (_, p1) (_, p2) -> compare p2 p1) with_probabilities in
  match sorted_permutations with
  | (best_name, _) :: _ -> best_name
  | [] -> failwith "No permutations found"



(* fonction pour générer des noms plausibles en utilisant les lettres données pour chaque chaîne de caractères de la liste *)
let generate_plausible_names_using_given_letters_list transition_probabilities given_letters_list =
  List.map (generate_plausible_name_using_given_letters transition_probabilities) given_letters_list


let rec concatene_avec_espace l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | t1 :: q1, t2 :: q2 ->
      print_endline (t1 ^ " " ^ t2);
      concatene_avec_espace q1 q2
  | _ -> raise (Invalid_argument "Les listes doivent être de la même longueur")

let () =
  Random.self_init ();
  let dictionnaire = load_names "data.txt" in
  let str = "jeanveronis" in
  let noms = prenom_possible str dictionnaire in
  let lst = remove_duplicates noms in
  let lst_lettres_restantes = appliquer_chaine str lst in


  let file_name = "data.txt" in
  let prenoms = read_lines file_name in
  let prenoms_with_virtual_letters = add_virtual_letters prenoms in
  let count_pairs = count_letter_pairs prenoms_with_virtual_letters in
  let transition_probabilities = calc_transition_probabilities count_pairs in
  let plausible_names = generate_plausible_names_using_given_letters_list transition_probabilities lst_lettres_restantes in
  concatene_avec_espace lst plausible_names

  