(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

(* Exercice 1.1.2 *)
(* Question 2 *)

type tformula =
  | Var of string
  | Non of tformula
  | Et of tformula * tformula
  | Ou of tformula * tformula
  | Alors of tformula * tformula
;;

(* Question 3 *)

(* pour cette fonction nous avons utilisé les symboles ∨ et ∧  car le backslash sort automatiquement du string  *)
let rec string_of_formula f =
  match f with
  | Var v -> v
  | Non f -> "~" ^ (string_of_formula f)
  | Et (f1, f2) -> "(" ^ (string_of_formula f1) ^ " ∧ " ^ (string_of_formula f2) ^ ")"
  | Ou (f1, f2) -> "(" ^ (string_of_formula f1) ^ " ∨ " ^ (string_of_formula f2) ^ ")"
  | Alors (f1, f2) -> "(" ^ (string_of_formula f1) ^ " ==> " ^ (string_of_formula f2) ^ ")"
;;

let p = Var "P";;  (* J'ai mon parapluie *)
let q = Var "Q";;  (* Il pleut *)
let r = Var "R";;  (* Je suis fatigué *)
let m = Var "M";;  (* Je suis mouillé *)  
                   
let exemple1 = Alors (Et (q, p), Non m);; (* Il pleut et j'ai mon parapluie alors je ne suis pas mouillé. *)
let exemple2 = Alors (Non (q), Non (m));; (* Il ne pleut pas alors je ne suis pas mouillé. *)
let exemple3 = Alors (Et (Non (p), q), m);; (* Je n'ai pas mon parapluie et il pleut alors je suis mouillé. *)
let exemple4 = Alors (Ou (q, Non (q)), p);; (* Aujourd'hui il pleut ou il ne pleut pas alors j'ai pris mon parapluie. *)
let exemple5 = Alors (p, Et (Non (m), Non (r)));; (* J'ai mon parapluie alors je ne suis pas mouillé et je ne suis pas fatigué. *)
                                           
string_of_formula(exemple1);; (* Q /\ P ==> ~M *) 
string_of_formula(exemple2);; (* ~Q ==> ~M *) 
string_of_formula(exemple3);; (* ~P /\ Q ==> M *) 
string_of_formula(exemple4);; (* Q \/ ~Q ==> P *) 
string_of_formula(exemple5);; (* P ==> ~M /\ ~R *) 
                              
(* 2 *)
(* 2.1 *)
(* Question 1 *)

type valuation = (string * bool) list;;

(* Question 2 *)

let rec give_value valuation var_name =
  match valuation with
  | [] -> failwith ("Variable " ^ var_name ^ " not found in valuation")
  | (var, value) :: rest -> 
      if var = var_name 
      then value 
      else give_value rest var_name
;; 
   
(* Question 3 *)

let valuation = [("P", true); ("Q", false); ("R", true); ("M", false)]

let rec eval formula valuation = 
  match formula with
  | Var v -> give_value valuation v
  | Non f -> not (eval f valuation)
  | Et (f1, f2) -> (eval f1 valuation) && (eval f2 valuation)
  | Ou (f1, f2) -> (eval f1 valuation) || (eval f2 valuation)
  | Alors (f1, f2) -> not (eval f1 valuation) || (eval f2 valuation)
;;

(* Question 4 *)


let res1 = eval exemple1 valuation;; (* true *)
let res2 = eval exemple2 valuation;; (* true *)                                   
let res3 = eval exemple3 valuation;; (* true *)
let res4 = eval exemple4 valuation;; (* true *)
let res5 = eval exemple5 valuation;; (* false *)    




(* 2.2 *)
(* Question 2 *)

let rec vars formula =
  match formula with
  | Var f -> [f]
  | Non f -> vars f
  | Et (f1, f2) -> vars(f1) @ vars(f2)
  | Ou (f1,f2) -> vars(f1) @ vars(f2)
  | Alors (f1,f2) -> vars(f1) @ vars(f2)
;;

vars(exemple1);; (* ["Q"; "P"; "M"] *)
                 
(* Question 3 *)

let drop var =
  List.sort_uniq (fun (x : string) (y : string) -> compare x y) var
;;
  
let input_list = ["x"; "y";  "z";  "x";  "y";  "w";  "z"];;

drop input_list;;

let set_vars formula =
  drop (vars formula) 
;;

(* Question 4 *)
(* Notre fonction enumerate_cases_aux nous permet de calculer toutes les combinaisons possibles de valeurs de vérité*)
(* Elle est récursive et elle ajoute des valeurs 'true' et 'false' alternativement *)
let rec enumerate_cases_aux n =
  if n = 0 then [[]]
  else
    let rest = enumerate_cases_aux (n - 1) in
    List.map (fun vals -> true :: vals) rest @ List.map (fun vals -> false :: vals) rest
;;

(* Notre fonction enumerate_cases est récursive, elle utilise les valeurs générées par enumerate_cases_aux afin *)
(* de remplir ligne par ligne la table de vérité finale *)
let rec enumerate_cases l =
  let res = enumerate_cases_aux (List.length l) in
  let rec build_case values =
    match values with
    | [] -> []
    | hd :: tl ->
        if hd then "V" :: build_case tl
        else "F" :: build_case tl
  in
  List.map build_case res
;;

(* Pour savoir le nombre de lignes à calculer il faut prendre 2^n avec n le nombre d'argument dans la liste en entrée*)
(* Donc pour trois variables P,Q et R on aura 2^3 donc 8 lignes*)

(* Tests de nos exemples vu plus haut dans le code *)

enumerate_cases(set_vars exemple1);;
enumerate_cases(set_vars exemple2);;
enumerate_cases(set_vars exemple3);;
enumerate_cases(set_vars exemple4);;
enumerate_cases(set_vars exemple5);;

(* 2.3 *)
(* Question 2 *)

let tautology formule =
  let rec tautology_aux vars =
    match vars with
    | [] -> [[]]
    | v :: res ->
        let rest = tautology_aux res in
        List.concat [List.map (fun r -> (v, true) :: r) rest; List.map (fun r -> (v, false) :: r) rest]
  in
  let variables = List.sort_uniq compare (vars formule) in
  let valuations = tautology_aux variables in
  List.for_all (fun v -> eval formule v) valuations
;;

tautology (Alors (Var "P", Non(Non(Var "P"))));; (* P ==> ~(~P) - Renvoie True*)
tautology (Alors (Var "P", Non(Var "P")));; (* P ==> ~(P) - Renvoie False *)
tautology (Ou(Var "P", Non(Var "P")));; (* P \/ ~P - Renvoie True *)
tautology (Et(Var "P", Non(Var "P")));; (* P /\ ~P - Renvoie False *)
tautology (Alors (Var "Q", Ou(Var "P", Non(Var "P"))));; (* Q ==> (P \\/ ~P) - Renvoie True *)
tautology (Alors (Var "Q", Ou(Var "P", Var "P")));; (* Q ==> (P \/ P) - Renvoie False *)

(* 3.1 *)

(* Question 1 *)

type ident = string ;;

type goal = (ident * tformula);;

(* Question 2 *)

