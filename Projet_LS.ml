(* Lespagnol Killian / Tribouillois Julien *)

(* Nous précisons que les questions d'écrit simple qui ne comprennent pas de code sont présentent dans le ReadMe.md*)


(* Exercice 1.1.2 *)
(* Question 2 *)
(* Type tformula composé de tous les connecteurs logiques*)
type tformula =
  | Var of string
  | Non of tformula
  | Et of tformula * tformula
  | Ou of tformula * tformula
  | Alors of tformula * tformula
  | True
  | False
;;

(* Question 3 *)

(* Pour cette fonction nous avons utilisé les symboles ∨ et ∧  car le backslash sortent automatiquement du string  *)

let rec string_of_formula f =
  match f with
  | Var v -> v
  | Non f -> "~" ^ (string_of_formula f)
  | Et (f1, f2) -> "(" ^ (string_of_formula f1) ^ " ∧ " ^ (string_of_formula f2) ^ ")"
  | Ou (f1, f2) -> "(" ^ (string_of_formula f1) ^ " ∨ " ^ (string_of_formula f2) ^ ")"
  | Alors (f1, f2) -> "(" ^ (string_of_formula f1) ^ " ==> " ^ (string_of_formula f2) ^ ")"
;;
(* Déclaration de nos variables, correspondantent à nos exemples dans le ReadMe *)
let p = Var "P";;  (* J'ai mon parapluie *)
let q = Var "Q";;  (* Il pleut *)
let r = Var "R";;  (* Je suis fatigué *)
let m = Var "M";;  (* Je suis mouillé *)  
                   
(* Création d'exemples par rapport à nos phrases dans le ReadMe *)

let exemple1 = Alors (Et (q, p), Non m);; (* Il pleut et j'ai mon parapluie alors je ne suis pas mouillé. *)
let exemple2 = Alors (Non (q), Non (m));; (* Il ne pleut pas alors je ne suis pas mouillé. *)
let exemple3 = Alors (Et (Non (p), q), m);; (* Je n'ai pas mon parapluie et il pleut alors je suis mouillé. *)
let exemple4 = Alors (Ou (q, Non (q)), p);; (* Aujourd'hui il pleut ou il ne pleut pas alors j'ai pris mon parapluie. *)
let exemple5 = Alors (p, Et (Non (m), Non (r)));; (* J'ai mon parapluie alors je ne suis pas mouillé et je ne suis pas fatigué. *)
                          
(* Tests de la fonction string_of_formula avec nos exemples *)

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

(*  *)

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

(* Déclaration de résulats pour nos valuations *)

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
 
(* Création d'un test pour la fonction drop *)
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

(* La fonction tautology permet de savoir si une proposition est une tautologie par rapport à la description faite
   dans le Readme  *)

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

(* Test de la fonction tautology avec chaque exemple de tautologie fait avec un exemple false *)

tautology (Alors (Var "P", Non(Non(Var "P"))));; (* P ==> ~(~P) - Renvoie True*)
tautology (Alors (Var "P", Non(Var "P")));; (* P ==> ~(P) - Renvoie False *)
tautology (Ou(Var "P", Non(Var "P")));; (* P \/ ~P - Renvoie True *)
tautology (Et(Var "P", Non(Var "P")));; (* P /\ ~P - Renvoie False *)
tautology (Alors (Var "Q", Ou(Var "P", Non(Var "P"))));; (* Q ==> (P \\/ ~P) - Renvoie True *)
tautology (Alors (Var "Q", Ou(Var "P", Var "P")));; (* Q ==> (P \/ P) - Renvoie False *)

(* 3.1 *)

(* Question 1 *)
type ident = string;;
type goal = (ident * tformula) list * (tformula list);;

(* Question 2 *)

let print_goal goal = 
  let rec print_goal_aux goal =
  match goal with
  | ([], []) -> ""
  | ([], (header :: tail)) -> "======================\n" ^string_of_formula header ^ "\n" ^ (print_goal_aux([], tail))
  | ((header, formule) :: tail, f) -> header ^ ": " ^ string_of_formula formule ^ "\n" ^ (print_goal_aux (tail, f))
in 
print_string(print_goal_aux goal)

;;


(* Création d'un test venant de l'énoncé, pour le test de la fontion print_goal *)
let ex_enonce = (("H",Alors (Ou(Var "P",Var "Q"),Var "R")) :: [("H2",Var "P")], [Ou(Var "P",Var "Q")]);;
print_goal ex_enonce;;

(* 3.2 *)
(* Question 1 *)

(* Type concernant la tactique avec toutes les possibilités présentent dans l'énoncé*)
type tactic = 
  |And_Intro
  |Or_Intro_1
  |Or_Intro_2
  |Impl_Intro
  |Not_Intro
  |And_Elim_1 of ident
  |And_Elim_2 of ident
  |Or_Elim of ident 
  |Impl_Elim of ident * ident
  |Not_Elim of ident * ident
  |Exact of ident
  |Assume of tformula
;;

(* 3.3 *)
(* Question 1 *)

(* Les fonctions fresh_ident et fresh_aux permettent de donner un  identifiant non utilisé pour chaque hypothèse H*)

let rec fresh_aux (context, n : goal * int) : string=
  match context with
  |([], _) -> "H" ^ string_of_int n
  |(liste, _)-> 
      if (List.exists (fun (ident, _) -> ident = "H" ^ string_of_int n) liste) 
      then fresh_aux(context, n+1)
      else "H" ^ string_of_int n
;; 

let fresh_ident (context : goal) : string = 
  fresh_aux(context, 1)
;;

  (* Question 2 *)
(* La fonction valid_ident fait la gestion des identifiants d'hypothèse*)

let valid_ident (ident, context : ident * goal) : bool =
  match context with
  |([], _) -> true
  |(liste, _) -> not(List.exists (fun (n,_) -> n = ident) liste)
;;

(*Tests des fonctions fresh_ident et valid_ident avec notre exemple créé plus haut dans le code *)
fresh_ident ex_enonce;;
valid_ident(fresh_ident(ex_enonce), ex_enonce);;

  (* Question 3 *)
(* Création d'une exception en cas d'erreur inattendu *)

exception InvalidTactic

(* Cette fonction apply_tactic prend en compte tous les cas possibles 
   elle provoque des warnings concernant les exceptions InvalidTactic mais nous n'avons pas juger important de les traiter
   le code reste fonctionnel *)

let apply_tactic (tactique , (h,context) : tactic * goal) : goal list=
  match tactique with
  |And_Intro -> 
      (match context with
       | (Et(f1,f2) :: tail) -> [(h, f1 :: f2 :: tail)]
       | _ -> raise InvalidTactic)

  |Or_Intro_1 ->
      (match context with
       | (Ou(f1,f2) :: tail) -> [(h, f1 :: tail)]
       | _ -> raise InvalidTactic)

  |Or_Intro_2 ->
      (match context with
       | (Ou(f1,f2) :: tail) -> [(h, f2 :: tail)]
       | _ -> raise InvalidTactic)

  |Impl_Intro->
      (match context with
       | (Alors(f1,f2) :: tail) -> [((fresh_ident (h,context),f1)::h,f2 :: tail)]
       | _ -> raise InvalidTactic)

  |Not_Intro ->
      (match context with
       | (Non(f1) :: tail) -> [((fresh_ident (h,context),f1)::h, False :: tail)]
       | _ -> raise InvalidTactic)

  |And_Elim_1 identi ->
      (match List.find (fun (x,x1) -> x = identi) h with
       | (header,hypo) -> 
           (match hypo with
            | Et(f1,f2) -> [((fresh_ident(h,context),f1)::h,context)]
            | _ -> raise InvalidTactic)
       | _ -> raise InvalidTactic)

  |And_Elim_2 identi ->
      (match List.find (fun (x,x1) -> x = identi) h with
       | (header,hypo) -> 
           (match hypo with
            | Et(f1,f2) -> [((fresh_ident(h,context),f2)::h,context)]
            | _ -> raise InvalidTactic)
       | _ -> raise InvalidTactic) 

  |Or_Elim identi ->
      (match List.find (fun (x,x1) -> x = identi) h with
       | (header,hypo) -> 
           (match hypo with
            | Ou(f1,f2) -> 
              (((fresh_ident(h,context),f1))
              :: List.filter (fun (x,x1) -> x <> identi) h ,context) 
              :: [(((fresh_ident(h,context),f2))
              :: List.filter (fun (x,x1) -> x <> identi) h ,context)]
            | _ -> raise InvalidTactic)
       | _ -> raise InvalidTactic) 

  |Impl_Elim (i1,i2) ->
      (match List.find (fun (x,x1) -> x = i1) h,List.find (fun (x,x1) -> x = i2) h with
       | (header1,hypo1),(header2,hypo2) -> (match (hypo1,hypo2) with
           |(Alors(f1, f2), f3) ->  
              if(f1 = f3) 
              then [((fresh_ident(h, context), f2) :: h, context)]
              else raise InvalidTactic
           | _ -> raise InvalidTactic)
       | _ -> raise InvalidTactic)

  |Not_Elim (i1,i2) ->
      (match (List.find (fun (x,x1) -> x = i1) h,List.find(fun (x,x1) -> x = i2) h) with
       |(header1, hypo1),(header2, hypo2) -> (match (hypo1, hypo2) with
           |(f1,f2) -> 
              if(f1 = f2)
              then [((fresh_ident(h, context), False)::h, context)] 
              else raise InvalidTactic
           | _ -> raise InvalidTactic)
       | _ -> raise InvalidTactic)

  |Exact identi -> 
    (match (List.find (fun (x,x1) -> x = identi) h,context) with
      | ((i1,f1),f2::tail)->if(f1=f2)then [(h,tail)]else raise InvalidTactic
      | _ -> raise InvalidTactic)
    
  |Assume formule -> 
    [((fresh_ident(h,context),formule)::h,formule :: context)]
;;




(* Reprise de la proposition de l'enonce : (P \/ Q -> R) -> (P -> R) /\ (Q -> R) *)

let prop_Exemple_enonce = 
  [Alors (Alors (Ou (Var "P" , Var "Q"),Var "R"), Et (Alors (Var "P", Var "R"), Alors (Var "Q", Var "R")))]
;;


(* Reprise de toutes les étapes pour valider la Preuve de la proposition vu dans l'enonce 
   Dans le ReadMe on a montré la réponse faite via CoqIDE, et on peut voir que les étapes diffèrent parfois*)

let e1 = apply_tactic(Impl_Intro, ([] , prop_Exemple_enonce));;
let e2 = apply_tactic(And_Intro, List.hd e1);;
let e3 = apply_tactic(Impl_Intro, List.hd e2);;
let e4 = apply_tactic(Assume (Ou (Var "P", Var "Q")), List.hd e3);;
let e5 = apply_tactic(Impl_Elim("H1", "H3"), List.hd e4);;
let e6 = apply_tactic(Exact "H3", List.hd e5);;
let e7 = apply_tactic(Exact "H4", List.hd e6);;
let e8 = apply_tactic(Impl_Intro, List.hd e7);;
let e9 = apply_tactic(Exact "H4", List.hd e8);;


(*Nous précisons que tout le code est fonctionnel sur nos machines personnelles*)