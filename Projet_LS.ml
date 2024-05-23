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
let rec string_of_formula = function
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
  | Var _ -> [formula]
  | Non f -> vars f
  | Et (f1, f2) -> vars(f1) @ vars(f2)
  | Ou (f1,f2) -> vars(f1) @ vars(f2)
  | Alors (f1,f2) -> vars(f1) @ vars(f2)
;;

vars(exemple1);; (* ["Q"; "P"; "M"] *)
                 
(* Question 3 *)

let drop var =
  List.sort_uniq (fun (Var x) (Var y) -> compare x y) var
;;
  
let input_list = [Var "x"; Var "y"; Var "z"; Var "x"; Var "y"; Var "w"; Var "z"];;

drop input_list;;

let set_vars formula =
  drop (vars formula) 
;;

(* Question 4 *)

let enumerate_cases 
    


      
                                     



                              
                              

