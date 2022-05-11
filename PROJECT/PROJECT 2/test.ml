(* Membre du groupe :
                        - Guillaume Pierre
                        - Valdrin Salihi
                        - Youssouf Ousmanou oumarou
*)


(* Question 1 *)

(* Définition des types du langage mini-ML *)
type myType =
  |TUnit
  |TInt
  |TBool
  |TChar
  |Tpair of (myType * myType)
  |Tfun of (myType * myType)
;;

(* Définition des expressions du langage *)
type expression =
  | Name of string
  | Int of int
  | Bool of bool 
  | Char of char
  | Primitive of string
  | Epair of (expression * expression)
  | Efun of myType * expression
  | App of expression * expression
  | Let of string * expression * expression 
;;


(* Question 2 *)

(* Fonction d'affichage des différentes expression présentes dans le mini-ML *)
let rec print_exp exp = 
  match exp with
  |Name n -> n
  |Eprimitive p -> p
  |Epair (p1, p2) -> "("^print_exp p1^ "," ^print_exp p2^ ")"
  |Efun (t, e) -> "fun " ^n^ " : " ^ print_exp t ^ " -> " ^ print_exp e^
  |App (e1, e2) -> print_exp e1 ^ " " ^ print_exp e2
  |Let(n, e1, e2) -> "let " ^n^ " = " ^ print_exp e1 ^ " in " ^ print_exp e2^";;" 
;;

let test = Elet("vitesse", Eprimitive("5"), Eprimitive("9"));;
print_exp test;;


(* Question 3 *)

(* Différents environnements composant le langage mini-ML *)
let envconstEnt  = [(1, "int"); (2, "int"); (3, "int"); (4, "int"); (5, "int")];;
let envConstChar = [('a', "char"); ('b', "char"); ('c', "char"); ('d', "char")];;
let envConstBool = [(true, "bool"); (false, "bool")];;
let envOperations = [("+", "int * int -> int"); ("-", "int * int -> int"); ("<", "int * int -> bool"); (">", "int * int -> bool"); ("if_then_else", "bool * (int * int) -> int") ];;

let (a,b) = List.hd envOperations;;


(* Question 4 *)

(* Fonction auxiliaire posant les règles prévu d'inférence monomorphe *)
let rec expTypeString exp =
  match exp with
  | Name s -> s
  | Int i -> "int"
  | Bool b -> "bool"
  | Char c -> "char"
  | Primitive p-> p
  | Epair (exp1, exp2) -> expTypeString exp1 ^" * "^ expTypeString exp2
  | Efun (typ, exp)-> myTypeString typ ^" -> "^ expTypeString exp
  | App (exp1, exp2) -> if expTypeString exp1 = expTypeString exp2 then expTypeString exp2 else failwith"problème de type"
  | Let (n, exp1, exp2) -> "string * "^expTypeString exp1 ^" * "^ expTypeString exp2 
;;

(* Idem *)
let rec myTypeString t =
  match t with
  |TUnit -> "unit"
  |TInt -> "int"
  |TBool -> "bool"
  |TChar -> "char"
  |Tpair (myType1, myType2) -> myTypeString myType1 ^" * "^ myTypeString myType2
  |Tfun (myType1, myType2) -> myTypeString myType1^" -> "^ myTypeString myType2
;;

let exptest1 = Efun(Tpair(TInt, TInt) , App(Int(4), Int(4)) );;

(expType exptest1) = b;;

(* Fonction permettant la vérification de chacun des types de l'environnement et de l'expression présent en argument *)
let rec checkType env exp =
  match env with
  |[] -> failwith"Inconue"
  |(v,t)::tl -> if (expTypeString exp) = t  
                then expTypeString exp 
                else checkType tl exp 
;;

#trace checkType;;

checkType envOperations exptest1;;

(* Question 5 *)