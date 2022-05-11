(*********   TP3   *********)

(*Ex1*)

(* Q1 *)

let rec build op l1 l2 = 
  match (l1,l2) with
  | ([],[]) -> []
  | (hd1::tl1, hd2::tl2) -> let hd = (op hd1 hd2) in
  hd::build op tl1 tl2 
  | _ -> failwith("les deux listes n'ont pas la même longeur")
  
;;

let l1 = [53; 6; -10; 3; 16; 256; 6; 85; 6];;
let l2 = [41; -3; 55; 4; 512; 1024];;
let l3 = [9];;
let l4 = ["salut"; "je"; "m'appelle"; "guillaume"; "j'ai" ;"51"; "ans"];;
let l5 = ['t'; '3'; 'p'; 'A'; '0'; '8';'e'; 'e'; 'a'];;

build (+) l1 l2;;

(* Q2 *)
List.map2 (+) l3 l2;;

(* Q3 *)
let build_max l1 l2 =
  List.map2 (max) l1 l2
  ;;

build_max l1 l2;;

(***** EX 2 *******)

(* Q1 *)
List.map (fun a -> a *2) l1;;

(* Q2 *)
List.map (fun a -> String.capitalize_ascii a) l4;;

(* Q3 *)
List.fold_left (fun count a -> if a mod 2 = 0 then a + count else a ) 0 l1;;

(* Q4 *)
List.fold_left (fun count a -> 
  if int_of_char a >= 48 && int_of_char a <= 57 
  then count +1
  else count) 0 l5
;;

(* Q5 *)
List.fold_left (fun count a -> 
  if (int_of_char a >= 97 && int_of_char a <= 122) || (int_of_char a >= 65 && int_of_char a <= 90)
  then count +1
  else count) 0 l5
;;

(* Q6 *)
List.fold_left (fun count a -> (String.length a) + count ) 0 l4;;

(***** EX 3 *******)

(* Q1 *)
let rec areHere predicat l =
  List.fold_left (fun lbool a -> if predicat a then true::lbool else lbool ) [] l
;;

areHere (fun a -> a mod 2 =0  ) l1;;

(* Q2 *)
let rec areHereV2 predicat l =
  List.fold_left (fun lval a -> if predicat a then a::lval else lval ) [] l
;;

areHereV2 (fun a -> a mod 2 =0  ) l2;;

let l6 = [[1; 2; 3]; [4; 5]; [6; 7; 8]];;
let l7 = [(fun a -> a/2); (fun a -> a*2); (fun a -> a+2)];;



(***** EX 4 *******)

(* Q1 *)
let map_fun l v =
  List.fold_left (fun l f -> l@[f v]) [] l
  ;;


  map_fun l7 1;;
let occ_a = occ 'a' ;;
let occ_e = occ 'e' ;;

(* Q2 *)
let occ v l =
  List.fold_left (fun c a -> if a = v then c+1 else c) 0 l
;;

let occV2 (l : 'a list) : ('a list -> int) list =
  List.map (fun x -> occ x) l
  ;;
  (*List.fold_left (fun res a -> res@[List.fold_left (fun cc v -> if a = v then cc+1 else cc) 0 l] ) [] lv*)

let voyelles = ['a'; 'e'; 'i'];;

map_fun (occV2 voyelles) l5;;

occ l1 6;;


(***** EX 5 *******)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree;;

(* Q1 *)
let rec map func tree = 
  match tree with 
   | Nil -> Nil 
   | Node(root, left_tree, right_tree) -> 
   Node(func root, map func left_tree, map func right_tree) 
;;    

(* Q2 *)
let abr = Node(5, 
Node(7, Nil, Node(9, Node(10, Nil, Nil), Node(30, Nil, Nil))), 
Node(50, Nil, Node(25, Node(35, Node(65, Nil, Nil), Node(75, Node(10, Nil, Nil), Nil)), Nil)));;


map (fun x -> Random.int(100)) abr;;
map (fun x -> Random.int(10)) (rd_tree_int (10, 1000) 20);;

let rec create_list n =
  if n = 0 
  then []
  else Random.int(n)::(create_list (n-1))
;;

create_list 5;;

map (fun c -> create_list c) abr;;
  
  
let rec eval_expr (f_cst : 'a -> 'b) (f_var : char -> 'b) (f_op : ('a->'a->'a) -> 'b->'b->'b) (exp: 'a expression) : 'b =
  match exp with
  | Cst(x) -> f_cst x
  | Var(x) -> f_var x
  | Op(ope, exp1, exp2) -> (f_op ope) (eval_expr f_cst f_var f_op exp1) (eval_expr f_cst f_var f_op exp2)
;;

eval_expr (fun x -> Cst(x)) (fun x -> Var(x))
  (fun operator left_expr right_expr ->
    match (left_expr, right_expr) with
      (Cst(left_val), Cst(right_val)) -> Cst(operator left_val right_val)
      | _ -> Op(operator, left_expr, right_expr))
      ( Op (( * ), Op (( + ), Var 'x', Cst 3), Op (( - ), Cst 2, Cst 8)))
;;

let isVar = eval_expr (fun x -> false) (fun x -> true) (fun _ lexpr rexpr -> lexpr || rexpr);;

isVar (Op (( * ), Op ((+), Var ('x'), Cst (3)), Op ((-), Cst (2), Cst (8))));;


let errVar = 
  eval_expr (fun x -> Cst(x)) (fun x -> failwith"Variable d'étecter")
  (fun operator left_expr right_expr ->
  match (left_expr, right_expr) with
  (Cst(left_val), Cst(right_val)) -> Cst(operator left_val right_val)
  | _ -> Op(operator, left_expr, right_expr) )
;;

errVar (Op (( * ), Op ((+), Cst (6), Cst (3)), Op ((-), Cst (2), Cst (8))));;

let gtree: 'a abrNaires = Node(7, [ Node(8, []); Node(6, [Node(3, []); Node(33, [])]); Node(100, [])]);;
  let rec map_gtree (f: 'a -> 'b) (gt : 'a abrNaires) : 'b abrNaires =
  match gt with
  | Node(v, sons) -> Node( f v, (List.map (map_gtree f) sons));;

map_gtree (fun x -> if x mod 2 =0 then x +100 else x -100) gtree;;


let rec fold_left_gtree (f:'a -> 'b abrNaires -> 'a) (acc:'a) (gt:'a abrNaires) : 'a =
  match gt with
  | Node(v, sons) ->  f ( List.fold_left (fold_left_gtree f) acc sons) gt
;;

fold_left_gtree (fun c gt -> match gt with | Node(v, sons) -> if v mod 2 = 0 then c + 1 else c) 0 gtree;;