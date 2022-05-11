(*********   TP2   *********)

(*********   Exercice 1   *********)

let add predicat =
  fun str -> predicat^str
  ;;

add "in" "out";;

let add_list e1 e2 =
  fun l -> [e1]@[e2]@l
  ;;
add_list 2 3 []
;;


let invert predicat =
  fun l -> List.map predicat l
  ;;
  invert (fun x -> x*. -1.) [1.];;

  let inc  =
    (+) 1
    ;;
    inc 7
  ;;


(*********   Exercice 2   *********)

let rec inv_sens sens l1 =
  match l1 with
  |[] -> []
  |h1::t1 -> (sens h1)::(inv_sens sens t1) 
;;
  
let sens h = 
   h * -1
  ;;
  sens (45)
;; 

  inv_sens sens l;;

map sens [4; -2; 6; -99];;
  
let mult_scalaire h =
  3 * h
;;
  
map mult_scalaire  [4; -2; 6; -99];;
  
let sommeVect l1 l2 = List.map2 (+) l1 l2;;

sommeVect [2; 69; 35] [0; 0; 1];;


  (*********   Exercice 3   *********)

type 'a expression =
  | Cst of 'a
  | Var of char
  | Op of ('a -> 'a -> 'a) * 'a expression * 'a expression
;;

let one =
Op (( * ), Op ((+), Var ('x'), Cst (3)), Op ((-), Cst (2), Cst (8)));;

let rec eval_simple expression = 
  match expression with
  | Op ( op , left , right )  -> op ( eval_simple left ) ( eval_simple right )
  | Cst a  -> a
  | Var _ -> failwith "Not yet implement"
;;

let rd_tree random n =
  let rec aux n =
  if n = 0 
  then Nil
  else
  let nl = Random.int n in Node (random(), aux nl , aux(n - 1 - nl))
  in aux n 
;;
  
let rd_int (a, b) = a + Random.int (b - a + 1);;


let rd_tree_int (a, b) n = rd_tree (function ()  -> rd_int(a, b)) n;;

let rd_char (a, b) =
  let code_a = Char.code a and  code_b = Char.code b in
  Char.chr ( rd_int ( code_a , code_b + 1))
;;

let rd_char_tree ( a , b ) = rd_tree ( function ()  -> rd_char ( a , b ))
;;

let non a = function x -> not(x);;
let et a b = function x -> a x && b x;;
let ou a b = function x -> a x || b x;;

let teste = 
  let pair x = (x mod  2 = 0) and positif x = x > 0 in
  ou(et pair positif) (et (non pair) (non positif)) 
;;


let rd_val_list rd min max =
  let len = rd_int(min, max) in
  let rec aux rd mi mx l len=
    if len = 0
    then l
    else let r = rd min max  in
    aux rd mi mx (r::l) (len-1)
    in aux rd min max [] len
;;



let rec build_list f n = 
  if n = 0
  then []
  else f()::build_list f (n-1)
;;

let l = rd_val_list (fun a b -> a + Random.int ( b - a + 1)) 0 100;;

let rd_tree_v2 rd nb_node min max =
  let rec aux n =
    if n = 0 
    then Empty
    else
    let nl = Random.int n in Node ( rd_val_list rd min max, aux nl , aux( n - 1 - nl ))
    in aux nb_node
;;
  
let  abr = rd_tree_v2 (fun a b -> a + Random.int ( b - a + 1)) 6 10;;
  let a abr = 
    match abr with
    |Empty -> []
    |Node(r, ls, rs) -> r
;;
  
a abr;;