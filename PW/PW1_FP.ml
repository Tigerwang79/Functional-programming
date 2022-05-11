(*********   TP1  *********)

(*****    EX1    *****)

(* Q1 *)
let rec countMult count l =
  match l with
  |[] -> count
  |h::t -> if  h mod 5 = 0
           then countMult (count+1) t
           else countMult count t
;;

let rec countMultV2 count l =
  match l with
  |[] -> count
  |h::t when h mod 5 = 0 -> countMult (count+1) t
  |h::t when h mod 5 != 0 -> countMult count t
  ;;

let l = [5;10;3;1;15];;

countMultV2 0 l;;

(* Q2 *)


(* Q3 *)

let rec cut l lp li =
  match l with
  |[] -> (lp, li)
  |h::t when h mod 2 = 0 -> cut t (h::lp) li
  |h::t when h mod 2 != 0 -> cut t lp (h::li)
  ;;

cut l [] [];;

(*****    EX2    *****)

(* Q1 *)
type 'a abr = Empty | Node of 'a * 'a abr * 'a abr
;;

let ex = Node('b', Node('h', Node('s', Node('q', Empty, Empty), Node('p', Empty, Empty)), Node('k', Empty, Empty)), Node('r', Node('c', Empty, Empty), Node('j', Node('o',Empty, Empty), Empty)));;

(* Q2 *)
let rec abrToList abr =
  match abr with
  |Empty -> []
  | Node(r, ls, rs) -> 
  let listLS = abrToList ls in
  let listRS = abrToList rs in 
  listLS@[r]@listRS
;;

abrToList ex;;

(* Q3 *)
let rec treeMax abr max =
  match abr with
  |Empty -> []
  |Node(r, Empty, Empty) -> if r > max 
                            then [r] 
                            else [max]
  |Node(r, ls, rs) -> if r > max 
                      then treeMax ls r @ treeMax rs r
                      else treeMax ls max @ treeMax rs max
;;

let maxList abr =
  match abr with
  |Empty -> []
  |Node(r, _, _) -> treeMax abr r
  ;;

  maxList ex;;






(*****    EX3    *****)

(* Q1 *)
type 'a bltree = Leaf of 'a |Node of 'a * 'a bltree * 'a bltree;; 

(* Q2 *)
let rec checkWay abr l =
  match l with
  | [] -> failwith"La liste vide ne peut pas être la suite des étiquettes d'un branche"
  | [last] -> (match abr with 
    | Leaf x when x = last -> true
    | _ -> false)
  | head::tail -> (match abr with 
    | Node(x, left, right) when x = head -> 
                           if checkWay left tail 
                           then true 
                           else checkWay right tail 
    | _ -> false)
  ;;

let aa = Node('b', Node('h', Node('s', Leaf('q'), Leaf('p')), Leaf('k')), Node('r', Leaf('c'), 
  Node('j', Leaf('o'), Leaf('t'))))
;;

#trace checkWay;;
checkWay aa ['b'; 'h'; 's';'z'];;

(* Q3 *)
type 'a abrNaires =  Node of 'a * 'a forest and 'a forest = 'a abrNaires list;;

let an = Node('a', 
  [ Node('b', []);Node('d', 
    [ Node('k', 
      [ Node('s', 
        [ Node('m', []);Node('r', []) ])  ])  ;Node('n',[])]) ])  ;;

(* Q4 *)
let rec wayNaires abr l =
  match l with
  |[] -> true
  |h::t ->(match abr with
          |Node(r, f) -> if h = r 
                         then wayForest f t 
                         else false
          |_ -> false) 
  and wayForest f l =
      match (f, l) with
      |([], []) -> true
      |([], _) -> false 
      |(hd::tl, _) -> wayNaires hd l || wayForest tl l
;;

#trace wayNaires;;
wayNaires an ['a'; 'd'; 'k';'s'];;

(* Q5 *)
type fileRepoH = Node of string * string;;

(* Q6 *)
let rec isFile f l =
match l with
|[] -> false
|hd::tl -> ( match hd with
          |Node(r, _) -> if f = r then true else isFile f tl
          )
;;

let re = "TP.ML";;

let er = [Node("TP", "A3");Node("ML", "P3");Node("TP.ML", "AP3");Node("ML", "P3");Node("TML", "A3")];;

isFile re er;;

let inc n = n +1;;

inc 4;;










(*****    EX4    *****)

type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree;;

let my_abr = Node(
  Node(
    Node(Leaf(5), Leaf(9)), Leaf(15)
    ),
  Node(
    Node(Leaf(2), Node(Leaf(17), Leaf(8))), 
    
    Node(Leaf(3), Leaf(25) ))
);;

let rec btree_to_list abr bro l =
  match abr with
  |Leaf(r) -> btl_bro bro [r]@l
  |Node(ls, rs) -> btree_to_list ls rs l 
  and btl_bro b l= 
  match b with 
  |Leaf(r) -> btl_bro b [r]@l
  |Node(ls, rs) -> btree_to_list ls rs l
  ;;
  #trace btree_to_list;; 
  btree_to_list my_abr empty [];;
  let rec btree_to_list abr =
    match abr with
    |Node(Leaf(ls), rs) -> [ls]@btree_to_list rs
    |Node(ls, Leaf(rs)) -> [rs]@btree_to_list ls
    |Node(rs, ls) -> btl_bro ls rs
    and btl_bro l r =

    ;;
let empty = Node(Leaf(0), Leaf(0));;

btree_to_list my_abr;;