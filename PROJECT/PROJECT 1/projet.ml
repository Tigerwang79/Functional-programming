#directory "ocaml-4.02.1+ocp1";;
#directory "ocaml-4.08.1";;
#load "expression_scanner.cmo";;
open Expression_scanner;;
#show Expression_scanner;;


(*Analyse syntaxique et constrution de l'arbre*)

type operator = |Plus |Moins |Mult |Div ;;
type tree =
  |Var of char
  |Cst of int
  |Unary of tree
  |Binary of operator * tree * tree
;;



let rec is_correct list = (*Verifie si l'expression est correct si end a la fin*)
  match list with
    [] -> false
   |hd::tail -> if hd = End && tail = []
                then true
                else
                  if hd != End && tail = []
                  then false
                  else is_correct tail
;;

(*met les tokens dans l'arbre et renvoie l'arbre *)
let add_token_to_tree (list, token) =
  if token = Minus
  then
    match list with
      [] -> failwith("add_token_to_tree : list vide")
     |hd::tail -> Unary(hd)::tail
  else
    match list with
      []-> failwith("add_token_to_tree : liste vide")
    | hd::[] ->  failwith ("Erreur : erreur de syntaxe")
    |hd::hd2::tail ->  match token with
                         Add ->  Binary(Plus, hd, hd2)::tail
                        |Subtract -> Binary(Moins, hd, hd2)::tail
                        |Multiply -> Binary(Mult, hd, hd2)::tail
                        |Divide -> Binary(Div, hd, hd2)::tail
                        |_-> failwith("add_token_to_tree : token invalid")
;;

let tree_of_list list =
  match list with
    []-> failwith("tree_of_list : liste vide")
   |hd::tail -> hd
;;


(* prend une liste et cree l'arbre en fonction des info de la liste *)
(*add_token_to_list sert a mettre les tokens dans l'arbre*)
let parse list  = 
  if is_correct (list)
  then
    let rec parse_aux (list, tree_list) =
      match list with
        [] -> tree_of_list(tree_list)
       |hd::tail -> match hd with
                      End -> tree_of_list(tree_list)
                     |Variable(x) -> parse_aux(tail, Var(x)::tree_list)
                     |Number(x)   -> parse_aux(tail, Cst(x)::tree_list)
                     |_           -> parse_aux(tail, add_token_to_tree(tree_list, hd))
  in
  parse_aux (list, [])
  else
    failwith("Fatal error : expression invalid")
;;


(*affichage de l'affichage de l'arbre*)

let show_no_simplify tree  = 
  let rec show_no_simplify_aux tree =
  match tree with 
	Var(x) -> print_char x
	|Cst(x) -> print_int x
	|Unary(tree) ->  print_string("- "); show_no_simplify_aux (tree)
	|Binary(op, lson, rson) ->
          print_string("(");
          show_no_simplify_aux(rson);
          match op with 
	    Plus ->  print_string(" + ");show_no_simplify_aux(lson);
                   print_string(")")
	   |Moins -> print_string (" - ");show_no_simplify_aux(lson);
                   print_string(")")
	   |Mult -> print_string (" * ");show_no_simplify_aux(lson);
                   print_string(")")
	   |Div -> print_string (" / ");show_no_simplify_aux(lson);
                   print_string(")")
  in
  match tree with
    Var(x) -> print_char x
   |Cst(x) -> print_int x
   |Unary(tree) ->  print_string("- "); show_no_simplify_aux (tree)
   |Binary(op,lson, rson) ->
     show_no_simplify_aux rson;
      match op with 
	    Plus ->  print_string(" + ");show_no_simplify_aux(lson);                     
	   |Moins -> print_string (" - ");show_no_simplify_aux(lson);                   
	   |Mult -> print_string (" * ");show_no_simplify_aux(lson);                   
	   |Div -> print_string (" / ");show_no_simplify_aux(lson);            
;;





(*simplification de l'arbre*)

let rec simplify tree =
  let tree_simplify = simp_aux tree in
  if tree_simplify = tree 
  then tree_simplify 
  else simplify tree_simplify
and simp_aux tree = 
  match tree with
    Var(x) -> Var(x)
   |Cst(x) -> Cst(x)
   |Unary(x) -> Unary(simplify x)
   |Binary(Plus, Cst(x), Cst(y)) -> Cst(y+x)
   |Binary(Moins, Cst(x), Cst(y)) -> Cst(y-x)
   |Binary(Mult, Cst(x), Cst(y)) -> Cst(x * y)
   |Binary(Div, Cst(x), Cst(y)) -> Cst(y/x)
                                 
   |Binary(Mult, Cst(1), rson) -> simplify rson
   |Binary(Mult, lson, Cst(1)) -> simplify lson
   |Binary(Plus, Cst(0), rson) -> simplify rson
   |Binary(Plus, lson, Cst(0)) -> simplify lson
                                
   |Binary(Mult, Cst(0), _) -> Cst(0)
   |Binary(Mult, _, Cst(0))-> Cst(0)
                            
   |Binary(Moins, lson, rson) -> if lson = rson then Cst(0) else Binary(Moins, simplify lson, simplify rson)
   |Binary(Div,lson, rson) -> if lson = rson then Cst(1) else Binary(Div, simplify lson, simplify rson)
                            
   |Binary(op, lson, rson) -> Binary(op, simplify lson, simplify rson)
;;


(*simplification parenthèse*)

let rec show_simplify tree  = 
  let rec show_simplify_aux (tree, operation) =
  match tree with 
	Var(x) -> print_char x
	|Cst(x) -> print_int x
	|Unary(tree) ->  print_string("- "); show_simplify (tree)
	|Binary(op, lson, rson) ->          
          match op with 
	    Plus ->  if operation != op
                     then (
                       print_string("(");
                       show_simplify_aux(rson, op);print_string(" + ");
                       show_simplify_aux(lson, op);
                       print_string(")")
                     )
                     else(
                       show_simplify_aux(rson, op);print_string(" + ");
                       show_simplify_aux(lson, op);
                     )
	   |Moins -> if operation != op
                     then (
                       print_string("(");
                       show_simplify_aux(rson, op);print_string(" - ");
                       show_simplify_aux(lson, op);
                       print_string(")")
                     )
                     else(
                       show_simplify_aux(rson, op);print_string(" - ");
                       show_simplify_aux(lson, op);
                     )
	   |Mult -> if operation != op
                     then (
                       print_string("(");
                       show_simplify_aux(rson, op);print_string(" * ");
                       show_simplify_aux(lson, op);
                       print_string(")")
                     )
                     else(
                       show_simplify_aux(rson, op);print_string(" * ");
                       show_simplify_aux(lson, op);
                     )
	   |Div -> if operation != op
                     then (
                       print_string("(");
                       show_simplify_aux(rson, op);print_string(" / ");
                       show_simplify_aux(lson, op);
                       print_string(")")
                     )
                     else(
                       show_simplify_aux(rson, op);print_string(" / ");
                       show_simplify_aux(lson, op);
                     )
  in
  match tree with
    Var(x) -> print_char x
   |Cst(x) -> print_int x
   |Unary(tree) ->  print_string("- "); show_simplify (tree)
   |Binary(op,lson, rson) ->
     show_simplify_aux (rson, op);
      match op with 
	    Plus ->  print_string(" + ");show_simplify_aux(lson, op);                     
	   |Moins -> print_string (" - ");show_simplify_aux(lson, op);                   
	   |Mult -> print_string (" * ");show_simplify_aux(lson, op);                   
	   |Div -> print_string (" / ");show_simplify_aux(lson, op);            
;;



(*exemple d'arbre*)
let t = "x 3 + 5 7 + + 3 4 * 1 3 + / /;";;



let tree = string_to_token_list t;;

show_no_simplify(parse (tree));;
show_no_simplify(simplify(parse (string_to_token_list ("x 3 + 5 7 + + 3 4 * 1 3 + / /;"))));;
show_simplify(parse (tree));;
show_simplify(simplify(parse (string_to_token_list ("x 3 + 5 7 + + 3 4 * 1 3 + / /;"))));;
