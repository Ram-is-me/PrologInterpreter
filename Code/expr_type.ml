type constant =
  | NUM of int
  | IDEN of string
 
type expr = 
  | VAR of string
  | CONST of constant
  | FUNC of string * expr list

type right = 
  | LEAF of expr
  | OR of right * right
  | AND of right * right
  | NOT of right

type rule = 
  | HEAD of expr
  | NODE of expr * right

let rec string_of_expr expr =
    match expr with 
    	CONST(IDEN(x)) -> x
    |   CONST(NUM(x)) -> (string_of_int x)
    |   VAR(x) -> x
    |   FUNC(name,list1) -> name ^ "(" ^ (string_of_list list1) ^ ")"
and string_of_list list1 = 
  	match list1 with
  		[] -> ""
  	|   [h] -> (string_of_expr h)
  	|   h :: t -> (string_of_expr h) ^ ", " ^ (string_of_list t)

let rec string_of_right right = 
  	match right with
    | LEAF(x) -> (string_of_expr x)
    | OR(x,y) -> (string_of_right x) ^ " | " ^ (string_of_right y)
    | AND(x,y) -> (string_of_right x) ^ " , " ^ (string_of_right y)
    | NOT(x) -> "!"^(string_of_right x) ^ " "


let rec string_of_rule rule = 
	match rule with
  	| HEAD(x) -> (string_of_expr x)
  	| NODE(x,y) -> (string_of_expr x) ^ " :- " ^ (string_of_right y)


