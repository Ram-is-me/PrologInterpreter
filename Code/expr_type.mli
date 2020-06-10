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

val string_of_expr : expr -> string
val string_of_right : right -> string
val string_of_rule : rule -> string
