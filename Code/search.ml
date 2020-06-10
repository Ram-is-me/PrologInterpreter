open Unified
open Expr_type 

let rec right_to_list right = 
  match right with 
  | LEAF(x) -> [x]
  | OR(x,y) -> (List.append (right_to_list x) (right_to_list y))
  | AND(x,y) -> (List.append (right_to_list x) (right_to_list y))
  | NOT(x) -> (right_to_list x)

let rec string_of_right right = 
  match right with
  | LEAF(x) -> (Expr_type.string_of_expr x)
  | OR(x,y) -> (string_of_right x) ^ " | " ^ (string_of_right y)
  | AND(x,y) -> (string_of_right x) ^ " , " ^ (string_of_right y)
  | NOT(x) -> "!"^(string_of_right x) ^ " "

let rec string_of_rule rule = 
  match rule with
  | HEAD(x) -> (Expr_type.string_of_expr x)
  | NODE(x,y) ->
    (Expr_type.string_of_expr x) ^ " :- " ^ (string_of_right y) 

let rec string_list_of_rule rules = 
  match rules with 
  | [] -> ""
  | h :: t -> (string_of_rule h) ^ ".\n" ^ (string_list_of_rule t)
  
let rules = [
  HEAD(FUNC("f",[CONST(IDEN("a"))])) ;
  HEAD(FUNC("f",[CONST(IDEN("b"))])) ;

  HEAD(FUNC("g",[CONST(IDEN("a"))])) ;
  HEAD(FUNC("g",[CONST(IDEN("b"))])) ;

  HEAD(FUNC("h",[CONST(IDEN("b"))])) ;

  NODE(FUNC("k",[VAR("X")]),AND(AND(LEAF(FUNC("f",[VAR("X")])),LEAF(FUNC("g",[VAR("X")]))),LEAF(FUNC("h",[VAR("X")])))) ;
]

(* let search_rules query rules =  *)


let query = HEAD(FUNC("k",[VAR("Y")]))

(* Variable mapping (Comes from unification)
Query variables <-> Head variables
Transfer the mapping onto the tail *)

let rec replace_var_expr orig var value = 
  match orig with 
  | VAR(x) -> if(x = var) then value else VAR(x)
  | CONST(x) -> CONST(x)
  | FUNC(x,y) -> FUNC(x,(replace_list y var value))
and replace_list list1 var value =
  match list1 with 
  | [] -> []
  | h::t -> 
    (replace_var_expr h var value) :: (replace_list t var value)

let rec replace_var_hash expr var_expr = 
  match expr with
  | VAR(x) -> 
    if((Hashtbl.find_opt var_expr x) = None) then VAR(x) else (Hashtbl.find var_expr x)
  | CONST(x) -> CONST(x)
  | FUNC(x,y) -> FUNC(x,(replace_var_hash_list y var_expr))
and replace_var_hash_list list1 var_expr = 
  match list1 with
  | [] -> []
  | h::t -> 
    (replace_var_hash h var_expr) :: (replace_var_hash_list t var_expr)

let rec has_var expr = 
  match expr with 
  | VAR(x) -> true
  | CONST(x) -> false
  | FUNC(x,y) -> (has_list_var expr y)
and has_list_var expr list1 = 
  match list1 with
  | [] -> false
  | h :: t -> 
    if (has_var h) then true
    else (has_list_var expr t)


    let rec search_rule_list query goal_list sub_rules rules = 
      Printf.printf "Query: %s Goals: %s\n" (string_of_expr query) (string_of_list goal_list);
      let empty_hash = Hashtbl.create 1000 in
      match sub_rules with 
      | [] -> false,empty_hash
      | h :: t ->
        match h with
        | HEAD(x) ->
          let flag,unified_hash = (Unified.unifier x query) in
          if (flag = false) then (search_rule_list query goal_list t rules) 
          else 
            let new_goal_list = (replace_var_hash_list goal_list unified_hash) in
            let flag_goal,goal_hash = (search_goal_list new_goal_list unified_hash rules rules) in
            if(flag_goal = false) then (search_rule_list query goal_list t rules)
            else flag_goal,goal_hash 
            
        | NODE(head,tail) ->
          let flag,unified_hash = (Unified.unifier head query) in
          if (flag = false) then (search_rule_list query goal_list t rules) 
          else 
            let flag_tail,tail_hash = (search_right tail empty_hash rules rules) in
            (* Printf.printf "Query: %s Head: %s Tail: %s flag_tail: %b\n" (string_of_expr query) (string_of_expr head) (string_of_right tail) flag_tail; *)
            if(flag_tail = false) then (search_rule_list query goal_list t rules)
            else
              let new_head = (replace_var_hash head tail_hash) in
              let new_flag,new_unified_hash = (Unified.unifier query new_head) in
              if(new_flag = false) then (search_rule_list query goal_list t rules)
              else
                let new_goal_list = (replace_var_hash_list goal_list new_unified_hash) in
                let new_flag_goal, new_goal_hash = (search_goal_list new_goal_list new_unified_hash rules rules) in
                if(new_flag_goal = false) then (search_rule_list query goal_list t rules)
                else new_flag_goal,new_goal_hash 
              
    
    and search_goal_list goal_list orig_hash sub_rules rules = 
      match goal_list with 
      | [] -> true,orig_hash
      | h :: t -> 
        let flag_tail,tail_hash = (search_rule_list h t rules rules) in
        (*Unionify the hash tables*)
        if(flag_tail = true) then 
        begin
          Hashtbl.iter (fun x y -> Hashtbl.add orig_hash x y) tail_hash;
          true,orig_hash
        end
        (*Doesn't really matter which hash_table we return*)
        else flag_tail,tail_hash 
    
    and search_right right empty_hash sub_rules rules = 
      let goal_list = (right_to_list right) in
      search_goal_list goal_list empty_hash rules rules
