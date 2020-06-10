open Expr_type

type pair_expr = expr * expr

let tests = [
  CONST ( IDEN ( "mia" ) ) , 
  CONST ( IDEN ( "mia" ) ) ;

  CONST ( IDEN ( "mia" ) ) , 
  CONST ( IDEN ( "vincent" ) ) ;

  VAR ( "X" ) , 
  VAR ( "Y" ) ;

  VAR ( "X" ) , 
  CONST ( IDEN ( "mia" ) ) ;

  FUNC ("f",[VAR ("X");VAR("Y")]) , 
  VAR ("X") ;

  FUNC ("vertical",[FUNC("line",[FUNC("point",[CONST(NUM(1));CONST(NUM(1))]);FUNC("point",[CONST(NUM(1));CONST(NUM(3))]) ]) ]),
  FUNC ("vertical",[FUNC("line",[FUNC("point",[VAR("X");VAR("Y")]);FUNC("point",[VAR("X");VAR("Z")])])]) ;

  FUNC ("vertical",[FUNC("line",[FUNC("point",[VAR("X");VAR("Y")]);FUNC("point",[VAR("X");VAR("Z")])])]),
  FUNC ("vertical",[FUNC("line",[FUNC("point",[CONST(NUM(1));CONST(NUM(1))]);FUNC("point",[CONST(NUM(2));CONST(NUM(3))]) ]) ]) ;

  FUNC("food",[CONST(IDEN("bread"));VAR("X")]),
  FUNC("food",[VAR("Y");CONST(IDEN("sausage"))]);

  FUNC("food",[CONST(IDEN("bread"));VAR("X")]),
  FUNC("food",[VAR("Y");CONST(IDEN("bread"))]);

  FUNC("meal",[FUNC("food",[FUNC("f1",[FUNC("f2",[FUNC("f3",[VAR("Y")])])])]);VAR("X")]),
  FUNC("meal",[VAR("X");FUNC("food",[FUNC("f1",[FUNC("f2",[FUNC("f3",[VAR("Y")])])])])]) ; 

  FUNC("meal",[FUNC("food",[FUNC("f1",[FUNC("f2",[FUNC("f3",[CONST(NUM(100))])])])]);VAR("X")]),
  FUNC("meal",[VAR("X");FUNC("food",[FUNC("f1",[FUNC("f2",[FUNC("f3",[CONST(NUM(10))])])])])]) ; 
]

let rec string_of_expr expr =
    match expr with 
    | CONST(IDEN(x)) | VAR(x) -> x
    | CONST(NUM(x)) -> (string_of_int x)
    | FUNC(name,list1) -> name ^ "(" ^ (string_of_list list1) ^ ")"
and string_of_list list1 = 
  match list1 with
  | [] -> ""
  | [h] -> (string_of_expr h)
  | h :: t -> (string_of_expr h) ^ ", " ^ (string_of_list t)


let unifier s t = 
  let var_exp = Hashtbl.create 1000 in
  let rec rec_match s t =
    match (s,t) with 
    | CONST(a),CONST(b) -> a = b

    | VAR(x) , value | value, VAR(x)-> 
        let prev_instant = Hashtbl.find_opt var_exp x in
        if(prev_instant = None) then 
        begin
          Hashtbl.add var_exp x value;
          true
        end
        else
          let prev_instant = (Hashtbl.find var_exp x) in
          if (value = prev_instant) then true
          else false

    | FUNC(f1,list1), FUNC(f2,list2) ->
    begin
      if(((compare f1 f2)!= 0) || ((List.length list1) != (List.length list2))) then false
      else
        let flag = ref true in 
        let flag_true = ref true in 
        let rec compute_list list1 list2 = 
          match list1,list2 with
          | [],[] | [],_ | _,[] -> ()
          | head1::tail1,head2::tail2 -> 
          begin
            flag := !flag && (rec_match head1 head2);
            compute_list tail1 tail2
          end
        in compute_list list1 list2;
        if(!flag = !flag_true) then true
        else false 
    end
    | _ -> false
  in 
  let flag = rec_match s t in
  Printf.printf "?- %s = %s\n" (string_of_expr s) (string_of_expr t);
  if(flag) then
  begin
    Hashtbl.iter (fun x y -> Printf.printf "%s = %s\n" x (string_of_expr y)) var_exp ;
    Printf.printf "true\n\n";
    true,var_exp
  end
  else 
  begin
    Printf.printf "false\n\n";
    false,var_exp
  end

let unify_pair a = begin match a with x,y -> ignore((unifier x y)); () end

let test_all () = 
  List.iter unify_pair tests 

