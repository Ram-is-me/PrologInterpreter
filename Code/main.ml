open Printf
open Expr_type
exception Out_of_loop

let extract_filename s =
	let len = String.length s in  
        let extract_within = String.sub s 1 (len-2) in
        let filename = (extract_within ^ ".pl") in
	filename

let load_file s =
	let input_file = open_in s in 
	let file_content = really_input_string input_file (in_channel_length input_file) in file_content


let global_arr = []

let test_parser s =
  try
    let lexbuf = Lexing.from_string s in
    let result = (Parser.rule Lexer.scan lexbuf) in
    Printf.printf "%s\n" (Expr_type.string_of_rule result)
  with Parsing.Parse_error ->
    Printf.printf "%s -> false\n" s

let rec make_list_of_rules arr =	
	match arr with
		[] -> []
  |	h::t -> 
        if((compare h) "" = 0) then []
        else
          let lexbuf = Lexing.from_string h in
          let result = (Parser.rule Lexer.scan lexbuf) in
          result::(make_list_of_rules t)

let make_expr rule =
	match rule with
		HEAD(x) -> x
	|	NODE(x,y) -> x

let rule_printer rule = print_endline (Expr_type.string_of_rule rule)

let test_file () =
  let input = print_string "> ";read_line () in 
  let filename = extract_filename input in 
  let readcontent = load_file filename in
  print_endline ("Successfully loaded " ^ filename);
  let test_list = String.split_on_char '\n' readcontent in 
  let rule_list = make_list_of_rules test_list in
  let one = ref true in
  let zero = ref false in
  while (!one) != (!zero)
  do
  	let in_query = print_string "?- "; read_line () in
	if ((compare in_query "exit") = 0) then one := !zero
	else
	begin
		let query_buf = Lexing.from_string in_query in
  		let query = make_expr (Parser.rule Lexer.scan query_buf) in
  		let flag, hash = (Search.search_rule_list query [] rule_list rule_list) in
  		Hashtbl.iter (fun x y -> if((Search.has_var y)) then () else Printf.printf "%s = %s\n" x (string_of_expr y)) hash;
  		Printf.printf "%b\n" flag
  	end
  done

let _ = test_file ()
