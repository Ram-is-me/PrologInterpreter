{
exception Lexer_exception of string
}

let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*
let upperCase = ['A'-'Z']
let lowerCase = ['a'-'z']
let underScore = '_'

let alphaNumeric = upperCase | lowerCase | underScore | digit

let variable = upperCase alphaNumeric*
let iden = (lowerCase alphaNumeric*)
let lparen = '('
let rparen = ')'
let comma = ',' (* also used for 'and' *)
let or_op = ';'
let not_op = '!'
let implies = [':']['-']
let empty = ""

rule scan = parse
  |	[' ' '\t']		{ scan lexbuf					} (*skips blanks*)
  | ['\n']			{ Parser.EOL					}
  | comma 			{ Parser.COMMA					}
  | or_op			{ Parser.OR						}
  | not_op			{ Parser.NOT					}
  | implies			{ Parser.IMPLIES				}
  | empty			{ Parser.EMPTY					}
  | variable as v 	{ Parser.VAR(v)					}
  | integer as i 	{ Parser.NUM(int_of_string i)	}
  | iden as c		{ Parser.IDEN(c)				}
  |	lparen			{ Parser.LPAREN					}
  | rparen			{ Parser.RPAREN					}
  | eof				{Parser.EOF					}
  |	_				{scan lexbuf				}

{

}
