%{
open Expr_type
%}

%token EOL EOF LPAREN RPAREN COMMA EMPTY OR NOT IMPLIES
%token <string> VAR
%token <string> IDEN
%token <int> NUM

%start rule
%type <Expr_type.rule> rule

%% /* Grammar rules and actions follow */
;
  	rule: 	
		| expr							{ Expr_type.HEAD($1)		}
    	| expr IMPLIES right            { Expr_type.NODE($1,$3)     }


	const:		
		| NUM							{ Expr_type.NUM($1) 		}
		| IDEN							{ Expr_type.IDEN($1)		}		

	right:
        | expr                          { Expr_type.LEAF($1)        }
        | right OR right                { Expr_type.OR($1, $3)      }   
        | right COMMA right             { Expr_type.AND($1, $3)     }
        | NOT right                     { Expr_type.NOT($2)         }

  	expr: 
		| VAR							{ Expr_type.VAR($1)			}
		| const							{ Expr_type.CONST($1)		}
		| IDEN LPAREN expr_list RPAREN 	{ Expr_type.FUNC($1, $3) 	}

	expr_list: 
		| expr							{ [$1]						}
		| expr COMMA expr_list 			{ $1 :: $3					}








