%{
  open Syntax
%}

%token ZERO ONE LPAREN RPAREN IF0 LAMBDA FOLD NOT SHL1 SHR1 SHR4 SHR16 AND OR XOR PLUS X Y Z

%start program
%type <Syntax.program> program
%%

program:
  | LPAREN LAMBDA LPAREN X RPAREN expr { Lambda $6 }

expr:
  | ZERO { Zero }
  | ONE  { One  }
  | X    { Var "x" }
  | Y    { Var "y" }
  | Z    { Var "z" }
  | LPAREN IF0 expr expr expr RPAREN { If0 ($3, $4, $5) }
  | LPAREN FOLD expr expr LPAREN LAMBDA LPAREN Y Z RPAREN expr RPAREN RPAREN { Fold ($3, $4, $11) }
  | LPAREN op1 expr RPAREN { Op1 ($2, $3) }
  | LPAREN op2 expr expr RPAREN { Op2 ($2, $3, $4) }

op1:
  | NOT { Not }
  | SHL1 { Shl1 }
  | SHR1 { Shr1 }
  | SHR4 { Shr4 }
  | SHR16 { Shr16 }

op2:
  | AND  { And }
  | OR   { Or  }
  | XOR  { Xor }
  | PLUS { Plus }
