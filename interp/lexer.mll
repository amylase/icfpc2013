{
  open Parser
}

let space = [' ' '\t' '\r' '\n']

rule token = parse
  | space+   { token lexbuf }
  | '0'      { ZERO }
  | '1'      { ONE }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | "if0"    { IF0 }
  | "lambda" { LAMBDA }
  | "fold"   { FOLD }
  | "not"    { NOT }
  | "shl1"   { SHL1 }
  | "shr1"   { SHR1 }
  | "shr4"   { SHR4 }
  | "shr16"  { SHR16 }
  | "and"    { AND }
  | "or"     { OR }
  | "xor"    { XOR }
  | "plus"   { PLUS }
  | 'x'      { X }
  | 'y'      { Y }
  | 'z'      { Z }
  | eof      { raise End_of_file }
