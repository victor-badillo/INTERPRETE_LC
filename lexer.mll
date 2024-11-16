
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "fix"       { FIX }
  | "in"        { IN }
  | "concat"    { CONCAT }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "quit"      { QUIT }
  | "as"        { AS }    (*Variants*)
  | "case"      { CASE }  (*Variants*)
  | "of"        { OF }    (*Variants*)
  | "|"         { OPT }   (*Variants*)
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LCURLY }  (*Register opening curly bracket*)
  | '}'         { RCURLY }  (*Register opening curly bracket*)
  | '<'         { LARROW }  (*Variants*)
  | '>'         { RARROW }  (*Variants*)
  | ','         { COMMA }   (*Register commas, used for tuples, records and variants*)
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | "=>"        { STRONGARROW }  (*Variants*)
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z']['A'-'Z''a'-'z' '_''0'-'9']*             (*Variables por types*)
                { IDT (Lexing.lexeme lexbuf) }
  | '"'[^ '"' ';' '\n']*'"' (**)
                { let s = Lexing.lexeme lexbuf in
                  STRINGV (String.sub s 1 (String.length s -2)) }
  | ";;"        { DOUBLE_SEMICOLON}     (*Token para final de expresion*)
  | eof         { EOF }
  | _           { raise Lexical_error }

