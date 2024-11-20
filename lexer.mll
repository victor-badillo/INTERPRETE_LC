
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
  | "letrec"    { LETREC }    (*Token for recursive functions*)
  | "fix"       { FIX }       (*Token for fix*)
  | "in"        { IN }
  | "concat"    { CONCAT }    (*Token for concatenating string*)
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }    (*Token for string*)
  | "quit"      { QUIT }      (*Token for exitS*)
  | "as"        { AS }        (*Token as from tagging in variants*)
  | "case"      { CASE }      (*Token case for variants*)
  | "of"        { OF }        (*Token of from variants*)
  | "List"      { LIST }      (*Token for type List*)
  | "nil"       { NIL }       (*Token for nil value on lists*)
  | "cons"      { CONS }      (*Token for adding values to lists*)
  | "isnil"     { ISNIL }     (*Token for checking if its value is nil on lists*)
  | "head"      { HEAD }      (*Token used for getting head of list*)
  | "tail"      { TAIL }      (*Token used for getting tail of list*)
  | "|"         { OPT }       (*Token for separating cases on variants*)
  | '('         { LPAREN }    (*Token for left bracket*)
  | ')'         { RPAREN }    (*Token for right bracket*)
  | '{'         { LCURLY }    (*Token for left curly bracket, tuples and records*)
  | '}'         { RCURLY }    (*Token for right curly bracket, tuples and records*)
  | '['         { LSQUARE }   (*Token for left square bracket, lists*)
  | ']'         { RSQUARE }   (*Token for right square bracket, lists*)
  | '<'         { LARROW }    (*Token for left angle bracket, variants*)
  | '>'         { RARROW }    (*Token for right angle bracket, variants*)
  | ','         { COMMA }     (*Token for commas, tuples, records and variants*)
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | "=>"        { STRONGARROW }   (*Token for strong arrow inside cases of variants*)
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*   (*No capital letter valid for variable names*)
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z']['A'-'Z''a'-'z' '_''0'-'9']*       (*Token identifiers of types*)
                { IDT (Lexing.lexeme lexbuf) }
  | '"'[^ '"' ';' '\n']*'"'     (*Token for string values, any value except double quote, semi colon and new line*)
                { let s = Lexing.lexeme lexbuf in
                  STRINGV (String.sub s 1 (String.length s -2)) }   (*Substract quotes at the beggining and at the end*)
  | ";;"        { DOUBLE_SEMICOLON }     (*Token for indicate the end of an expression*)
  | eof         { EOF }
  | _           { raise Lexical_error }

