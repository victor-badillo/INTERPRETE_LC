
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token FIX
%token IN
%token CONCAT
%token BOOL
%token NAT
%token STRING (*Token de string*)
%token QUIT
%token AS       (*Variants*)
%token CASE     (*Variants*)
%token OF       (*Variants*)
%token LIST    (*Lists*)
%token NIL      (*Lists*)
%token CONS     (*Lists*)
%token ISNIL    (*Lists*)
%token HEAD     (*Lists*)
%token TAIL     (*Lists*)

%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token LSQUARE  (*Lists*)
%token RSQUARE  (*Lists*)
%token LARROW   (*Variants*)
%token RARROW   (*Variants*)
%token COMMA
%token DOT
%token EQ
%token COLON
%token ARROW
%token STRONGARROW  (*Variants*)
%token EOF
%token DOUBLE_SEMICOLON     (*Token final de expresion*)
%token OPT                  (*Variants*)

%token <int> INTV
%token <string> IDV
%token <string> IDT
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%
(*
s :
    term DOUBLE_SEMICOLON
      { $1 }*)

s: IDV EQ term DOUBLE_SEMICOLON
        { Bind ($1, $3) }
   | IDT EQ ty DOUBLE_SEMICOLON
        { TBind ($1, $3) }
   | term DOUBLE_SEMICOLON
        { Eval $1 }
   | QUIT DOUBLE_SEMICOLON  (*Puso EOF*)
        { Quit }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  (*| LET IDV EQ term             (*let without 'in'*)
      { TmLetIn ($2, $4, $4) } *)
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  (*| LETREC IDV COLON ty EQ term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), TmVar $2) } *)(* letrec without 'in' *) 
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
  | LARROW IDV EQ term RARROW AS ty
      { TmTag ($2, $4, $7) }
  | CASE term OF variantCases
      { TmCase ($2, $4) } 

appTerm :
    indexTerm
      { $1 }
  | SUCC indexTerm
      { TmSucc $2 }
  | PRED indexTerm
      { TmPred $2 }
  | ISZERO indexTerm
      { TmIsZero $2 }
  | CONCAT indexTerm indexTerm
      { TmConcat ($2, $3) }
  | FIX indexTerm
      { TmFix $2 }
  | CONS LSQUARE ty RSQUARE indexTerm indexTerm
      { TmCons ($3,$5,$6) }
  | ISNIL LSQUARE ty RSQUARE indexTerm
      { TmIsNil ($3,$5) }
  | HEAD LSQUARE ty RSQUARE indexTerm
      { TmHead ($3,$5) }
  | TAIL LSQUARE ty RSQUARE indexTerm
      { TmTail ($3,$5) }
  | appTerm indexTerm
      { TmApp ($1, $2) }

indexTerm :
  | indexTerm DOT INTV                      (*proj with numbers*)
      { TmProj ($1, string_of_int $3) }
  | indexTerm DOT IDV
      { TmProj ($1, $3) } 
  | atomicTerm
      { $1 } 


atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
      { TmString $1 }
  | LCURLY tupleTerm RCURLY
      { TmTuple $2 }
  | LCURLY recordTerm RCURLY
      { TmRecord $2 }
  | NIL LSQUARE ty RSQUARE 
      { TmNil $3 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | LARROW variantTy RARROW
      { TyVariant $2 } 

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | IDT
      { TyVar $1 }
  | LCURLY recordTy RCURLY
      { TyRecord $2 }
  | LIST LSQUARE ty RSQUARE
      { TyList $3 }

recordTy:
    { [] }
  | IDV COLON ty
      { [($1,$3)] }
  | IDV COLON ty COMMA recordTy
      { ($1,$3) :: $5 }

variantTy :
    IDV COLON ty
      { [($1, $3)] }
  | IDV COLON ty COMMA variantTy
      { ($1, $3) :: $5 } 

tupleTerm : (* save terms from tuple in a list, not valid empty tuple*)
    term
      { [$1] }
  | term COMMA tupleTerm
      { $1 :: $3 }

recordTerm:
    IDV EQ term
      { [($1, $3)] }
  | IDV EQ term COMMA recordTerm
      { ($1, $3) :: $5 }
  | { [] }

variantCases:
  variantCase 
     { [$1] }
| variantCase OPT variantCases
    { $1 :: $3}

variantCase :
    LARROW IDV EQ IDV RARROW STRONGARROW appTerm
      { [($2, $4, $7)] }
 (* | LARROW IDV EQ term  RARROW STRONGARROW appTerm OPT variantCase
      { ($2, $4, $7) :: $9 } *)s

