
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

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF
%token DOUBLE_SEMICOLON     (*Token final de expresion*)

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

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
  | FIX atomicTerm
      { TmFix $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

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

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

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

