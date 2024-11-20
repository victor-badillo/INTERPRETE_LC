
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
%token LETREC   (*Recursion*)
%token FIX      (*Fix*)
%token IN
%token CONCAT   (*Concat*)
%token BOOL
%token NAT
%token STRING   (*Strings*)
%token QUIT     (*Quit*)
%token AS       (*Variants*)
%token CASE     (*Variants*)
%token OF       (*Variants*)
%token LIST     (*Lists*)
%token NIL      (*Lists*)
%token CONS     (*Lists*)
%token ISNIL    (*Lists*)
%token HEAD     (*Lists*)
%token TAIL     (*Lists*)

%token LPAREN
%token RPAREN
%token LCURLY   (*Tuples, Records*)
%token RCURLY   (*Tuples, Records*)
%token LSQUARE  (*Lists*)
%token RSQUARE  (*Lists*)
%token LARROW   (*Variants*)
%token RARROW   (*Variants*)
%token COMMA    (*Tuples, Records, Variants*)
%token DOT
%token EQ
%token COLON
%token ARROW
%token STRONGARROW  (*Variants*)
%token EOF
%token DOUBLE_SEMICOLON     (*End of an expression*)
%token OPT       (*Variants*)

%token <int> INTV
%token <string> IDV
%token <string> IDT
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

(*Assign term, assign type, evaluate term or exit with quit*)
s: IDV EQ term DOUBLE_SEMICOLON
        { Bind ($1, $3) }
   | IDT EQ ty DOUBLE_SEMICOLON
        { TBind ($1, $3) }
   | term DOUBLE_SEMICOLON
        { Eval $1 }
   | QUIT DOUBLE_SEMICOLON
        { Quit }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }      (*Fix for referencing itself*)
  | LARROW IDV EQ term RARROW AS ty     (*Tagging at variants*)
      { TmTag ($2, $4, $7) }
  | CASE term OF variantCases           (*Case, variants*)
      { TmCase ($2, $4) }               (*Save term and then get the list of cases*)

appTerm :
    indexTerm
      { $1 }
  | SUCC indexTerm
      { TmSucc $2 }
  | PRED indexTerm
      { TmPred $2 }
  | ISZERO indexTerm
      { TmIsZero $2 }
  | CONCAT indexTerm indexTerm     (*Concatenation, ex: concat "under" "water"*)
      { TmConcat ($2, $3) }
  | FIX indexTerm                  (*Fix*)
      { TmFix $2 }
  | CONS LSQUARE ty RSQUARE indexTerm indexTerm     (*Contruct value for list, ex: cons[Ty] _ _*)
      { TmCons ($3,$5,$6) }
  | ISNIL LSQUARE ty RSQUARE indexTerm              (*Check nil value, ex: isnil[Ty] _*)
      { TmIsNil ($3,$5) }
  | HEAD LSQUARE ty RSQUARE indexTerm               (*Get head of list, ex: head[Ty] _*)
      { TmHead ($3,$5) }
  | TAIL LSQUARE ty RSQUARE indexTerm               (*Get tail of list, ex: tail[Ty] _*)
      { TmTail ($3,$5) }
  | appTerm indexTerm
      { TmApp ($1, $2) }

indexTerm :
  | indexTerm DOT INTV                      (*Projection with numbers*)
      { TmProj ($1, string_of_int $3) }
  | indexTerm DOT IDV                       (*Projection with variables*)
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
  | STRINGV                         (*Term string with its value*)
      { TmString $1 }
  | LCURLY tupleTerm RCURLY         (*Tuples are surrounded by {}*)
      { TmTuple $2 }
  | LCURLY recordTerm RCURLY        (*Records are surrounded by {}*)
      { TmRecord $2 }
  | NIL LSQUARE ty RSQUARE          (*Nil value on lists, ex: nil[Ty]*)
      { TmNil $3 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | LARROW variantTy RARROW         (*Variants are surrounded by <>*)
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
  | LCURLY tupleTy RCURLY           (*Tuples are surrounded by {}*)
      { TyTuple $2 } 
  | LCURLY recordTy RCURLY          (*Records are surrounded by {}*)
      { TyRecord $2 }
  | LIST LSQUARE ty RSQUARE         (*Lists type, ex: List[Ty]*)
      { TyList $3 }

(*Save types in a list, not valid empty tuple*)
tupleTy:
    ty
      { [$1] }
  | ty COMMA tupleTy
      { $1 :: $3 }

(*Save identifiers and types in a list, valid empty record*)
recordTy:
    { [] }
  | IDV COLON ty
      { [($1,$3)] }
  | IDV COLON ty COMMA recordTy
      { ($1,$3) :: $5 }

(*Similar to records, not valid empty variant*)
variantTy :
    IDV COLON ty
      { [($1, $3)] }
  | IDV COLON ty COMMA variantTy
      { ($1, $3) :: $5 } 

(*Save terms in a list, not valid empty tuple*)
tupleTerm :
    term
      { [$1] }
  | term COMMA tupleTerm
      { $1 :: $3 }

(*Save records in a list, valid empty record*)
recordTerm:
    IDV EQ term
      { [($1, $3)] }
  | IDV EQ term COMMA recordTerm
      { ($1, $3) :: $5 }
  | { [] }

(*Get cases from variants in a list*)
variantCases:
  variantCase 
     { [$1] }
| variantCase OPT variantCases     (*More than one case is separated by | *)
    { $1 :: $3}

(*Save tag, value and body next to strong arrow*)
variantCase :
    LARROW IDV EQ IDV RARROW STRONGARROW appTerm
      { ($2, $4, $7) }

