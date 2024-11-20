
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString (*String*)
  | TyVar of string (*Global context*)
  | TyTuple of ty list  (*Tuples*)
  | TyRecord of (string * ty) list  (*Records*)
  | TyList of ty  (*Lists*)
  | TyVariant of (string * ty) list (*Variants*)
;;


type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term       (*Fix*)
  | TmString of string (*String*)
  | TmConcat of term * term (*Concat*)
  | TmTuple of term list  (*Tuples*)
  | TmProj of term * string  (*Projection*)
  | TmRecord of (string * term) list  (*Records*)
  | TmNil of ty (*Nil*)
  | TmCons of ty * term * term  (*Constructor*)
  | TmIsNil of ty * term  (*Isnil*)
  | TmHead of ty * term   (*Head*)
  | TmTail of ty * term   (*Tail*)
  | TmTag of string * term * ty (*Variants tagging*)
  | TmCase of term * (string * string * term) list  (*Variants case*)
;;

(*Type of command input by user*)
type command = 
    Eval of term            (*Evaluate*)
  | Bind of string * term   (*Binding terms*)
  | TBind of string * ty    (*Binding types*)
  | Quit                    (*Exit programm*)
;;

(*Type of bindings for context*)
type binding =
  TyBind of ty              (*Types*)
  | TyTmBind of (ty * term) (*Terms*)
;;

(*Global context*)
type context =
  (string * binding) list
;;

val emptyctx : context;;  (*Initial value for context*)

(*Functions used for adding types or values for variables*)
val addtbinding : context -> string -> ty -> context;;
val addvbinding : context -> string -> ty -> term -> context;;
(*Functions used for retrieving types or values from variables*)
val gettbinding : context -> string -> ty;;
val getvbinding : context -> string -> term;;

exception Type_error of string;;      (*Exception*)
val typeof : context -> term -> ty;;  (*Type of term in context*)

exception NoRuleApplies;;             (*No rule applies*)
val eval : context -> term -> term;;   (*Evaluate term in context*)
val execute : context -> command -> context;;   (*Execute command in context*)

