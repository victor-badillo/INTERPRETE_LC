
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString (*Añadido tipo string*)
  | TyVar of string
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyList of ty  (*type for Lists*)
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
  | TmFix of term
  | TmString of string (*Termino de string*)
  | TmConcat of term * term (*Termino para concatenar string*)
  | TmTuple of term list
  | TmProj of term * string
  | TmRecord of (string * term) list
  | TmNil of ty (*Lists*)
  | TmCons of ty * term * term  (*Lists*)
  | TmIsNil of ty * term  (*Lists*)
  | TmHead of ty * term   (*Lists*)
  | TmTail of ty * term   (*Lists*)
  | TmTag of string * term * ty
  | TmCase of term * (string * string * term) list
;;

type command = 
    Eval of term
  | Bind of string * term
  | TBind of string * ty
  | Quit
;;

type binding =
  TyBind of ty
  | TyTmBind of (ty * term)
;;

type context =
  (string * binding) list
;;

val emptyctx : context;;
val addtbinding : context -> string -> ty -> context;;
val addvbinding : context -> string -> ty -> term -> context;;
val gettbinding : context -> string -> ty;;
val getvbinding : context -> string -> term;;
(*
val addbinding : context -> string -> ty -> context;;
val getbinding : context -> string -> ty;;
*)

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val pretty_printer : term -> unit;;
(*val string_of_term : term -> string;;*)
exception NoRuleApplies;;
val eval : context -> term -> term;;
val execute : context -> command -> context;;

