open Format
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString (*Añadido tipo string**)
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
  | TmString of string (*Añadido tipo de string*)
  | TmConcat of term * term (*Añadido tipo de concat*)
;;

type command = 
    Eval of term
  | Bind of string * term
  | Quit
;;

type binding =
    TyBind of ty
  | TyTmBind of (ty * term)
;;

type context =
  (string * binding) list
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addtbinding ctx s ty =
  (s, TyBind ty) :: ctx
;;

let addvbinding ctx s ty tm =
  (s, TyTmBind (ty, tm)) :: ctx
;;

let gettbinding ctx s =
  match List.assoc s ctx with
      TyBind ty -> ty
    | TyTmBind (ty, _) -> ty
;;

let getvbinding ctx s =
  match List.assoc s ctx with
      TyTmBind (_, tm) -> tm
    | _ -> raise Not_found
;;

(*
let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;
*)

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      string_of_ty ty1 ^ " -> " ^ string_of_ty ty2
  | TyString ->
      "String"
;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addtbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2
  
    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if tyT11 = tyT12 then tyT12
          else raise (Type_error "result of body not compatible with domain")
      | _ -> raise (Type_error "arrow type expected"))
      (*new rule*)
  | TmString _ ->
      TyString
  | TmConcat (t1, t2) ->
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "argument of concat is not error")
;;


(* TERMS MANAGEMENT (EVALUATION) *)
(*
let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^string_of_term t ^ ")"
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1,t2) ->
      "concat " ^ "(" ^ string_of_term t1 ^ ")" ^ " " ^ "(" ^ string_of_term t2 ^ ")"
;;
*)
(*
let rec string_of_term n = match n with
  | TmAbs (s, tyS, t) ->
      "lambda " ^ s ^ " : " ^ string_of_ty tyS ^ ". " ^ string_of_term t
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmIf (t1,t2,t3) ->
        "if " ^ string_of_term t1 ^ 
        " then " ^ string_of_term t2 ^ 
        " else " ^ string_of_term t3
  | _ -> 
        string_of_appTerm n


and string_of_appTerm t = match t with
  | TmApp (t1, t2) -> 
      string_of_appTerm t1 ^ " " ^ string_of_appTerm t2
  | TmSucc t -> 
    let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "(succ " ^ string_of_atomicTerm t ^ ")"
      in f 1 t
  | TmPred t -> 
      "(pred " ^ string_of_atomicTerm t ^ ")"
  | TmIsZero t ->
      "iszero " ^ string_of_atomicTerm t
  | TmFix t ->
      "fix " ^ string_of_atomicTerm t
  | TmConcat (t1,t2) ->
      "concat " ^ "(" ^ string_of_atomicTerm t1 ^ ")" ^ " " ^ "(" ^ string_of_atomicTerm t2 ^ ")"
  | _ -> 
      string_of_atomicTerm t


and string_of_atomicTerm t = match t with
  | TmVar s -> 
      s
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmZero ->
      "0"
  | TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | _ -> "(" ^ string_of_term t ^ ")"

;;
*)

let rec pretty_printer n = match n with
  | TmAbs (s, tyS, t) ->
      open_hvbox 1;
      print_string ("lambda " ^ s ^ " : " ^ string_of_ty tyS ^ ".");
      print_space();
      pretty_printer t;
      close_box ()
  | TmLetIn (s, t1, t2) ->
      open_hvbox 0;
      print_string("let " ^ s ^ " = ");
      pretty_printer t1;
      print_string(" in ");
      pretty_printer t2;
      close_box ()
  | TmIf (t1,t2,t3) ->
      open_hvbox 1;
      print_string("if ");
      pretty_printer t1;
      print_string(" then ");
      pretty_printer t2;
      print_space();
      print_string("else ");
      pretty_printer t3;
      close_box ()
  | _ -> 
        string_of_appTerm n


and string_of_appTerm t = match t with
  | TmApp (t1, t2) -> 
      string_of_appTerm t1;
      print_string(" ");
      string_of_appTerm t2
      
  | TmSucc t -> 
    let rec f n t' = match t' with
          TmZero -> print_string(string_of_int n)
        | TmSucc s -> f (n+1) s
        | _ -> print_string("(succ ");
               string_of_atomicTerm t;
               print_string(")")
      in f 1 t
  | TmPred t -> 
      print_string("(pred ");
      string_of_atomicTerm t;
      print_string(")")
  | TmIsZero t ->
      print_string("iszero ");
      string_of_atomicTerm t
  | TmFix t ->
      open_hvbox 0;
      print_string("fix ");
      string_of_atomicTerm t;
      print_space();
      close_box ()
  | TmConcat (t1,t2) ->
      print_string("concat (");
      string_of_atomicTerm t1;
      print_string(") (");
      string_of_atomicTerm t2;
      print_string(")")
  | _ -> 
      string_of_atomicTerm t


and string_of_atomicTerm t = match t with
  | TmVar s -> 
      print_string(s)
  | TmString s ->
      print_string("\"" ^ s ^ "\"")
  | TmZero ->
      print_string("0")
  | TmTrue ->
      print_string("true")
  | TmFalse ->
      print_string("false")
  | _ -> print_string("(");
         pretty_printer t;
         print_string(")")

;;


let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString _ ->
      []
  | TmConcat (t1,t2) ->
      lunion (free_vars t1) (free_vars t2)
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t -> 
      TmFix (subst x s t)
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2
      (*
  | TmFix v1 when isval v1 ->
      (match v1 with
          TmAbs (x,_, t12) -> subst x tm t12
        | _ -> raise NoRuleApplies)
  *)
    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'
    (* new rule for string*)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)
    (* new rule for string*)
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 ctx t2 in
      TmConcat (TmString s1, t2')

   (* new rule for string*)
  | TmConcat (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmConcat ( t1', t2)
  | TmVar s ->
      getvbinding ctx s
  | _ ->
      raise NoRuleApplies
;;

(*Lista de variables libres en el termino, coge las variables libres y luego una vez por cada una*)
let apply_ctx ctx tm =
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)
;;

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;

(*
let execute ctx = function 
  Eval tm ->
    let tyTm = typeof ctx tm in
    let tm' = eval ctx tm in
    print_endline("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    ctx

  | Bind (s, tm) -> 
    let tyTm = typeof ctx tm in
    let tm' = eval ctx tm in
    print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    addvbinding ctx s tyTm tm'
  | Quit ->
    raise End_of_file
*)


(*
        let tm = s token (from_string input) in
        let tyTm = typeof ctx tm in
        (*print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term (eval tm)); *) (* First type and the term*)
        Format.open_hvbox 0;
        print_string ("- : " ^ string_of_ty tyTm ^ " =");
        Format.print_space();
        pretty_printer (eval tm);
        Format.close_box ();
        Format.print_flush(); (*Clean boxes*)
        print_newline();
        loop ctx
        *)

let execute ctx = function 
  Eval tm ->
    let tyTm = typeof ctx tm in
    let tm' = eval ctx tm in
    Format.open_hvbox 0;
    print_string ("- : " ^ string_of_ty tyTm ^ " =");
    Format.print_space();
    pretty_printer (tm');
    Format.close_box ();
    Format.print_flush(); (*Clean boxes*)
    print_newline();
    ctx

  | Bind (s, tm) -> 
    let tyTm = typeof ctx tm in
    let tm' = eval ctx tm in
    Format.open_hvbox 0;
    print_string (s ^ " : " ^ string_of_ty tyTm ^ " =");
    Format.print_space();
    pretty_printer (tm');
    Format.close_box ();
    Format.print_flush(); (*Clean boxes*)
    print_newline();
    addvbinding ctx s tyTm tm'
  | Quit ->
    raise End_of_file