open Format
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyVar of string
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyList of ty
  | TyVariant of (string * ty) list
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
  | TmString of string
  | TmConcat of term * term
  | TmTuple of term list
  | TmProj of term * string
  | TmRecord of (string * term) list
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
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
  | TyVar s ->
      s
  | TyTuple ty ->   (*Separate values by commas and surrounded by {}*)
    let rec aux l = 
      match l with
          h :: [] -> string_of_ty h
        | h :: t -> (string_of_ty h ^ ", ") ^ aux t
        | [] -> raise (Invalid_argument "not valid empty tuple") 
    in "{" ^ aux ty ^ "}"
  | TyRecord ty ->   (*Separate key-value by commas and surrounded by {}*)
    let rec aux l = 
      match l with
          (i, h) :: [] -> i ^ " : " ^ string_of_ty h
        | (i, h) :: t -> (i ^ " : " ^ string_of_ty h ^ ", ") ^ aux t
        | [] -> ""
    in "{" ^ aux ty ^ "}"
  | TyList ty ->
    "List[" ^ string_of_ty ty ^ "]"
  | TyVariant ty ->   (*Separate key-value by commas and surrounded by <>*)
    let rec aux l = 
      match l with
          (i, h) :: [] -> i ^ " : " ^ string_of_ty h
        | (i, h) :: t -> (i ^ " : " ^ string_of_ty h ^ ", ") ^ aux t
        | [] -> ""
    in "<" ^ aux ty ^ ">"
;;

exception Type_error of string
;;

(*Function which operates recursively to determine if one type is a subtype of another type*)
let rec subtypeof tm1 tm2 = 
  match (tm1, tm2) with
    (TyRecord(l1), TyRecord(l2)) ->   (*Checks if each field in tm1 exists in tm2 and if their types are compatible*)
    let check (x, ty) l =
      try 
        subtypeof ty (List.assoc x l)
      with Not_found -> false
    in let rec contains l1 l2 = 
      match l1 with
          [] -> true
        | h::t -> check h l2 && contains t l2
    in contains l1 l2
  | (TyArr(s1, s2), TyArr(t1, t2)) -> subtypeof s1 t1 && subtypeof t2 s2   (*Checks contravariant compatibility of input types and covariant compatibility of output types*)
  | (tm1, tm2) -> tm1 = tm2
;;

(*Function which recursively obtains the basic type of some type*)
let rec typeofTy ctx ty =
  match ty with
      TyBool ->
        TyBool
    | TyNat ->
        TyNat
    | TyString ->
        TyString
    | TyArr (ty1, ty2) ->
        TyArr(typeofTy ctx ty1, typeofTy ctx ty2)
    | TyVar s ->
        (try 
          let newType = gettbinding ctx s in
          typeofTy ctx newType
        with
          _ -> raise (Type_error ("no binding type for variable " ^ s)))
    | TyTuple l ->                                                    (*Get basic type of every type*)
        TyTuple (List.map (typeofTy ctx) l)
    | TyRecord l ->                                                   (*Get basic type of every type*)
        TyRecord (List.map (fun (s, t) -> (s, typeofTy ctx t)) l)
    | TyList s ->
        TyList (typeofTy ctx s)
    | TyVariant l ->                                                  (*Get basic type of every type*)
        TyVariant (List.map (fun (s, t) -> (s, typeofTy ctx t)) l)
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
      let base_tyT1 = typeofTy ctx tyT1 in
      let ctx' = addtbinding ctx x base_tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (base_tyT1, tyT2)
    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if subtypeof tyT11 tyT2 then tyT12   (*Check if tyT11 is a subtype of tyT2*)
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
    (* T-String *)
  | TmString _ ->
      TyString
    (* T-Concat *)
  | TmConcat (t1, t2) ->
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "argument of concat is not a string")
    (* T-Tuple *)
  | TmTuple t1 -> (*Apply typeof to every value in tuple*)
      TyTuple (List.map (typeof ctx) t1)
    (* T-Record *)
  | TmRecord t1 ->  (*Get keys, get values and for this list apply typeof for each one, then combine resulting list with keys*)
        TyRecord (List.combine (List.map fst t1) (List.map (typeof ctx) (List.map snd t1)))
    (* T-Nil *)
  | TmNil t1 ->
      let base_ty = typeofTy ctx t1 in
      TyList base_ty
    (* T-Cons *)
  | TmCons (ty, t1, t2) ->
      let bty = typeofTy ctx ty in
      let ty1 = typeof ctx t1 in
      let ty2 = typeof ctx t2 in
        if ty1 = bty && ty2 = TyList bty then TyList bty    (*Check basic type with type from constructor and check same type for following values, List of basic type*)
        else raise (Type_error "cons operands have different types")
    (* T-IsNil *)
  | TmIsNil (ty, t) ->
      let bty = typeofTy ctx ty in
      if typeof ctx t = TyList(bty) then TyBool
      else raise (Type_error ("argument of isNil is not a " ^ "List[" ^ (string_of_ty ty) ^ "]"))
    (* T-Head *)
  | TmHead (ty,t) ->
      let bty = typeofTy ctx ty in
      if typeof ctx t = TyList(bty) then bty
      else raise (Type_error ("argument of head is not a " ^ "List[" ^ (string_of_ty ty) ^ "]"))
    (* T-Tail *)
  | TmTail (ty,t) ->
      let bty = typeofTy ctx ty in
      if typeof ctx t = TyList(bty) then TyList(bty)
      else raise (Type_error ("argument of tail is not a " ^ "List[" ^ (string_of_ty ty) ^ "]"))
    (* T-Variant *)
  | TmTag (s, t, ty) ->
      let tyT1 = typeof ctx t in
      let tyT2 = typeofTy ctx ty in
      (match tyT2 with
          | TyVariant l -> 
            (try 
              if tyT1 = List.assoc s l then tyT2    (*Check that term type matches with expected type*)
              else raise (Type_error ("type mismatch in variant"))
            with Not_found -> raise (Type_error ("case " ^ s ^ " not found")))
      | _ -> raise (Type_error "variant expected"))
    (* T-Case *)
  | TmCase (t, cases) ->
      let tyT1 = typeof ctx t in
      (match tyT1 with
         TyVariant l -> 
            let vtags = List.map (function (tag, _) -> tag) l in          (*Get tags from types list*)
            let ctags = List.map (function (tag, _, _) -> tag) cases in   (*Get tags from cases*)
            if List.length vtags = List.length ctags && List.for_all (fun tag -> List.mem tag vtags) ctags (*Check that both lists have the same length and all values in cases are included in list of types*)
            then
              let (tag1, id1, tm1) = List.hd cases in   (*First case*)
              let ty1 = List.assoc tag1 l in
              let ctx1 = addtbinding ctx id1 ty1 in
              let rty = typeof ctx1 tm1 in  (*Get root type*)
              let rec aux = function        (*For every value in cases, get its type, include it into the context and compare types with root type*)
                 [] -> rty
                | (tagi, idi, tmi) :: rest ->
                    let tyi = List.assoc tagi l in
                    let ctxi = addtbinding ctx idi tyi in
                    let tyi = typeof ctxi tmi in
                    if tyi = rty then aux rest
                    else raise (Type_error "result type mismatch in case")
              in aux (List.tl cases)
            else
              raise (Type_error "variant and cases have different tags")
        | _ -> raise (Type_error "variant expected"))
    (* T-Proj *)
  | TmProj (t1,s) ->            (*Get corresponding value from tuple/record*)
    match typeof ctx t1 with
         TyTuple l -> (try List.nth l (int_of_string s -1) with
                      | _ -> raise (Type_error ("label " ^ s  ^ " not found"))) (*Label without any value*)
        | TyRecord l -> (try List.assoc s l with
                      | _ -> raise (Type_error ("label " ^ s  ^ " not found"))) (*label without any value*)
        | _ -> raise (Type_error "tuple/record type expected")
;;

(* TERMS MANAGEMENT (EVALUATION) *)

(*Function for printting the output nicely. Uses boxes from Format module*)
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
  | TmTag (idv, t, ty) ->
      open_hvbox 0;
      print_string("<" ^ idv ^ " =");
      print_space();
      pretty_printer t;
      print_string(">");
      close_box ()
  | TmCase (t, cases) ->
      open_vbox 0;
      print_string "case ";
      pretty_printer t;
      print_string " of";
      print_space ();
      List.iteri
        (fun i (label, var, body) ->
          if i > 0 then (
            print_space ();
            print_string "| ";
          ) else print_string "  ";
          print_string ("<" ^ label ^ " = " ^ var ^ "> => ");
          print_space ();
          string_of_atomicTerm body;
        )
        cases;
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
  | TmNil ty -> print_string("nil[" ^string_of_ty ty ^ "]");
  | TmCons (ty,h,t) -> 
    let aux = function
          TmNil _ -> print_string("cons[" ^ string_of_ty ty ^ "]"); print_space(); pretty_printer h; print_space(); pretty_printer t;
        | TmCons (_, subh, subt) -> print_string("cons[" ^ string_of_ty ty ^ "] ");  pretty_printer h; print_space(); print_string("("); pretty_printer t; print_string(")") 
        | _ -> raise (Failure "invalid pattern in TmCons aux function")(*esta bien?*)
      in aux t
  | TmIsNil (ty,t) -> print_string("isnil[" ^ string_of_ty ty ^ "]"); print_space(); print_string("("); pretty_printer t; print_string(")") 
  | TmHead (ty,t) -> print_string("head[" ^ string_of_ty ty ^ "]"); print_space(); print_string("("); pretty_printer t; print_string(")") 
  | TmTail (ty,t) -> print_string("tail[" ^ string_of_ty ty ^ "]"); print_space(); print_string("(");  pretty_printer t; print_string(")")
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
  | TmTuple s ->  (*prints every term separated by ',' and finally surrounded bu curly brackets*)
      let rec aux = function
          [] -> print_string("")
        | h :: [] -> pretty_printer h
        | h :: t -> pretty_printer h;
                    print_string(", ");
                    aux t
      in print_string("{"); aux s; print_string("}")
  | TmProj (t, s) ->  (*prints the projection*)
        pretty_printer t;
        print_string(".");
        print_string(s)
  | TmRecord s ->   (*prints every key next to an equal symbol assigning its value. Separated by commas*)
      let rec aux = function
          [] -> print_string("")
        | (i, h) :: [] -> print_string(i); print_string(" = "); pretty_printer h
        | (i, h) :: t -> print_string(i); print_string(" = "); pretty_printer h; print_string(", ");aux t
      in print_string("{"); aux s; print_string("}")
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
  | TmConcat (t1,t2) ->     (*Union of free vars from both terms*)
      lunion (free_vars t1) (free_vars t2)
  | TmTuple t ->            (*For every term from tuple get free vars and make union of all*)
    let rec aux l = 
      match l with
          h :: [] -> free_vars h
        | h :: t -> lunion (free_vars h) (aux t)
        | [] -> []
    in aux t
  | TmRecord t ->           (*For every pair from record get free vars and make union of all*)
    let rec aux l =
      match l with
          (i, h) :: [] -> free_vars h
        | (i, h) :: t -> lunion (free_vars h) (aux t)
        | [] -> []
    in aux t
  | TmProj (t, _) ->        (*Free vars from proj come from term*)
      free_vars t 
  | TmNil ty ->
      []
  | TmCons (ty,t1,t2) ->     (*Union of free vars from both terms*)
      lunion (free_vars t1) (free_vars t2)       
  | TmIsNil (ty,t) ->
      free_vars t   
  | TmHead (ty,t) ->
      free_vars t 
  | TmTail (ty,t) -> 
      free_vars t
  | TmTag (_, t, _) ->        (*Free vars from tag come from term*)
      free_vars t
  | TmCase (t, cases) ->      (*Union of free vars from term after case and free vars from cases*)
      lunion (free_vars t)
        (List.fold_left       (*Check that tags could appear in the right part from each case, neccessary to substract them from free vars*)
            (fun fv (lb, id, ti) -> lunion (ldif (free_vars ti) [id]) fv)
          [] cases)
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
      if y = x then tm    (*Not neccessary to substitute*)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)   (*Direct substitution*)
           else let z = fresh_name y (free_vars t @ fvs) in       (*Create new name for variable which is not in s nor in t*)
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))   (*Substitute with new name*)
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)   (*Substitue in t1 and not in t2 when variable from let is equal to substituting variable*)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)   (*Direct substitution if y does not belong to free vars*)
           else let z = fresh_name y (free_vars t2 @ fvs) in                (*Create new name which is not in s nor in t2*)
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2)) (*Substitute with new name*)
  | TmFix t -> 
      TmFix (subst x s t)
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmTuple t ->
      TmTuple (List.map (subst x s) t)  (*Apply substitution to every term from tuple*)
  | TmRecord t ->
      TmRecord (List.combine (List.map fst t) (List.map (subst x s) (List.map snd t))) (*Get keys, get values and for this list apply subst for each one, then combine resulting list with keys*)
  | TmProj (t1, t2) ->
      TmProj (subst x s t1, t2)
  | TmNil ty -> 
      tm
  | TmCons (ty,t1,t2) ->  
      TmCons (ty, (subst x s t1), (subst x s t2))
  | TmIsNil (ty,t) -> 
      TmIsNil (ty, (subst x s t))  
  | TmHead (ty,t) ->  
      TmHead (ty, (subst x s t))  
  | TmTail (ty,t) ->
      TmTail (ty, (subst x s t))
  | TmTag (s1, t, ty) ->    (*Substitute in term from tagging*)
      TmTag (s1, subst x s t, ty)
  | TmCase (t, cases) -> 
      let t' = subst x s t in   (*Substitute in term after "case"*)
      let cases' = List.map (fun (tag, v, case) ->    (*For each case apply subst*)
        if v = x then
          (tag, v, case)
        else
          let fvs = free_vars s in
          if not (List.mem v fvs) then   (*If v is not in s, safely substitute*)
            (tag, v, subst x s case)
          else
            let z = fresh_name v (free_vars case @ fvs) in  (*Create new name for variable which is not in s nor in case*)
            (tag, z, subst x s (subst v (TmVar z) case))    (*Substitute with new name*)
      ) cases in
      TmCase (t', cases')
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
  | TmString _ -> true     (*String is always a value*)
  | TmTuple l -> List.for_all (fun t -> isval t) l  (*Check if every element in tuple is valid*)
  | TmRecord l -> List.for_all (fun t -> isval t) (List.map snd l)  (*Check if every value in each pair from record is valid*)
  | TmNil _ -> true        (*Nil is always a value*)
  | TmCons(_,h,t) -> isval h && isval t (*Check both terms are values*)
  | TmTag(_, t, _) -> isval t (*Check if the term in tagging is a value*)
  | TmCase(t, cases) -> isval t && List.for_all (fun (_, _, t_case) -> isval t_case) cases (*Check if term after "case" is a value and for each term afer => check if it is a value*)
  | t when isnumericval t -> true   (*Numericals are values*)
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
    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'
    (* E-Concat *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)
    (* E-Concat2: eval second*)
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 ctx t2 in
      TmConcat (TmString s1, t2')
    (* E-Concat2: eval first*)
  | TmConcat (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmConcat ( t1', t2)
    (* E-Var *)
  | TmVar s ->
      getvbinding ctx s
    (* E-Tuple*)
  | TmTuple l -> (* Recursively evaluates each element of a tuple, leaving values unchanged and evaluating unevaluated terms. *)
      let rec aux = function
          h :: t when isval h -> let t' = aux t in h::t'
        | h :: t -> let h' = eval1 ctx h in h'::t
        | [] -> raise NoRuleApplies
      in TmTuple (aux l)
    (* E-Record *)
  | TmRecord l ->  (* Recursively evaluates each element of a record, leaving values unchanged and evaluating unevaluated terms. *)
      let rec aux = function
          (key, h) :: t when isval h -> let t' = aux t in (key, h) :: t'
        | (key, h) :: t -> let h' = eval1 ctx h in (key, h') :: t
        | [] -> raise NoRuleApplies
      in TmRecord (aux l)
     (* E-Proj-Tuple *)
  | TmProj (TmTuple l as v, s) when isval v ->  (*Evaluate nth element from tuple*)
      List.nth l (int_of_string s - 1)
     (* E-Proj-Record *)
  | TmProj (TmRecord l as v, s) when isval v -> (*Evaluate correspinding element from record*)
      List.assoc s l
      (* E-Proj *)
  | TmProj (t, s) ->
      let t' = eval1 ctx t in TmProj (t', s)
    (*E-Cons2*)
  |TmCons(ty,h,t) when isval h -> 
      TmCons(ty,h,(eval1 ctx t))
    (*E-Cons1*)
  |TmCons(ty,h,t) -> 
      TmCons(ty,(eval1 ctx h),t)
    (*E-IsNilNil*)
  |TmIsNil(ty,TmNil(_)) -> 
      TmTrue
    (*E-IsNilCons*)
  |TmIsNil(ty,TmCons(_,_,_)) -> 
      TmFalse
    (*E-IsNil*)
  |TmIsNil(ty,t) -> 
      TmIsNil(ty,eval1 ctx t)
    (*E-HeadCons*)
  |TmHead(ty,TmCons(_,h,_))-> 
      h
    (*E-Head*)
  |TmHead(ty,t) -> 
      TmHead(ty,eval1 ctx t)
    (*E-TailCons*)
  |TmTail(ty,TmCons(_,_,t)) -> 
      t
    (*E-Tail*)
  |TmTail(ty,t) -> 
      TmTail(ty,eval1 ctx t)
    (* E-Tag *)
  | TmTag (s, t, ty) when not (isval t) ->
      let t' = eval1 ctx t in
      TmTag (s, t', ty)
    (* E-Case *)
  | TmCase (TmTag (s, v, ty), cases) when isval v ->
      let rec aux = function
      | [] -> raise NoRuleApplies
      | (t, var, b) :: rest when t = s -> subst var v b   (*Apply subst when t maches s from case*)
      | _ :: rest -> aux rest
    in aux cases
    (* E-Case2 *)
  | TmCase (t, cases) ->
      let t' = eval1 ctx t in
      TmCase (t', cases)
  | _ ->
      raise NoRuleApplies
;;

(*apply_ctx performs a substitution on the term tm for all free variables using the values associated with those variables in the context ctx. *)
let apply_ctx ctx tm =
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)
;;

(*eval keeps evaluating a term step-by-step using eval1 until no more evaluation rules apply. 
Once no more rules apply, it performs a substitution of free variables using the context*)
let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;


let execute ctx = function 
    Eval tm ->    (*Perform evaluation*)
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      Format.open_hvbox 0;   (*Opening box*)
      print_string ("- : " ^ string_of_ty tyTm ^ " =");
      Format.print_space();
      pretty_printer (tm');
      Format.close_box ();
      Format.print_flush(); (*Clean boxes*)
      print_newline();
      ctx
  | Bind (s, tm) ->        (*Bind value to variable*)
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      Format.open_hvbox 0;  (*Opening box*)
      print_string (s ^ " : " ^ string_of_ty tyTm ^ " =");
      Format.print_space();
      pretty_printer (tm');
      Format.close_box ();
      Format.print_flush(); (*Clean boxes*)
      print_newline();
      addvbinding ctx s tyTm tm'  (*Add new value to context*)
  | TBind (s, ty) ->        (*Bind type to variable*)
      let bty = typeofTy ctx ty in  (*Get base type for assigning this basic type later, for simplicity*)
      Format.open_hvbox 0;   (*Opening box*)
      print_string ("type " ^ s ^ " =");
      Format.print_space();
      print_string(string_of_ty bty);
      Format.close_box ();
      Format.print_flush(); (*Clean boxes*)
      print_newline();
      addtbinding ctx s bty       (*Add new type to context*)
  | Quit ->                  (*Exit*)
      raise End_of_file