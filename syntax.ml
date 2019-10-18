type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }


(* ========== Debug ========== *)
let rec print_syntax_inner depth expr =
  (* return 2n spaces *)
  let rec print depth s =
    if depth = 0
    then
      print_endline s
    else
      (print_string "  ";
       print (depth - 1) s) in

  (* print a variable with its type and indentations *)
  let print_var depth (name, ty) =
    print depth (name ^ " : " ^ (Type.string_of_type ty)) in

  (* print a function and its arguments *)
  let rec print_func name exprs =
    print depth name;
    List.iter (print_syntax_inner (depth + 1)) exprs in

  let print_if cond et ef =
    print depth "IF";
    print_syntax_inner (depth + 1) cond;
    print depth "THEN";
    print_syntax_inner (depth + 1) et;
    print depth "ELSE";
    print_syntax_inner (depth + 1) ef in

  let print_let vars e1 e2 =
    print depth "LET";
    List.iter (print_var (depth + 1)) vars;
    print depth "=";
    print_syntax_inner (depth + 1) e1;
    print depth "IN";
    print_syntax_inner (depth + 1) e2 in

  let print_letrec fdef e =
    print depth "LET";
    print_var (depth + 1) fdef.name;
    print depth "ARGS";
    List.iter (print_var (depth + 1)) fdef.args;
    print depth "=";
    print_syntax_inner (depth + 1) fdef.body;
    print depth "IN";
    print_syntax_inner (depth + 1) e in
  
  match expr with
  | Unit ->
     print_func "UNIT" []
  | Bool b ->
     print_func (if b then "TRUE" else "FALSE") []
  | Int i ->
     print_func (string_of_int i) []
  | Float f ->
     print_func (string_of_float f) []
  | Not e ->
     print_func "NOT" [e]
  | Neg e ->
     print_func "NEG" [e]
  | Add (e1, e2) ->
     print_func "ADD" [e1; e2]
  | Sub (e1, e2) ->
     print_func "SUB" [e1; e2]
  | FNeg e ->
     print_func "FNEG" [e]
  | FAdd (e1, e2) ->
     print_func "FADD" [e1; e2]
  | FSub (e1, e2) ->
     print_func "FSUB" [e1; e2]
  | FMul (e1, e2) ->
     print_func "FMUL" [e1; e2]
  | FDiv (e1, e2) ->
     print_func "FDIV" [e1; e2]
  | Eq (e1, e2) ->
     print_func "EQ" [e1; e2]
  | LE (e1, e2) ->
     print_func "LE" [e1; e2]
  | If (e1, e2, e3) ->
     print_if e1 e2 e3
  | Let (var, e1, e2) ->
     print_let [var] e1 e2
  | Var name ->
     print_func ("VAR " ^ name) []
  | LetRec (fdef, e) ->
     print_letrec fdef e
  | App (e, es) ->
     print_func "APP" (e :: es)
  | Tuple es ->
     print_func "TUPLE" es
  | LetTuple (vars, e1, e2) ->
     print_let vars e1 e2
  | Array (e1, e2) ->
     print_func "ARRAY" [e1; e2]
  | Get (e1, e2) ->
     print_func "GET" [e1; e2]
  | Put (e1, e2, e3) ->
     print_func "PUT" [e1; e2; e3]

let print_syntax = print_syntax_inner 0
