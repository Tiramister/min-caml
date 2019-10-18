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
(* return 2n spaces *)
let rec print depth s =
  if depth = 0
  then
    print_endline s
  else
    (print_string "  ";
     print (depth - 1) s)

(* print a variable with its type and indentations *)
let print_var depth (name, ty) =
  print depth (name ^ " : " ^ (Type.string_of_type ty))

(* print a function and its arguments *)
let rec print_func depth printer name exprs =
  print depth name;
  List.iter (printer (depth + 1)) exprs

(* print an if statement *)
and print_if depth printer cond et ef =
  print depth "IF";
  printer (depth + 1) cond;
  print depth "THEN";
  printer (depth + 1) et;
  print depth "ELSE";
  printer (depth + 1) ef

(* print a let statement *)
and print_let depth printer vars e1 e2 =
  print depth "LET";
  List.iter (print_var (depth + 1)) vars;
  print depth "=";
  printer (depth + 1) e1;
  print depth "IN";
  printer (depth + 1) e2

and print_syntax_inner depth expr =
  let print_func_local = print_func depth print_syntax_inner in
  let print_if_local = print_if depth print_syntax_inner in
  let print_let_local = print_let depth print_syntax_inner in
  
  match expr with
  | Unit ->
     print_func_local "UNIT" []
  | Bool b ->
     print_func_local (if b then "TRUE" else "FALSE") []
  | Int i ->
     print_func_local (string_of_int i) []
  | Float f ->
     print_func_local (string_of_float f) []
  | Not e ->
     print_func_local "NOT" [e]
  | Neg e ->
     print_func_local "NEG" [e]
  | Add (e1, e2) ->
     print_func_local "ADD" [e1; e2]
  | Sub (e1, e2) ->
     print_func_local "SUB" [e1; e2]
  | FNeg e ->
     print_func_local "FNEG" [e]
  | FAdd (e1, e2) ->
     print_func_local "FADD" [e1; e2]
  | FSub (e1, e2) ->
     print_func_local "FSUB" [e1; e2]
  | FMul (e1, e2) ->
     print_func_local "FMUL" [e1; e2]
  | FDiv (e1, e2) ->
     print_func_local "FDIV" [e1; e2]
  | Eq (e1, e2) ->
     print_func_local "EQ" [e1; e2]
  | LE (e1, e2) ->
     print_func_local "LE" [e1; e2]
  | If (e1, e2, e3) ->
     print_if_local e1 e2 e3
  | Let (var, e1, e2) ->
     print_let_local [var] e1 e2
  | Var name ->
     print_func_local ("VAR " ^ name) []
  | LetRec (fdef, e) ->
     print_let_local [fdef.name] fdef.body e
  | App (e, es) ->
     print_func_local "APP" (e :: es)
  | Tuple es ->
     print_func_local "TUPLE" es
  | LetTuple (vars, e1, e2) ->
     print_let_local vars e1 e2
  | Array (e1, e2) ->
     print_func_local "ARRAY" [e1; e2]
  | Get (e1, e2) ->
     print_func_local "GET" [e1; e2]
  | Put (e1, e2, e3) ->
     print_func_local "PUT" [e1; e2; e3]

let print_syntax = print_syntax_inner 0
