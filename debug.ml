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
  | Syntax.Unit ->
     print_func_local "UNIT" []
  | Syntax.Bool b ->
     print_func_local (if b then "TRUE" else "FALSE") []
  | Syntax.Int i ->
     print_func_local (string_of_int i) []
  | Syntax.Float f ->
     print_func_local (string_of_float f) []
  | Syntax.Not e ->
     print_func_local "NOT" [e]
  | Syntax.Neg e ->
     print_func_local "NEG" [e]
  | Syntax.Add (e1, e2) ->
     print_func_local "ADD" [e1; e2]
  | Syntax.Sub (e1, e2) ->
     print_func_local "SUB" [e1; e2]
  | Syntax.FNeg e ->
     print_func_local "FNEG" [e]
  | Syntax.FAdd (e1, e2) ->
     print_func_local "FADD" [e1; e2]
  | Syntax.FSub (e1, e2) ->
     print_func_local "FSUB" [e1; e2]
  | Syntax.FMul (e1, e2) ->
     print_func_local "FMUL" [e1; e2]
  | Syntax.FDiv (e1, e2) ->
     print_func_local "FDIV" [e1; e2]
  | Syntax.Eq (e1, e2) ->
     print_func_local "EQ" [e1; e2]
  | Syntax.LE (e1, e2) ->
     print_func_local "LE" [e1; e2]
  | Syntax.If (e1, e2, e3) ->
     print_if_local e1 e2 e3
  | Syntax.Let (var, e1, e2) ->
     print_let_local [var] e1 e2
  | Syntax.Var name ->
     print_func_local ("VAR " ^ name) []
  | Syntax.LetRec (fdef, e) ->
     print_let_local [fdef.name] fdef.body e
  | Syntax.App (e, es) ->
     print_func_local "APP" (e :: es)
  | Syntax.Tuple es ->
     print_func_local "TUPLE" es
  | Syntax.LetTuple (vars, e1, e2) ->
     print_let_local vars e1 e2
  | Syntax.Array (e1, e2) ->
     print_func_local "ARRAY" [e1; e2]
  | Syntax.Get (e1, e2) ->
     print_func_local "GET" [e1; e2]
  | Syntax.Put (e1, e2, e3) ->
     print_func_local "PUT" [e1; e2; e3]

let print_syntax = print_syntax_inner 0
