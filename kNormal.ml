(* give names to intermediate values (K-normalization) *)

type t = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
 
let rec same_expr expr1 expr2 =
  match (expr1, expr2) with
  (* require recursive calls *)
  | (IfEq (x1, y1, e1, f1), IfEq (x2, y2, e2, f2))
    -> x1 = x2
       && y1 = y2
       && (same_expr e1 e2)
       && (same_expr f1 f2)
  | (IfLE (x1, y1, e1, f1), IfLE (x2, y2, e2, f2))
    -> x1 = x2
       && y1 = y2
       && (same_expr e1 e2)
       && (same_expr f1 f2)
  | (Let (x1, e1, f1), Let (x2, e2, f2))
    -> x1 = x2
       && (same_expr e1 e2)
       && (same_expr f1 f2)
  | (LetRec (fdef1, e1), LetRec (fdef2, e2))
    -> fdef1.name = fdef2.name
       && fdef1.args = fdef2.args
       && (same_expr fdef1.body fdef2.body)
       && (same_expr e1 e2)
  | (LetTuple (xs1, x1, e1), LetTuple (xs2, x2, e2))
    -> xs1 = xs2
       && x1 = x2
       && (same_expr e1 e2)
  (* side effects *)
  | (Get _, Get _) -> false
  | (Put _, Put _) -> false
  | (ExtArray _, ExtArray _)   -> false
  | (ExtFunApp _, ExtFunApp _) -> false
  (* leave the rest to default comparator *)
  | _ -> expr1 = expr2


let rec fv = function (* 式に出現する（自由な）変数 (caml2html: knormal_fv) *)
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))

let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
      Let((x, t), e, e'), t'

let rec g env = function (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Bool(b) -> Int(if b then 1 else 0), Type.Int (* 論理値true, falseを整数1, 0に変換 (caml2html: knormal_bool) *)
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Float(d) -> Float(d), Type.Float
  | Syntax.Not(e) -> g env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
  | Syntax.Neg(e) ->
      insert_let (g env e)
        (fun x -> Neg(x), Type.Int)
  | Syntax.Add(e1, e2) -> (* 足し算のK正規化 (caml2html: knormal_add) *)
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> Add(x, y), Type.Int))
  | Syntax.Sub(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> Sub(x, y), Type.Int))
  | Syntax.FNeg(e) ->
      insert_let (g env e)
        (fun x -> FNeg(x), Type.Float)
  | Syntax.FAdd(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FAdd(x, y), Type.Float))
  | Syntax.FSub(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FSub(x, y), Type.Float))
  | Syntax.FMul(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FMul(x, y), Type.Float))
  | Syntax.FDiv(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FDiv(x, y), Type.Float))
  | Syntax.Eq _ | Syntax.LE _ as cmp ->
      g env (Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
  | Syntax.If(Syntax.Not(e1), e2, e3) -> g env (Syntax.If(e1, e3, e2)) (* notによる分岐を変換 (caml2html: knormal_not) *)
  | Syntax.If(Syntax.Eq(e1, e2), e3, e4) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              IfEq(x, y, e3', e4'), t3))
  | Syntax.If(Syntax.LE(e1, e2), e3, e4) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              IfLE(x, y, e3', e4'), t3))
  | Syntax.If(e1, e2, e3) -> g env (Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2)) (* 比較のない分岐を変換 (caml2html: knormal_if) *)
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
      Let((x, t), e1', e2'), t2
  | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
  | Syntax.Var(x) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> ExtArray x, t
      | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
      LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f env) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
      (match M.find f !Typing.extenv with
      | Type.Fun(_, t) ->
          let rec bind xs = function (* "xs" are identifiers for the arguments *)
            | [] -> ExtFunApp(f, xs), t
            | e2 :: e2s ->
                insert_let (g env e2)
                  (fun x -> bind (xs @ [x]) e2s) in
          bind [] e2s (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.App(e1, e2s) ->
      (match g env e1 with
      | _, Type.Fun(_, t) as g_e1 ->
          insert_let g_e1
            (fun f ->
              let rec bind xs = function (* "xs" are identifiers for the arguments *)
                | [] -> App(f, xs), t
                | e2 :: e2s ->
                    insert_let (g env e2)
                      (fun x -> bind (xs @ [x]) e2s) in
              bind [] e2s) (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.Tuple(es) ->
      let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
        | [] -> Tuple(xs), Type.Tuple(ts)
        | e :: es ->
            let _, t as g_e = g env e in
            insert_let g_e
              (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
      bind [] [] es
  | Syntax.LetTuple(xts, e1, e2) ->
      insert_let (g env e1)
        (fun y ->
          let e2', t2 = g (M.add_list xts env) e2 in
          LetTuple(xts, y, e2'), t2)
  | Syntax.Array(e1, e2) ->
      insert_let (g env e1)
        (fun x ->
          let _, t2 as g_e2 = g env e2 in
          insert_let g_e2
            (fun y ->
              let l =
                match t2 with
                | Type.Float -> "create_float_array"
                | _ -> "create_array" in
              ExtFunApp(l, [x; y]), Type.Array(t2)))
  | Syntax.Get(e1, e2) ->
      (match g env e1 with
      |        _, Type.Array(t) as g_e1 ->
          insert_let g_e1
            (fun x -> insert_let (g env e2)
                (fun y -> Get(x, y), t))
      | _ -> assert false)
  | Syntax.Put(e1, e2, e3) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> insert_let (g env e3)
                (fun z -> Put(x, y, z), Type.Unit)))

let f e = fst (g M.empty e)


(* ========== Debug ========== *)
let rec print_knormal_inner depth expr =
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
    List.iter (print (depth + 1)) exprs in

  let print_if var1 comp var2 et ef =
    print depth "IF";
    print (depth + 1) var1;
    print depth comp;
    print (depth + 1) var2;
    print depth "THEN";
    print_knormal_inner (depth + 1) et;
    print depth "ELSE";
    print_knormal_inner (depth + 1) ef in

  let print_let vars e1 e2 =
    print depth "LET";
    List.iter (print_var (depth + 1)) vars;
    print depth "=";
    print_knormal_inner (depth + 1) e1;
    print depth "IN";
    print_knormal_inner (depth + 1) e2 in

  let print_letrec fdef e =
    print depth "LET";
    print_var (depth + 1) fdef.name;
    print depth "ARGS";
    List.iter (print_var (depth + 1)) fdef.args;
    print depth "=";
    print_knormal_inner (depth + 1) fdef.body;
    print depth "IN";
    print_knormal_inner (depth + 1) e in

  let print_lettuple vars var e =
    print depth "LET";
    List.iter (print_var (depth + 1)) vars;
    print depth "=";
    print (depth + 1) var;
    print depth "IN";
    print_knormal_inner (depth + 1) e in
  
  match expr with
  | Unit ->
     print_func "UNIT" []
  | Int i ->
     print_func (string_of_int i) []
  | Float f ->
     print_func (string_of_float f) []
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
  | IfEq (var1, var2, et, ef) ->
     print_if var1 "==" var2 et ef
  | IfLE (var1, var2, et, ef) ->
     print_if var1 "<" var2 et ef
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
  | LetTuple (vars, var, e) ->
     print_lettuple vars var e
  | Get (e1, e2) ->
     print_func "GET" [e1; e2]
  | Put (e1, e2, e3) ->
     print_func "PUT" [e1; e2; e3]
  | ExtArray var ->
     print_func ("ARRAY " ^ var) []
  | ExtFunApp (f, args) ->
     print_func "APP" (f :: args)

let print_knormal = print_knormal_inner 0
