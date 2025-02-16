type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* クロージャ変換後の式 (caml2html: closure_t) *)
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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
type fundef = { name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t }
type prog = Prog of fundef list * t

let rec fv = function
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) | Tuple(xs) -> S.of_list xs
  | LetTuple(xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put(x, y, z) -> S.of_list [x; y; z]

let toplevel : fundef list ref = ref []

let rec g env known = function (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Float(d) -> Float(d)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (M.add x t env) known e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* 関数定義の場合 (caml2html: closure_letrec) *)
      (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
         xに自由変数がない(closureを介さずdirectに呼び出せる)
         と仮定し、knownに追加してe1をクロージャ変換してみる *)
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' e1 in
      (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
      (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) *)
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let known', e1' =
        if S.is_empty zs then known', e1' else
        (* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
        (Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
         Format.eprintf "function %s cannot be directly applied in fact@." x;
         toplevel := toplevel_backup;
         let e1' = g (M.add_list yts env') known e1 in
         known, e1') in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* 自由変数のリスト *)
      let zts = List.map (fun z -> (z, M.find z env')) zs in (* ここで自由変数zの型を引くために引数envが必要 *)
      toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* トップレベル関数を追加 *)
      let e2' = g env' known' e2 in
      if S.mem x (fv e2') then (* xが変数としてe2'に出現するか *)
        MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* 出現していたら削除しない *)
      else
        (Format.eprintf "eliminating closure(s) %s@." x;
         e2') (* 出現しなければMakeClsを削除 *)
  | KNormal.App(x, ys) when S.mem x known -> (* 関数適用の場合 (caml2html: closure_app) *)
      Format.eprintf "directly applying %s@." x;
      AppDir(Id.L(x), ys)
  | KNormal.App(f, xs) -> AppCls(f, xs)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (M.add_list xts env) known e)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(Id.L(x))
  | KNormal.ExtFunApp(x, ys) -> AppDir(Id.L("min_caml_" ^ x), ys)

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')


(* ========== Debug ========== *)
let rec print_closure_inner depth expr =
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
    print_closure_inner (depth + 1) et;
    print depth "ELSE";
    print_closure_inner (depth + 1) ef in

  let print_let vars e1 e2 =
    print depth "LET";
    List.iter (print_var (depth + 1)) vars;
    print depth "=";
    print_closure_inner (depth + 1) e1;
    print depth "IN";
    print_closure_inner (depth + 1) e2 in

  let print_makecls var {entry = L(label); actual_fv = fvs} e =
    print depth "LET";
    print_var (depth + 1) var;
    print depth "=";
    print (depth + 1) ("LABEL = " ^ label);
    print (depth + 1) ("FVS   = " ^
                         (String.concat ", " fvs));
    print depth "IN";
    print_closure_inner (depth + 1) e in

  let print_appdir lb vars =
    let Id.L(label) = lb in
    print depth "APPDIR";
    print (depth + 1) ("LABEL : " ^ label);
    List.iter (print (depth + 1)) vars in
  
  let print_lettuple vars var e =
    print depth "LET";
    List.iter (print_var (depth + 1)) vars;
    print depth "=";
    print (depth + 1) var;
    print depth "IN";
    print_closure_inner (depth + 1) e in
  
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
  | MakeCls (var, cls, e) ->
     print_makecls var cls e
  | AppCls (e, es) ->
     print_func "APPCLS" (e :: es)
  | AppDir (e, es) ->
     print_appdir e es
  | Tuple es ->
     print_func "TUPLE" es
  | LetTuple (vars, var, e) ->
     print_lettuple vars var e
  | Get (e1, e2) ->
     print_func "GET" [e1; e2]
  | Put (e1, e2, e3) ->
     print_func "PUT" [e1; e2; e3]
  | ExtArray Id.L(label) ->
     print_func ("ARRAY " ^ label) []

let print_toplevel fundefs =
  List.iter
    (fun {name = (Id.L(label), ty);
          args = args;
          formal_fv = fvs;
          body = e} ->
      print_endline ("LABEL : " ^ label);
      print_endline ("TYPE  : " ^ (Type.string_of_type ty));
      print_endline ("ARGS  : " ^ (String.concat ", " (List.map fst args)));
      print_endline ("FVS   : " ^ (String.concat ", " (List.map fst fvs)));
      print_endline ("BODY  : ");
      print_closure_inner 1 e;
      print_endline ("------------------------------")
    ) fundefs

let print_closure (Prog (top, expr)) =
  print_toplevel top;
  print_closure_inner 0 expr
