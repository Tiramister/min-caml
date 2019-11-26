(* type inference/reconstruction *)

open Syntax

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t

let extenv = ref M.empty

(* for pretty printing (and type normalization) *)
let rec deref_typ = function (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  | Type.Fun(t1s, t2) -> Type.Fun(List.map deref_typ t1s, deref_typ t2)
  | Type.Tuple(ts) -> Type.Tuple(List.map deref_typ ts)
  | Type.Array(t) -> Type.Array(deref_typ t)
  | Type.Var({ contents = None } as r) ->
      Format.eprintf "uninstantiated type variable detected; assuming int@.";
      r := Some(Type.Int);
      Type.Int
  | Type.Var({ contents = Some(t) } as r) ->
      let t' = deref_typ t in
      r := Some(t');
      t'
  | t -> t
let rec deref_id_typ (x, t) = (x, deref_typ t)
let rec deref_term = function
  | Not(e) -> Not(deref_term e)
  | Neg(e) -> Neg(deref_term e)
  | Add(e1, e2) -> Add(deref_term e1, deref_term e2)
  | Sub(e1, e2) -> Sub(deref_term e1, deref_term e2)
  | Eq(e1, e2) -> Eq(deref_term e1, deref_term e2)
  | LE(e1, e2) -> LE(deref_term e1, deref_term e2)
  | FNeg(e) -> FNeg(deref_term e)
  | FAdd(e1, e2) -> FAdd(deref_term e1, deref_term e2)
  | FSub(e1, e2) -> FSub(deref_term e1, deref_term e2)
  | FMul(e1, e2) -> FMul(deref_term e1, deref_term e2)
  | FDiv(e1, e2) -> FDiv(deref_term e1, deref_term e2)
  | If(e1, e2, e3) -> If(deref_term e1, deref_term e2, deref_term e3)
  | Let(xt, e1, e2) -> Let(deref_id_typ xt, deref_term e1, deref_term e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = deref_id_typ xt;
               args = List.map deref_id_typ yts;
               body = deref_term e1 },
             deref_term e2)
  | App(e, es) -> App(deref_term e, List.map deref_term es)
  | Tuple(es) -> Tuple(List.map deref_term es)
  | LetTuple(xts, e1, e2) -> LetTuple(List.map deref_id_typ xts, deref_term e1, deref_term e2)
  | Array(e1, e2) -> Array(deref_term e1, deref_term e2)
  | Get(e1, e2) -> Get(deref_term e1, deref_term e2)
  | Put(e1, e2, e3) -> Put(deref_term e1, deref_term e2, deref_term e3)
  | e -> e

let rec occur r1 = function (* occur check (caml2html: typing_occur) *)
  | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Tuple(t2s) -> List.exists (occur r1) t2s
  | Type.Array(t2) -> occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec unify t1 t2 = (* 型が合うように、型変数への代入をする (caml2html: typing_unify) *)
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument(_) -> raise (Unify(t1, t2)));
      unify t1' t2'
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument(_) -> raise (Unify(t1, t2)))
  | Type.Array(t1), Type.Array(t2) -> unify t1 t2
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ -> (* 一方が未定義の型変数の場合 (caml2html: typing_undef) *)
      if occur r1 t2 then raise (Unify(t1, t2));
      r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2) ->
      if occur r2 t1 then raise (Unify(t1, t2));
      r2 := Some(t1)
  | _, _ -> raise (Unify(t1, t2))

let rec g env e = (* 型推論ルーチン (caml2html: typing_g) *)
  try
    match e with
    | Unit -> Type.Unit, e
    | Bool(_) -> Type.Bool, e
    | Int(_) -> Type.Int, e
    | Float(_) -> Type.Float, e

    | Not(e) ->
        let t, ne = g env e in
        unify Type.Bool t;
        Type.Bool, Not(ne)

    | Neg(e) ->
       let t, ne = g env e in
       if t == Type.Float then
         Type.Float, Neg(ne)
       else
         (unify Type.Int t;
          Type.Int, Neg(ne))

    | Add(e1, e2) ->
       let t1, ne1 = g env e1 in
       let t2, ne2 = g env e2 in
       if t1 == Type.Float || t2 == Type.Float then
         (* infer as float *)
         (unify Type.Float t1;
          unify Type.Float t2;
          Type.Float, FAdd(ne1, ne2))
       else
         (* infer as int *)
         (unify Type.Int t1;
          unify Type.Int t2;
          Type.Int, Add(ne1, ne2))

    | Sub(e1, e2) ->
       let t1, ne1 = g env e1 in
       let t2, ne2 = g env e2 in
       if t1 == Type.Float || t2 == Type.Float then
         (* infer as float *)
         (unify Type.Float t1;
          unify Type.Float t2;
          Type.Float, FSub(ne1, ne2))
       else
         (* infer as int *)
         (unify Type.Int t1;
          unify Type.Int t2;
          Type.Int, Sub(ne1, ne2))

    | FNeg(e) ->
        let t, ne = g env e in
        unify Type.Float t;
        Type.Float, FNeg(ne)

    | FAdd(e1, e2) ->
        let t1, ne1 = g env e1 in
        let t2, ne2 = g env e2 in
        unify Type.Float t1;
        unify Type.Float t2;
        Type.Float, FAdd(ne1, ne2)

    | FSub(e1, e2) ->
        let t1, ne1 = g env e1 in
        let t2, ne2 = g env e2 in
        unify Type.Float t1;
        unify Type.Float t2;
        Type.Float, FSub(ne1, ne2)

    | FMul(e1, e2) ->
        let t1, ne1 = g env e1 in
        let t2, ne2 = g env e2 in
        unify Type.Float t1;
        unify Type.Float t2;
        Type.Float, FMul(ne1, ne2)
        
    | FDiv(e1, e2) ->
        let t1, ne1 = g env e1 in
        let t2, ne2 = g env e2 in
        unify Type.Float t1;
        unify Type.Float t2;
        Type.Float, FDiv(ne1, ne2)

    | Eq(e1, e2) ->
        let t1, ne1 = g env e1 in
        let t2, ne2 = g env e2 in
        unify t1 t2;
        Type.Bool, Eq(ne1, ne2)

    | LE(e1, e2) ->
        let t1, ne1 = g env e1 in
        let t2, ne2 = g env e2 in
        unify t1 t2;
        Type.Bool, LE(ne1, ne2)

    | If(e1, e2, e3) ->
        let t1, ne1 = g env e1 in
        let t2, ne2 = g env e2 in
        let t3, ne3 = g env e3 in
        unify Type.Bool t1;
        unify t2 t3;
        t2, If(ne1, ne2, ne3)

    | Let((x, t), e1, e2) -> (* letの型推論 (caml2html: typing_let) *)
        let t1, ne1 = g env e1 in
        unify t t1;
        let t2, ne2 = g (M.add x t env) e2 in
        t2, Let((x, t), ne1, ne2)

    | Var(x) when M.mem x env ->
        let t = M.find x env in
        t, Var(x)

    | Var(x) when M.mem x !extenv ->
        let t = M.find x !extenv in
        t, Var(x)

    | Var(x) ->
        Format.eprintf "free variable %s assumed as external@." x;
        let t = Type.gentyp () in
        extenv := M.add x t !extenv;
        t, Var(x)

    | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recの型推論 (caml2html: typing_letrec) *)
        let env = M.add x t env in
        let t1, ne1 = g (M.add_list yts env) e1 in
        unify t (Type.Fun(List.map snd yts, t1));
        let t2, ne2 = g env e2 in
        t2, LetRec({ name = (x, t); args = yts; body = ne1 }, ne2)

    | App(e, es) -> (* 関数適用の型推論 (caml2html: typing_app) *)
        let nt = Type.gentyp () in
        let t, ne = g env e in
        let ts, nes = List.split (List.map (g env) es) in
        unify t (Type.Fun(ts, nt));
        nt, App(ne, nes)

    | Tuple(es) ->
        let ts, nes = List.split (List.map (g env) es) in
        Type.Tuple(ts), Tuple(nes)

    | LetTuple(xts, e1, e2) ->
        let t1, ne1 = g env e1 in
        unify (Type.Tuple(List.map snd xts)) t1;
        let t2, ne2 = g (M.add_list xts env) e2 in
        t2, LetTuple(xts, ne1, ne2)

    | Array(e1, e2) -> (* must be a primitive for "polymorphic" typing *)
        let t1, ne1 = g env e1 in
        unify Type.Int t1;
        let t2, ne2 = g env e2 in
        Type.Array(t2), Array(ne1, ne2)

    | Get(e1, e2) ->
        let t = Type.gentyp () in
        let t1, ne1 = g env e1 in
        unify (Type.Array(t)) t1;
        let t2, ne2 = g env e2 in
        unify Type.Int t2;
        t, Get(ne1, ne2)

    | Put(e1, e2, e3) ->
        let t1, ne1 = g env e1 in
        let t2, ne2 = g env e2 in
        let t3, ne3 = g env e3 in
        unify (Type.Array(t3)) t1;
        unify Type.Int t2;
        Type.Unit, Put(ne1, ne2, ne3)

  with Unify(t1, t2) -> raise (Error(deref_term e, deref_typ t1, deref_typ t2))

let f e =
  extenv := M.empty;
  let t, ne = g M.empty e in
  (try
     unify Type.Unit t
  with Unify _ -> failwith "top level does not have type unit");
  extenv := M.map deref_typ !extenv;
  deref_term ne
