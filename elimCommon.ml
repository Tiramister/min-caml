let rec g env expr =
  match expr with
  (* apply recursively *)
  | KNormal.IfEq (x, y, et, ef) ->
     KNormal.IfEq (x, y, g env et, g env ef)
  | KNormal.IfLE (x, y, et, ef) ->
     KNormal.IfLE (x, y, g env et, g env ef)
  | KNormal.LetTuple (xs, y, e) ->
     KNormal.LetTuple (xs, y, g env e)
    
  | KNormal.Let ((x, ty), b, e) ->
     (* find a variable whose body is equivalent to b *)
     let matched = M.filter
           (fun y b' -> KNormal.same_expr b b') env in
     let newbody =
       if M.is_empty matched then b
       else let (y, _) = M.choose matched in KNormal.Var(y) in
     (* if found, replace b with the variable *)
     let newenv = M.add x newbody env in
     (* apply recursively *)
     KNormal.Let ((x, ty), newbody, g newenv e)

  | KNormal.LetRec (fdef, e) ->
     let (x, _) = fdef.name in
     let matched = M.filter
           (fun y b' -> KNormal.same_expr fdef.body b') env in
     let newbody =
       if M.is_empty matched then fdef.body
       else let (y, _) = M.choose matched in KNormal.Var(y) in
     let newenv = M.add x newbody env in
     KNormal.LetRec ({fdef with body = newbody}, g newenv e)

  | _ -> expr

let f expr = g M.empty expr
