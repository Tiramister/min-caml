type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

let rec string_of_type ty =
  match ty with
  | Unit  -> "UNIT"
  | Bool  -> "BOOL"
  | Int   -> "INT"
  | Float -> "FLOAT"
  | Fun (tys, ty') ->
     let args =
       (String.concat ", "
          (List.map string_of_type tys)) in
     "(" ^ args ^ ") -> " ^ (string_of_type ty')
  | Tuple tys ->
     let args =
       (String.concat ", "
          (List.map string_of_type tys)) in
     "(" ^ args ^ ")"
  | Array ty' -> "ARRAY OF " ^ (string_of_type ty')
  | Var ty' ->
     match !ty' with
     | None   -> "UNKNOWN"
     | Some t -> string_of_type t
