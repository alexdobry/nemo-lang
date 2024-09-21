type expr =
  | Var of string
  | Abs of string*expr
  | App of expr*expr

let rec fvars expr = match expr with
  | Var ident -> [ident]
  | Abs (binder, body) -> List.filter (fun x -> x != binder) (fvars body)
  | App (func, arg) -> fvars func @ fvars arg |> List.sort_uniq String.compare

let v s = Var(s)
let abs b e = Abs(b, e)
let app f a = App(f, a)

let () =
  let e = abs "hallo" (app (v "hallo") (v "world")) in
  let res = fvars e in
  List.iter print_endline res
