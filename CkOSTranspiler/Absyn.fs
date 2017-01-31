(*
  Abstract syntax of the CkOS language.
  jvoi@itu.dk 30-01-2017
*)
module Absyn

(*
  Indication of the proximity (scope) of the variable.
*)
type proximity = 
  | Local
  | Global

(*
  CkOS language expressions.
*)
type expr =
  | Access of access
  | CstN of float
  | CstS of string
  | CstB of bool
  | Call of string * expr list
  | Prim1 of string * expr
  | Prim2 of string * expr * expr
  | Andalso of expr * expr
  | Orelse of expr * expr
  | PreInc of access
  | PreDec of access
  | PostInc of access
  | PostDec of access
(*
  Access expressions.
*)
and access =
  | AccVar of string (* Access a variable *)
  | AccMem of string * string (* Access a member of a struct *)
  | AccFun of string (* Acces a function as a delegate *)
  | AccIndex of access * expr (* Access an entry in a datastructure by index *)
and stmt = 
  | If of expr * stmt * stmt
  | Break
  | Block of stmtordec list
  | DoWhile of stmt * expr
  | While of expr * stmt
  | Lock of string * expr
  | Unlock of string
  | ForIn of string * string * stmt
  | For of stmtordec * expr * stmt * stmt //for (var i = 0, i < 5, i++) BLOCK
  | Wait of expr
  | WhenThen of expr * stmt
  | On of expr * stmt
  | Preserve
  | Return of expr option
  | Print of expr
  | AssignVar of string * expr
  | AssignMember of string * string * expr
  | Expr of expr
and stmtordec =
  | Dec of proximity * string * expr
  | Stmt of stmt
and topdec = 
  | Fundec of proximity * string * string list * stmt
  | Vardec of proximity * string * expr
  | ExprSeq of expr list
and program =
  | Prog of topdec list