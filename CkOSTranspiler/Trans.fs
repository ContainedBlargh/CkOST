module Trans

open Absyn
open System

let format = String.Format

type 'v env = (string * 'v) list



let tranProximity = function
  | Local -> "LOCAL"
  | Global -> "GLOBAL"

let rec negateBoolExpr check = //De Morgan's laws:
    match check with
    | Prim2(ope, lh, rh) -> match ope with
                            | "&&" -> Prim2("||", (negateBoolExpr lh), (negateBoolExpr rh)) //!(P&&Q) -> (!P) || (!Q)
                            | "||" -> Prim2("&&", (negateBoolExpr lh), (negateBoolExpr rh)) //!(P||Q) -> (!P) && (!Q)
                            | ">" -> Prim2("<=", lh, rh)
                            | ">=" -> Prim2("<", lh, rh)
                            | "<" -> Prim2(">=", lh, rh)
                            | "<=" -> Prim2(">", lh, rh)
                            | "==" -> Prim2("!=", lh, rh)
                            | "!=" -> Prim2("==", lh, rh)
                            | _ -> failwith "Non-boolean expression in For-loop check not allowed."
    | _ -> failwith "Non-boolean expression in For-loop check not allowed."

let rec tranParameters pars acc = 
  match pars with
  | [] -> acc
  | a -> "PARAMETER a. "+acc

let rec tranExpr expr = 
  match expr with
  | Access(access) -> tranAccess access
  | CstN(n) -> (string n)
  | CstS(s) -> "\""+s+"\""
  | CstB(b) -> (String.map (fun x -> Char.ToLower(x)) (b.ToString()))
  | Call(name, parameters) -> 
      let rec listParams p acc =
         match p with
         | [] -> acc
         | e::[] -> acc+(tranExpr e)
         | e::p -> listParams p (acc+","+(tranExpr e))
      if name = "print" then
        "PRINT " + (tranExpr (List.head parameters))
      else
        name + "(" + (listParams parameters String.Empty) + ")"
  | PreInc(access) -> "++"+(tranAccess access)
  | PreDec(access) -> "--"+(tranAccess access)
  | PostInc(access) -> (tranAccess access)+"++"
  | PostDec(access) -> (tranAccess access)+"--"
  | Prim1(ope, expr) -> ope + " " + tranExpr expr
  | Prim2(ope, l, r) -> match ope with
                        | "!=" -> (tranExpr l) + " <> " + (tranExpr r)
                        | _ -> (tranExpr l) + " " + ope + " " + (tranExpr r)
  | Andalso(l, r) -> (tranExpr l) + " and " + (tranExpr r)
  | Orelse(l,r) -> (tranExpr l) + " or " + (tranExpr r)
and tranAccess = function
  | AccVar(s) -> s
  | AccMem(s, m) -> String.Format("{0}.{1}", [|s;m|])
  | AccFun(s) -> (s+"@")
  | AccIndex(a, x) -> (tranAccess a)+"["+(tranExpr x)+"]"

let rec tranStmt stmt =
  match stmt with
  | Expr(expr) -> ((tranExpr expr)+".")
  | If(check, t, f) -> if (tranStmt f) = String.Empty then
                         "IF "+(tranExpr check) + (tranStmt t)
                       else
                         "IF "+(tranExpr check) + (tranStmt t) + "ELSE" + (tranStmt f)
  | Break -> "BREAK."
  | Block(stmts) -> "{ "+(tranBlock stmts String.Empty)+" }"
  | DoWhile(stmt, expr) -> (tranStmt stmt)+"."+(tranStmt (While(expr, stmt))) 
  | While(expr, stmt) -> "UNTIL ("+(tranExpr(negateBoolExpr expr))+")"+(tranStmt stmt)
  | Lock(name, expr) -> String.Format("LOCK {0} TO {1}.", [|name;(tranExpr expr)|])
  | Unlock(name) -> "UNLOCK " + name + "."
  | ForIn(var, collection, stmt) -> "FOR " + var + " IN " + collection + " " + (tranStmt stmt)
  | For(init, check, step, block) -> tranFor init check step block
  | Wait(expr) -> "WAIT " + (tranExpr expr) + "."
  | WhenThen(expr, stmt) -> "WHEN " + (tranExpr expr) + " THEN " + (tranStmt stmt)
  | On(expr, stmt) -> "ON " + (tranExpr expr) + " " + (tranStmt stmt)
  | Preserve -> "PRESERVE."
  | Return(exprOpt) -> match exprOpt with
                       | Some expr -> "RETURN " + (tranExpr expr) + "."
                       | None -> "RETURN."
  | Print(expr) -> "PRINT " + (tranExpr expr) + "."
  | AssignVar(name, rhs) -> "SET " + name + " TO " + (tranExpr rhs) + "."
  | AssignMember(str, mem, expr) -> "SET " + str + ":" + mem + " TO " + (tranExpr expr) + "."
and tranBlock stmtsordecs acc =
  match stmtsordecs with
  | [] -> acc
  | Stmt(stmt)::xr -> tranBlock xr acc+(tranStmt stmt)+"."
  | Dec(prox, name, expr)::xr -> tranBlock xr acc+(tranVardec prox name expr)
and forCheckStep check step : string * string =
  let check = (tranExpr (negateBoolExpr check))
  let step = (tranStmt step)
  check,step
and tranFor init check step block =
  let iterator = match init with
                 | Dec(prox, name, expr) -> tranVardec prox name expr
                 | _ -> failwith "For initializer must be a declaration."
  let (check, step) = forCheckStep check step
  let block = tranStmt block
  "FROM { " + iterator + " } UNTIL " + check + " STEP { " + step + " } DO " + block
and tranFunBody body =
  String.map (fun x -> if x = '{' || x = '}' then ' ' else x) (tranStmt body)
and tranFundec proximity name parameters body =
  "DECLARE " + (tranProximity proximity) + " FUNCTION " + name + " { " + tranParameters parameters String.Empty + tranFunBody body + " }"
and tranVardec proximity name expr =
  "DECLARE " + (tranProximity proximity) + " " + name + " TO " + (tranExpr expr) + "."
and tranExprSeq exprs acc =
  match exprs with
  | [] -> acc
  | expr::xs -> tranExprSeq xs (acc+(tranExpr expr))+". "

let rec tranTopdec (decs : topdec list) pile = 
  match decs with
  | [] -> pile
  | Vardec(prox, name, expr)::xs -> tranTopdec xs (pile + tranVardec prox name expr)
  | Fundec(proximity, name, parameters, body)::xs -> tranTopdec xs (pile + (tranFundec proximity name parameters body))
  | ExprSeq(exprs)::xs -> tranTopdec xs (pile + (tranExprSeq exprs String.Empty))

let transpile (prog : program) =   
  match prog with
  | Prog(xr) -> tranTopdec xr String.Empty
