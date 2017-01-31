// Signature file for parser generated by fsyacc
module TransPar
type token = 
  | LOCAL
  | GLOBAL
  | EOF
  | DEL
  | LPAR
  | RPAR
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | SEMI
  | COMMA
  | ASSIGN
  | AMP
  | DOT
  | NOT
  | SEQOR
  | SEQAND
  | EQ
  | NE
  | GT
  | LT
  | GE
  | LE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | INC
  | DEC
  | ON
  | PRESERVE
  | STRUCT
  | RETURN
  | PRINT
  | CHAR
  | IF
  | ELSE
  | BREAK
  | DO
  | WHILE
  | LOCK
  | UNLOCK
  | FOR
  | IN
  | WAIT
  | WHEN
  | THEN
  | VAR
  | FUN
  | CSTSTRING of (string)
  | NAME of (string)
  | CSTBOOL of (bool)
  | CSTFLOAT of (float)
  | CSTINT of (int)
type tokenId = 
    | TOKEN_LOCAL
    | TOKEN_GLOBAL
    | TOKEN_EOF
    | TOKEN_DEL
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_LBRACK
    | TOKEN_RBRACK
    | TOKEN_SEMI
    | TOKEN_COMMA
    | TOKEN_ASSIGN
    | TOKEN_AMP
    | TOKEN_DOT
    | TOKEN_NOT
    | TOKEN_SEQOR
    | TOKEN_SEQAND
    | TOKEN_EQ
    | TOKEN_NE
    | TOKEN_GT
    | TOKEN_LT
    | TOKEN_GE
    | TOKEN_LE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_INC
    | TOKEN_DEC
    | TOKEN_ON
    | TOKEN_PRESERVE
    | TOKEN_STRUCT
    | TOKEN_RETURN
    | TOKEN_PRINT
    | TOKEN_CHAR
    | TOKEN_IF
    | TOKEN_ELSE
    | TOKEN_BREAK
    | TOKEN_DO
    | TOKEN_WHILE
    | TOKEN_LOCK
    | TOKEN_UNLOCK
    | TOKEN_FOR
    | TOKEN_IN
    | TOKEN_WAIT
    | TOKEN_WHEN
    | TOKEN_THEN
    | TOKEN_VAR
    | TOKEN_FUN
    | TOKEN_CSTSTRING
    | TOKEN_NAME
    | TOKEN_CSTBOOL
    | TOKEN_CSTFLOAT
    | TOKEN_CSTINT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Topdecs
    | NONTERM_Proximity
    | NONTERM_Vardec
    | NONTERM_Vardesc
    | NONTERM_Topdec
    | NONTERM_Fundec
    | NONTERM_Params
    | NONTERM_Params1
    | NONTERM_Block
    | NONTERM_Dec
    | NONTERM_StmtSeq
    | NONTERM_Stmt
    | NONTERM_StmtM
    | NONTERM_VarAssign
    | NONTERM_StructAssign
    | NONTERM_StmtU
    | NONTERM_ExprSeq
    | NONTERM_Expr
    | NONTERM_Access
    | NONTERM_ExprNotAccess
    | NONTERM_AtExprNotAccess
    | NONTERM_Exprs
    | NONTERM_Exprs1
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Main : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Absyn.program) 
