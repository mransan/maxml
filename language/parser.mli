type token =
  | EOF
  | Lfloat of (float)
  | Lplus
  | Lminus
  | Ltimes
  | Ldiv
  | Lleftp
  | Lrightp
  | Lvar of (string)
  | Llet
  | Lin
  | Leq

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
