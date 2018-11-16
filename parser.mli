type token =
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | EQUAL
  | LESS
  | MORE
  | LESSEQUAL
  | MOREEQUAL
  | NOTEQUAL
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | FUN
  | ARROW
  | REC
  | NUMBER of (int)
  | VAR of (string)
  | TRUE
  | FALSE
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
