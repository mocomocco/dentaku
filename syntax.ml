(* op_t : ２項演算子の型 *)
type op_t = Plus | Minus | Times | Equal | Less | Lessequal | Notequal | Divide

(* ２項演算子を文字列にする関数 *)
(* op_to_string : op_t -> string *)
let op_to_string op = match op with
    Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Equal -> " = "
  | Less -> " < "
  | Lessequal -> " <= "
  | Notequal -> " <> "
  | Divide -> " / "

(* Syntax.t : プログラムを表す型 *)
type t = Number of int
       | Bool of bool
       | Var of string
       | Op of t * op_t * t
       | If of t * t * t
       | Let of string * t * t
       | Fun of string * t
       | App of t * t
       | Rec of string * string * t * t
       | Nil
       | Cons of t * t
       | Match of t * t * string * string * t
       | Raise of t
       | Try of t * string * t

(* プログラムを文字列にする関数 *)
(* Syntax.to_string : Syntax.t -> string *)
let rec to_string exp = match exp with
    Number (n) -> string_of_int n
  | Var (v) -> v
  | Nil -> "[]"
  | Bool (b) -> if b then "true" else "false"
  | Op (arg1, op, arg2) ->
	"(" ^ to_string arg1
	    ^ op_to_string op
	    ^ to_string arg2 ^ ")"
  | If (arg1, arg2, arg3) -> " if " ^ to_string arg1 ^ " then " ^ to_string arg2 ^ " else " ^to_string arg3
  | Let (arg1, arg2, arg3) -> "let "^ arg1 ^ " = " ^to_string arg2 ^ " in " ^to_string arg3
  | Fun (arg1,arg2) -> " fun " ^ arg1 ^ " -> " ^ to_string arg2
  | App (arg1 , arg2) -> to_string arg1 ^" " ^ to_string arg2
  | Rec (arg1 , arg2 , arg3 , arg4) -> "let rec "^arg1 ^" "^arg2 ^" = " ^ to_string arg3 ^ " in "^to_string arg4
  | Cons(arg1,arg2) ->to_string arg1 ^ " :: " ^ to_string arg2
  | Match(arg1,arg2,arg3,arg4,arg5) -> "match "^to_string arg1 ^ " with [] -> " ^  to_string arg2 ^ " | " ^ arg3 ^" :: "^arg4^" -> "^to_string arg5



(* プログラムをプリントする関数 *)
(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
