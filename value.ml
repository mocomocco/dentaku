(* Value.t : プログラムの実行結果を表す型 *)

type 'a lst=Emptylist
           | List of 'a * 'a 

type t = VNumber of int
       | VBool of bool
       | Vclo of string * Syntax.t * ((string , t) Env.t)
       | VcloR of string * string * Syntax.t * ((string , t) Env.t)
       | Tlist of t lst


(*type l= VNumber of int*)

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | Vclo(x,t,e) -> "<fun>"
  | VcloR(g,x,t,e) -> "<fun>"
  | Tlist(Emptylist) -> "[]"
  | Tlist(List (t1,t2)) -> to_string t1 ^ " :: " ^ to_string t2

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
