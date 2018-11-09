	(* キーが 'a 型、値が 'b 型の環境を表す型 *)
type ('a, 'b) t = Empty
                  | Node of {
                              key : 'a ;
                              value : 'b ;
                              next : ('a , 'b) t ;
                          }
(*(String*Value.t) list*)
let empty=Empty

let rec get kankyou hensuu = match kankyou with
  Empty ->raise Not_found
 |Node {key=namae;value=atai;next=tsugi} ->
     if namae = hensuu then
        atai
     else
        get tsugi hensuu

(*val get : ('a, 'b) t -> 'a -> 'b*)
	(* 使い方：get env var *)
	(* 環境 env の中で変数 var の値を返す *)
	(* 変数 var が見つからなかったら例外 Not_found を起こす *)

let extend kankyou hensuumei atai = Node{key=hensuumei;value=atai;next=kankyou}

(*val extend : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t*)
	(* 使い方：extend env var value *)
	(* 環境 env に変数 var の値を value に登録した新たな環境を返す *)
