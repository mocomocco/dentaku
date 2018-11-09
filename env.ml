type ('a, 'b) t = ('a * 'b) list

let empty=[]

let get kankyou hensuu = List.assoc hensuu kankyou

(*val get : ('a, 'b) t -> 'a -> 'b*)
	(* 使い方：get env var *)
	(* 環境 env の中で変数 var の値を返す *)
	(* 変数 var が見つからなかったら例外 Not_found を起こす *)

let extend kankyou hensuumei atai = (hensuumei,atai)::kankyou

(*val extend : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t*)
	(* 使い方：extend env var value *)
	(* 環境 env に変数 var の値を value に登録した新たな環境を返す *)
