open Syntax
open Value
open Env



let set_value kankyou namae atai =extend kankyou namae atai
	(* 使い方：get env var *)
let rec get_value kankyou namae= try get kankyou namae with
	Not_found->failwith("Unbound variable: "^namae)


(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.t *)
let rec f expr kankyou=
    match expr with
      Number (n) -> VNumber (n)
    | Bool (b) -> VBool (b)
    | Var(name) -> get_value kankyou name
    | Op (arg1, Plus, arg2) ->
      let v1 = f arg1 kankyou in
      let v2 = f arg2 kankyou in
      begin match (v1, v2) with
	  (VNumber (n1), VNumber (n2)) -> VNumber (n1 + n2)
	| (_, _) -> failwith ("Bad arguments to +: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Minus, arg2) ->
      let v1 = f arg1 kankyou in
      let v2 = f arg2 kankyou in
      begin match (v1, v2) with
	  (VNumber (n1), VNumber (n2)) -> VNumber (n1 - n2)
	| (_, _) -> failwith ("Bad arguments to -: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Times, arg2) ->
      let v1 = f arg1 kankyou in
      let v2 = f arg2 kankyou in
      begin match (v1, v2) with
	  (VNumber (n1), VNumber (n2)) -> VNumber (n1 * n2)
	| (_, _) -> failwith ("Bad arguments to *: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Less, arg2) ->
      let v1 = f arg1 kankyou in
      let v2 = f arg2 kankyou in
      begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) -> VBool (n1 < n2)
	| (_, _) -> failwith ("Bad arguments to <: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Equal, arg2) ->
      let v1 = f arg1 kankyou in
      let v2 = f arg2 kankyou in
      begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) -> VBool (n1 = n2)
(*  | (VBool (b1), VBool (b2)) -> VBool (b1 = b2)*)
	| (_, _) -> failwith ("Bad arguments to =: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1,Notequal, arg2) ->
      let v1 = f arg1 kankyou in
      let v2 = f arg2 kankyou in
      begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) -> VBool (n1 <> n2)
	| (_, _) -> failwith ("Bad arguments to <>: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Lessequal, arg2) ->
      let v1 = f arg1 kankyou in
      let v2 = f arg2 kankyou in
      begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) -> VBool (n1 <= n2)
    | (_, _) -> failwith ("Bad arguments to <=: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | If (arg1, arg2, arg3) ->
      let v1 = f arg1 kankyou in
      begin match v1 with
        VNumber(n1) -> failwith("Predicate part is not a boolean: "^ Value.to_string v1)
       |VBool(true) -> let v2 =f arg2 kankyou in
                  (match v2 with
                   VBool(b2)->VBool(b2)
                   |VNumber(n2)->VNumber(n2)
                 )
       |VBool(false) -> let v3=f arg3 kankyou in
                  (match v3 with
                   VBool(b3)->VBool(b3)
                   |VNumber(n3)->VNumber(n3)
                 )
    end
  | Let (arg1, arg2, arg3) ->
      let atai= f arg2 kankyou in
      begin match atai with
        VNumber(n1)-> f arg3 (set_value kankyou arg1 atai)
      end
