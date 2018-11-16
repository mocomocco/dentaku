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
  | If (arg1, arg2, arg3) ->(*if arg1 then arg2 else arg3*)
      let v1 = f arg1 kankyou in
      begin match v1 with
        VBool(true) -> f arg2 kankyou
       |VBool(false) -> f arg3 kankyou
       | _ ->failwith("Predicate part is not a boolean: "^ Value.to_string v1)
    end
  | Let (arg1, arg2, arg3) ->(*let arg1 = arg2 in arg3 ex: let x=3 in x+2*)
      let atai= f arg2 kankyou in
        f arg3 (set_value kankyou arg1 atai)
  | Fun (arg1, arg2) ->(*fun arg1:string -> arg2:expr  ex: fun x ->x+1*)
      Vclo(arg1,arg2,kankyou)
  | App (arg1, arg2) ->(*arg1 arg2*)
      let kansuu=f arg1 kankyou in
      let hikisuu=f arg2 kankyou in
      begin match kansuu with
        Vclo(x,t,env) ->   f t (set_value env x hikisuu)
       |VcloR(g,x,t,env) ->
          (*tを解釈*)let genv = (set_value env g kansuu)  in
              let newenv = set_value genv x hikisuu in
                f t newenv
       |_ ->failwith(to_string kansuu ^ " is not a function")
      end
  | Rec (arg1, arg2, arg3,arg4) ->(*let rec arg1 arg2 = arg3 in arg4 ex: let x=3 in x+2*)
(*      let newenv =set_value kankyou arg1 (VcloR(arg1,arg2,arg3,kankyou)) in
      let atai= f arg3  newenv in
        f arg4 (set_value newenv arg1 atai)*)
	f arg4 (set_value kankyou arg1 (VcloR(arg1,arg2,arg3,kankyou)))
