open Syntax
open Value
open Env



let set_value kankyou namae atai =extend kankyou namae atai
	(* 使い方：get env var *)
let rec get_value kankyou namae= try get kankyou namae with
	Not_found->failwith("Unbound variable: "^namae)


let rec eval exp env cont =
  let intop f fstring e1 e2 env cont =
    eval e1 env
      (fun v1 ->
	 eval e2 env
	   (fun v2 ->
	     begin  match (v1,v2) with
		| (VNumber(n1),VNumber(n2)) -> cont (VNumber(f n1 n2))
		| _ -> failwith ("Bad arguments to "^fstring^": " ^Value.to_string v1 ^ ", " ^Value.to_string v2)
             end
           )
       )
  in let boolop f fstring e1 e2 env cont =
	    eval e1 env
	      (fun v1 ->
		 eval e2 env
		   (fun v2 ->
		     begin  match (v1,v2) with
			| (VNumber(n1),VNumber(n2)) -> cont (VBool(f n1 n2))
			| _ -> failwith ("Bad arguments to "^fstring^": " ^Value.to_string v1 ^ ", " ^Value.to_string v2)
	             end
                   )
              )
  in match exp with
    |Number(n)-> cont (VNumber (n))
    | Bool (b) -> cont (VBool (b))
    | Nil -> cont (Tlist(Emptylist))
    | Fun (arg1, arg2) ->(*fun arg1:string -> arg2:expr  ex: fun x ->x+1*)
        cont (Vclo(arg1,arg2,env))
    | Var(name) -> cont (get_value env name)
    | Op (arg1, Plus, arg2) ->intop ( + ) "+" arg1 arg2 env cont
    | Op (arg1, Minus, arg2) -> intop ( - ) "-" arg1 arg2 env cont
    | Op (arg1, Times, arg2) -> intop ( * ) "*" arg1 arg2 env cont
    | Op (arg1, Less, arg2) -> boolop ( < ) "<" arg1 arg2 env cont
    | Op (arg1, Equal, arg2) -> boolop ( = ) "=" arg1 arg2 env cont
    | Op (arg1,Notequal, arg2) -> boolop ( <> ) "<>" arg1 arg2 env cont
    | Op (arg1, Lessequal, arg2) -> boolop ( <= ) "<=" arg1 arg2 env cont
    | Op (arg1, Divide, arg2) ->
       eval arg1 env
	      (fun v1 ->
		       eval arg2 env
		        (fun v2 ->
		           begin  match (v1,v2) with
		         	| (VNumber(n1),VNumber(n2)) -> if n2=0 then Exception(0) else cont (VNumber(n1/n2))
		        	| _ -> failwith ("Bad arguments to "^ "/" ^": " ^Value.to_string v1 ^ ", " ^Value.to_string v2)
	                   end
                        )
              )
    | If (arg1, arg2, arg3) ->(*if arg1 then arg2 else arg3*)
      eval arg1 env
        (fun v1 -> match v1 with
           VBool(true) -> eval arg2 env cont
           |VBool(false) -> eval arg3 env cont
           | _ ->failwith("Predicate part is not a boolean: "^ Value.result_to_string (eval arg1 env cont))
          )
    | Let (arg1, arg2, arg3) ->(*let arg1 = arg2 in arg3 ex: let x=3 in x+2*)
        eval arg2 env
          (fun v1 ->
             let newenv= set_value env arg1 v1
                in eval arg3 newenv cont)
    | App (arg1, arg2) ->(*arg1 arg2*)
      eval arg1 env
        ( fun kansuu ->
            eval arg2 env (fun hikisuu ->
              begin match kansuu with
                Vclo(x,t,cloenv) ->   eval t (set_value cloenv x hikisuu) cont
               |VcloR(g,x,t,cloenv) ->
                  (*tを解釈*)let genv = (set_value cloenv g kansuu)  in
                   let newenv = set_value genv x hikisuu in
                   eval t newenv cont
               |_ ->failwith(to_string kansuu ^ " is not a function")
              end))
     | Rec (arg1, arg2, arg3,arg4) ->
      eval arg4 (set_value env arg1 (VcloR(arg1,arg2,arg3,env))) cont
     |Cons(t1,t2) ->
        eval t1 env
          (fun vt1 -> eval t2 env
            (fun vt2 -> cont(Tlist(List(vt1,vt2)))))
     |Match (t1, t2 , str1 , str2 , t3) ->(*ex:  match t1 with [] -> t2 | str1 ::str2 ->t3*)
        eval t1 env
          (fun vt1 ->
             begin  match vt1 with
                Tlist(Emptylist) -> eval t2 env cont
              | Tlist(List (first, rest)) ->
                  let newenv1=set_value env str1 first in
                    let newenv2= set_value newenv1 str2 rest in
                      eval t3 newenv2 cont
              | (_) -> failwith(Value.to_string vt1 ^ " is not a list")
             end
          )
     |Raise (t1) ->
        eval t1 env
          (fun v1 -> match v1 with
            VNumber(n1) -> Exception(n1)
           |(_) -> failwith(Value.to_string v1 ^ " is not a Integer"))
     |Try (t1,t2,t3) -> (*try 3 / 0 with Error x -> x + 5*)
        let r = eval t1 env (fun v1 -> OK(v1)) in
           begin match r with
             OK(v) -> cont (v)
            |Exception(n1)-> (*cont (VNumber(n1))*)
              let newenv  = set_value env t2 (VNumber(n1))
                in eval t3 newenv cont
            end
(*    | Let (arg1, arg2, arg3) ->(*let arg1 = arg2 in arg3 ex: let x=3 in x+2*)
        eval arg2 env
          (fun v1 ->
             let newenv= set_value env arg1 v1
                in eval arg3 newenv cont)*)
(*  |Try()->
    let r= eval t1 env (fun v1 -> ok(v1)) in
    match r with
      ok(v)->cont v
     |Errorvx)->eval t2 env[v/x] cont*)
