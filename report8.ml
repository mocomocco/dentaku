(*mのn乗を求める関数powerを継続わたし形式でかけ*)
let power m n =
let rec powerrec m n k=
if n=0 then k 1
else powerrec m (n-1) (fun v -> k (m*v))
in powerrec m n (fun v ->v)


(*テスト*)
let test1 = power 1 1 =1
let test2 = power 1 0  =1
let test3 = power 5 3 =125
let test4 = power 0 1 =0
let test5 = power 0 0 =1

(*フィボナッチ数を求める関数fibを継続渡し形式でかけ*)
(*let rec fib counter k=
if counter=0 then (k 0) 0
else if counter=1 then k 1 0
else if counter=2 then k 1 0
else fib (counter-1) (fun v2 -> (fun v1 ->(k v2 v1+v2)))
*)
let rec fib counter k =
  if counter = 0 then k 0
  else if counter = 1 || counter = 2 then k 1
  else fib (counter - 1) (fun x -> fib (counter - 2) (fun y -> k (x + y)))
(*
let katei1
=fib 4 (fun v2 -> (fun v1 -> (v1 + v2)) )
=fib 3 (fun v4 -> (fun v3 -> ((fun v2 -> (fun v1 -> (v1 + v2))) v4 v3+v4)))


let katei2
=fib 3 (fun v4 -> (fun v3 -> ((fun v2 -> (fun v1 -> (v1 + v2))) v4 v3+v4)))
=fib 2 (fun v6 -> (fun v5 -> ((fun v4 -> (fun v3 -> ((fun v2 -> (fun v1 -> (v1 + v2))) v4 v3+v4))) v6 v5+v6))) 

let katei3
=fib 2 (fun v6 -> (fun v5 -> ((fun v4 -> (fun v3 -> ((fun v2 -> (fun v1 -> (v1 + v2))) v4 v3+v4))) v6 v5+v6))) 
=(fun v6 -> (fun v5 -> ((fun v4 -> (fun v3 -> ((fun v2 -> (fun v1 -> (v1 + v2))) v4 v3+v4))) v6 v5+v6))) 0 1

let katei4
(*=(fun v6 -> (fun v5 -> ((fun v4 -> (fun v3 -> ((fun v2 -> (fun v1 -> (v1 + v2))) v4 v3+v4))) v6 v5+v6))) 1 0*)
=(fun v5 -> ((fun v4 -> (fun v3 -> ((fun v2 -> (fun v1 -> (v1 + v2))) v4 v3+v4))) 0 v5)) 1

*)

let f =fun x ->x
let test1=fib 1 f
let test2=fib 2 f
let test3=fib 3 f
let test4=fib 4 f
let test5=fib 5 f
let test6=fib 6 f




(*
power 2 3 (fun v1 -> v1)
=power 2 2 (fun v2 -> (fun v1 ->v1) 2*v2)
=power 2 1 (fun v3 -> (fun  v2 -> (fun v1 ->v1) 2*v2) 2*v3)
=power 2 0 (fun v4 -> (fun v3 -> (fun  v2 -> (fun v1 ->v1) 2*v2) 2*v3)2*v4)
=(fun v4 -> (fun v3 -> (fun  v2 -> (fun v1 ->v1) 2*v2) 2*v3)2*v4) 1
= (fun v3 -> (fun  v2 -> (fun v1 ->v1) 2*v2) 2*v3) 2
=(fun  v2 -> (fun v1 ->v1) 2*v2) 4
=(fun v1 ->v1) 8
=8
*)
