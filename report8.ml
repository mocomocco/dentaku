(*mのn乗を求める関数powerを継続わたし形式でかけ*)
let power m n =
let rec powerrec m n k=
if n=0 then k 1
else powerrec m (n-1) (fun v -> k (m*v))
in powerrec m n (fun v ->v)

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

(*テスト*)
let test1 = power 1 1 =1
let test2 = power 1 0  =1
let test3 = power 5 3 =125
let test4 = power 0 1 =0
let test5 = power 0 0 =1

(*フィボナッチ数を求める関数fibを継続渡し形式でかけ*)
let fib counter=
let rec recfib counter k =
  if counter = 0 then k 0
  else if counter = 1 || counter = 2 then k 1
  else recfib (counter - 1) (fun x -> recfib (counter - 2) (fun y -> k (x + y)))
in recfib counter (fun x->x)

let test1=fib 0 =0
let test2=fib 1 =1
let test3=fib 2 =1
let test4=fib 3=2
let test5=fib 4 =3
let test6=fib 5 =5





