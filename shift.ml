(*t=...
|shift(fun k -> t)
| reset (fun()->t)*)


(*shiftの動き
(1)直近のresetまでの継続をkに入れる
(2) 直近のresetまでの継続を捨てる
(3) shiftのbody部分を実行する*)

(*
1+ reset (fun() -> 2 * shift (fun k -> 10))
(*k= fun x -> 2*x*)
-> 1+ reset (fun ()-> 10
->1+10
->11*)

(*
1+ reset (fun() -> 2 * shift (fun k -> k 10))
(*k= fun x -> 2*x*)
-> 1+ reset (fun ()-> 20
->1+20
->21*)

(*
1+ reset (fun() -> 2 * shift (fun k ->100 +  k 10))
(*k= fun x -> 2*x*)
-> 1+ reset (fun ()-> 120
->1+120
->121*)

(*
let choose a b =
shift (fun k -> ka;kb)

reset(fun ()->
let a = choose 1 2 in
let b = a*2 in print_int b)
実行結果
2
1
*)

(*
let rec choose a b =
  if a=b then a
     else shift (fun k ->
                  ka;
                  choose (a+1) b

let a= choose 1 15 in
let b = choose a 15 in
let c = choose b 15 in
if a^2 +b^2 = c^2 then print (a,b,c)
*)

(*
let rec f lst =
 match lst with
   []-> []
  |first::rest-> first ::f rest
(*f:高等関数*)
*)

(*
let rec f lst =
 match lst with
   []-> shift(fun k -> k)
  |first::rest-> first ::f rest

let g = reset (fun () -> f [1;2;3])
(*1::f[2;3]
1::2::f[3]
1::2::3::f[]*)

g [] -> [1;2;3]
g [4;5] -> [1;2;3;4;5]

let append l1 l2 =
  let g = reset (fun () -> f l1) in
    g l2

*)
(*
let take n lst=match lst with
  []->[]
| first ::rest ->
  if n = 0  then shift ( fun k -> first ::k rest)
            else first :: take (n-1) rest
*)
