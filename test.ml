(*let rec times lst k = match lst with
[] -> k 1
| 0::rest -> 0
| first :: rest -> times rest (fun v -> k(first *v))
in
times [3;5;2] (fun v -> v)*)

let rec fac n k =
if n=0 then k 1
else fac (n-1) (fun v -> k (n*v))
in fac 5 (fun v-> v)
(*
times [3;5;2] (fun v1 -> v1)
= times [5;2] (funv2 ->(fun v1 ->v1) (3*v2))
= times [2] (fun v3 -> (fun v2 -> (fun v1 ->v1) (3*v2)) (5*v3))
= times [] (fun v4 -> (fun v3 -> (fun v2 -> (fun v1 ->v1) (3 *v2)) (5 *v3))(2*v4
=((fun v4 -> (fun v3 -> (fun v2 -> (fun v1 ->v1) (3 *v2)) (5 *v3))(2*v4))
=30
*)
