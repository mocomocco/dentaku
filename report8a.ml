let rec fib1 n k=
if n<= 1 then k n
else fib1 (n-1) (fun v1 -> fib1 (n-2) (fun v2 -> k(v1 +v2)))

let rec fib2 n k=
if n<= 1 then k n
else fib2 (n-2) (fun v1 -> fib2 (n-1) (fun v2 -> k(v1 +v2)))
