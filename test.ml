(let f = fun x -> fun y -> y+1 in     
let x=3 in
if x>0 then f (-x) else f 1) 3 

let f = fun x -> (if x=3 then 1 else 3) in
f 3
