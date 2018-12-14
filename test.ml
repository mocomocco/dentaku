let rec add1_list lst = match lst with
  [] -> []
  | first :: rest ->
  first*3 :: add1_list rest
in add1_list [3; 2; 1; 4; 5]
