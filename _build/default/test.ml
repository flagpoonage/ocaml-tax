type numeric = 
  | Odd of int
  | Even of int

let get_variant x = 
  match x mod 2 with
    | 0 -> Even(x)
    | _ -> Odd(x)

let get_numeric_args =
  Array.to_list Sys.argv 
    |> List.tl 
    |> List.filter_map int_of_string_opt

let sprint_variant = function
  | Even(x) -> Printf.sprintf "%d is even" x
  | Odd(x) -> Printf.sprintf "%d is odd" x

let () =
  let args = get_numeric_args in
  let output = List.map get_variant args
    |> List.map sprint_variant
    |> String.concat "\n" in
  print_string output;
  
