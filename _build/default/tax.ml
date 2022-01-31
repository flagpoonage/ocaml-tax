
let rec get_country () =
  print_string "Enter Country (SG|NZ) -> ";
  let c = read_line () in
  match c with
    | ("SG" | "NZ" as cv) -> cv
    | _ -> get_country ()

let rec get_salary () =
  print_string "Enter Salary -> ";
  let s = int_of_string_opt (read_line ()) in
  match s with
    | Some(s) -> s
    | None -> get_salary ()

let () =
  let c = get_country () in
  let s = get_salary () in
  print_string (Printf.sprintf "%s for %d" c s);
