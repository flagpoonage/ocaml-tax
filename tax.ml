let nz_tax () = 
  [
    (0, 10.5); 
    (14000, 17.5); 
    (48_000, 30.0); 
    (70_000, 33.0); 
    (180_000, 39.0)
  ]

let sg_tax () = 
  [
    (20_000, 2.0); 
    (30_000, 3.5); 
    (40_000, 7.0); 
    (80_000, 11.5); 
    (120_000, 15.0); 
    (160_000, 18.0); 
    (200_000, 19.0); 
    (240_000, 19.5); 
    (280_000, 20.0); 
    (320_000, 22.0)
  ]

let rec get_country_tax_brackets () =
  print_string "Enter Country (SG|NZ) -> ";
  let c = read_line () in
  match c with
    | "SG" -> sg_tax ()
    | "NZ" -> nz_tax ()
    | _ -> get_country_tax_brackets ()

let rec get_salary () =
  print_string "Enter Salary -> ";
  let s = int_of_string_opt (read_line ()) in
  match s with
    | Some(s) -> s
    | None -> get_salary ()

let calculate_tax_for_bracket (bracket : int * float) income =
  let (bracket_start, rate) = bracket in
  let taxable = income - bracket_start in
  let tax = match taxable > 0 with
    | true -> (float_of_int taxable) *. (rate /. 100.0)
    | false -> 0.0
  in
  print_endline (Printf.sprintf "Tax of %f, on taxable %i, at rate of %f, for remaining income %i" tax taxable rate income);
  tax

let calculate_tax (rates : (int * float) list) income =
  List.fold_right (fun bracket (i, t) ->
    let taxed = calculate_tax_for_bracket bracket i in
    let (next_bracket, _) = bracket in
    match income - next_bracket > 0 with
      | true -> (next_bracket, taxed +. t)
      | false -> (i, t)
  ) rates (income, 0.0)
  

let () =
  let rates = get_country_tax_brackets () in
  let salary = get_salary () in
  let (_, t) = calculate_tax rates salary in
  print_endline (Printf.sprintf "Total tax: %f" t);
  (*
let r = get_country_tax_rates () in
  let s = get_salary () in
   print_string (Printf.sprintf "%s for %d" c s); *)
