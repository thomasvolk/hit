let find_closest_elements rows =
  let lmax = List.fold_left max 0 in
  let lmin = List.fold_left min max_int in
  CCListLabels.cartesian_product rows
  |> List.fold_left
       (fun acc current_numbers ->
         let current_span = lmax current_numbers - lmin current_numbers in
         match acc with
         | Some (_, best_span) when current_span < best_span ->
             Some (current_numbers, current_span)
         | Some (best_numbers, best_span)
           when current_span = best_span && current_numbers < best_numbers ->
             Some (current_numbers, current_span)
         | Some _ -> acc
         | None when List.is_empty current_numbers -> None
         | None -> Some (current_numbers, current_span))
       None

  let score doc_entries =
    let cnt = List.fold_left (fun acc e -> acc + Token.DocumentEntry.count e) 0 doc_entries in
    let span = find_closest_elements (List.map Token.DocumentEntry.to_list doc_entries) in
    let df = match span with
      | Some (_, s) -> 1. /. (1. +. float_of_int s)
      | None -> 0.
    in
    (1. +. float_of_int cnt) *. (1. +. df) |> int_of_float

