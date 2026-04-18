let find_closest_elements (rows : 'a list list) : 'a list =
  let lmax = List.fold_left max 0 in
  let lmin = List.fold_left min max_int in
  let closest =
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
           | None -> Some (current_numbers, current_span))
         None
  in
  match closest with
  | Some (numbers, _) -> numbers |> List.sort compare
  | None -> []
