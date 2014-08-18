open Core.Std

let is_pentagonal_number n =
    let delta = 1 + 24 * n in
    let root = (1. +. sqrt (Float.of_int delta)) /. 6. in
    let root_int = Float.to_int root in
    root_int * (3 * root_int - 1) / 2 = n ||
    (root_int + 1) * (3 * root_int + 2) / 2 = n

let rec iter_diff cur =
    let diff = cur * (3 * cur - 1) / 2 in
    let rec iter pos_a pos_b =
        let a = pos_a * (3 * pos_a - 1) / 2 in
        let b = a + diff in
        if (pos_b + 1) * (3 * pos_b + 2) / 2 <= b then
            iter pos_a (pos_b + 1)
        else
            if pos_b * (3 * pos_b - 1) / 2 = b &&
               is_pentagonal_number (a + b) then
                Some b
            else
                if pos_a = pos_b then
                    None
                else
                    iter (pos_a + 1) pos_b
    in
    match iter 1 cur with
    | None -> iter_diff (cur + 1)
    | Some _ -> diff

let () = iter_diff 1
         |> Printf.printf "%d\n"
