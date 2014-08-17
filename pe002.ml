open Core.Std

let compute_sum limit =
    let rec compute last current sum =
        if current > limit then
            sum
        else
            let new_sum = if current % 2 = 0 then sum + current else sum in
            compute current (last + current) new_sum
    in
    compute 1 1 0

let () = Printf.printf "%d\n" (compute_sum 4_000_000)
