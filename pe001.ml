open Core.Std

let compute_sum left right =
    let rec compute current sum =
        if current = right then
            sum
        else
            if current % 3 = 0 || current % 5 = 0 then
                compute (current + 1) (sum + current)
            else
                compute (current + 1) sum
    in
    compute left 0

let () = Printf.printf "%d\n" (compute_sum 1 1000)
