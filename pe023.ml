open Core.Std

let sum_divisors_upto upto =
    Common.generic_sieve upto
        ~init:(fun ~fill ~set:_ -> fill 1)
        ~update:(fun orig _ prv -> prv + orig)

let find_minimum_non_sum upto =
    let sum_divisors = sum_divisors_upto upto in
    let is_abundant n = sum_divisors n > n in
    let rec iter acc cur =
        if cur > upto then
            acc
        else
            let rec iteri i =
                if i > cur - i then
                    false
                else
                    if is_abundant i && is_abundant (cur - i) then
                        true
                    else
                        iteri (i + 1)
            in
            iter (acc + (if iteri 1 then 0 else cur)) (cur + 1)
    in
    iter 0 0

let () = find_minimum_non_sum 28_123 |> Printf.printf "%d\n"
