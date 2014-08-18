open Core.Std

let sum_divisors_upto upto =
    Common.generic_sieve upto
        ~init:(fun ~fill ~set:_ -> fill 1)
        ~update:(fun orig _ _ sum -> sum + orig)

let sum_of_amicable_upto upto =
    let sum_divisors = sum_divisors_upto upto in
    let rec iter acc cur =
        if cur > upto then
            acc
        else
            let cur_sum_div = sum_divisors cur in
            if cur_sum_div <= upto then
                if cur <> cur_sum_div &&
                   sum_divisors cur_sum_div = cur then
                    iter (acc + cur) (cur + 1)
                else
                    iter acc (cur + 1)
            else
                iter acc (cur + 1)
    in
    iter 0 1

let () = sum_of_amicable_upto 10_000 |> Printf.printf "%d\n"
