open Core.Std

let get_divisor_count_upto upto =
    Common.generic_sieve upto
        ~init:(fun ~fill ~set -> fill 2; set 0 0; set 1 1;)
        ~update:(fun _ _ prv -> prv + 1)

let find_triangular_with_divisors at_least upto =
    let get_divisor_count = get_divisor_count_upto upto in
    let rec iter acc cur =
        if acc > upto then
            None
        else
            if get_divisor_count acc >= at_least then
                Some acc
            else
                iter (acc + cur) (cur + 1)
    in
    iter 1 2

let () = Option.value_exn (find_triangular_with_divisors 500 100_000_000)
         |> Printf.printf "%d\n"
