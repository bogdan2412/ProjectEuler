open Core.Std

let sum_divisors_upto n =
    let divisors =
        Bigarray.Array1.create Bigarray.int Bigarray.c_layout (n + 1) in
    let rec add_divisor stride = function
        | from when from > n -> ()
        | from ->
            let prv = Bigarray.Array1.unsafe_get divisors from in
            Bigarray.Array1.unsafe_set divisors from (prv + stride);
            add_divisor stride (from + stride)
    in
    let rec iter = function
        | cur when cur > n -> ()
        | cur ->
            add_divisor cur (cur + cur);
            iter (cur + 1)
    in
    Bigarray.Array1.fill divisors 1;
    iter 2;
    divisors

let sum_of_amicable_upto upto =
    let sum_divisors = sum_divisors_upto upto in
    let rec iter acc cur =
        if cur > upto then
            acc
        else
            let cur_sum_div = Bigarray.Array1.unsafe_get sum_divisors cur in
            if cur_sum_div <= upto then
                if cur <> cur_sum_div &&
                   Bigarray.Array1.unsafe_get
                       sum_divisors cur_sum_div = cur then
                    iter (acc + cur) (cur + 1)
                else
                    iter acc (cur + 1)
            else
                iter acc (cur + 1)
    in
    iter 0 1

let () = sum_of_amicable_upto 10_000 |> Printf.printf "%d\n"
