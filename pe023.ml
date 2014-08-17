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

let find_minimum_non_sum upto =
    let sum_divisors = sum_divisors_upto upto in
    let is_abundant n = Bigarray.Array1.unsafe_get sum_divisors n > n in
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
