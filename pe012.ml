open Core.Std

let count_divisors_upto n =
    let divisors =
        Bigarray.Array1.create Bigarray.int Bigarray.c_layout (n + 1) in
    let rec add_divisor stride = function
        | from when from > n -> ()
        | from ->
            let prv = Bigarray.Array1.unsafe_get divisors from in
            Bigarray.Array1.unsafe_set divisors from (prv + 1);
            add_divisor stride (from + stride)
    in
    let rec iter = function
        | cur when cur > n -> ()
        | cur ->
            add_divisor cur (cur + cur);
            iter (cur + 1)
    in
    Bigarray.Array1.fill divisors 2;
    Bigarray.Array1.unsafe_set divisors 0 0;
    Bigarray.Array1.unsafe_set divisors 1 1;
    iter 2;
    divisors

let find_triangular_with_divisors at_least upto =
    let divisors = count_divisors_upto upto in
    let rec iter acc cur =
        if acc > upto then
            None
        else
            if Bigarray.Array1.get divisors acc >= at_least then
                Some acc
            else
                iter (acc + cur) (cur + 1)
    in
    iter 1 2

let () = Option.value_exn (find_triangular_with_divisors 500 100_000_000)
         |> Printf.printf "%d\n"
