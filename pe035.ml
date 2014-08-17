open Core.Std

let (_, is_prime) = Common.prime_sieve_upto 1_000_000

let is_circular_prime n =
    let pow10 =
        let rec iter pow =
            if pow * 10 <= n then
                iter (pow * 10)
            else
                pow
        in
        iter 1
    in
    let perm v = v / 10 + (v % 10) * pow10 in
    let rec iter cur =
        if cur = n then
            true
        else
            if is_prime cur then
                iter (perm cur)
            else
                false
    in
    is_prime n && iter (perm n)

let () = Util.fold_range 1 1_000_000 ~init:0
             ~f:(fun acc v -> acc + (if is_circular_prime v then 1 else 0))
         |> Printf.printf "%d\n"
