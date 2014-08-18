open Core.Std

let primes, is_prime = Common.prime_sieve_upto 1_000_000

let is_square n =
    let root = Float.to_int (sqrt (Float.of_int n)) in
    root * root = n || (root + 1) * (root + 1) = n

let rec iter n =
    if is_prime n then
        iter (n + 2)
    else
        let rec iter_primes = function
            | [] -> false
            | hd :: _ when hd >= n -> false
            | hd :: tl ->
                if is_square ((n - hd) / 2) then
                    true
                else
                    iter_primes tl
        in
        if iter_primes primes then
            iter (n + 2)
        else
            n

let () = iter 9
         |> Printf.printf "%d\n"
