open Core.Std

let (_, is_prime) = Common.prime_sieve_upto 10_000_000

let is_truncatable_prime n =
    let rec iter = function
        | 0 -> true
        | cur -> if is_prime cur then iter (cur / 10) else false
    in
    if iter n then
        let pow10 =
            let rec iter pow =
                if pow * 10 <= n then
                    iter (pow * 10)
                else pow
            in
            iter 1
        in
        let rec iter = function
            | 0, 0 -> true
            | pow, cur ->
                if is_prime cur then
                    iter (pow / 10, cur % pow)
                else
                    false
        in
        iter (pow10, n)
    else
        false

let truncatable_primes =
    Util.fold_range 10 10_000_000 ~init:[]
        ~f:(fun acc n -> if is_truncatable_prime n then n :: acc else acc)

assert (List.length truncatable_primes = 11)

let () = List.fold truncatable_primes ~init:0 ~f:(Int.(+))
         |> Printf.printf "%d\n"
