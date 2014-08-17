open Core.Std

let primes, _ = Common.prime_sieve_upto 1_000_000

let () = List.nth_exn primes 10_000
         |> Printf.printf "%d\n"
