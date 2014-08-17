open Core.Std

let () = (Common.prime_sieve_upto 2_000_000)
         |> (fun (primes, _) -> primes)
         |> List.fold ~init:0 ~f:(Int.(+))
         |> Printf.printf "%d\n"
