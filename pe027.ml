open Core.Std
open Util

let (_, is_prime) = Common.prime_sieve_upto 10_000_000

let get_prime_chain a b =
    let rec iter n =
        let v = n * n + a * n + b in
        if is_prime v then
            iter (n + 1)
        else
            n
    in
    iter 0

let () = fmax_range
            ~f:(fun a ->
                let b = argmax_range
                    ~f:(fun b -> get_prime_chain a b)
                    ~cmp:Int.compare
                    (-1_000) 1_000 in
                (get_prime_chain a b, a, b))
            ~cmp:(fun (v1, _, _) (v2, _, _) -> Int.compare v1 v2)
            (-1_000) 1_000
         |> (fun (_, a, b) -> Printf.printf "%d\n" (a * b))
