open Core.Std

let find_sum_of_longest_streak upto =
    let primes, is_prime = Common.prime_sieve_upto upto in
    let rec iter_left bst_len bst_val = function
        | [] -> bst_val
        | prime::primes ->
            let rec iter_right bst_len bst_val cnt sum =
                let new_bst_len, new_bst_val =
                    if cnt > bst_len && sum <= upto && is_prime sum then
                        (cnt, sum)
                    else
                        (bst_len, bst_val)
                in function
                | [] -> (new_bst_len, new_bst_val)
                | prime::primes ->
                    if sum > upto then
                        (new_bst_len, new_bst_val)
                    else
                        iter_right new_bst_len new_bst_val
                             (cnt + 1) (sum + prime) primes
            in
            let new_bst_len, new_bst_val =
                iter_right bst_len bst_val 1 prime primes
            in
            iter_left new_bst_len new_bst_val primes
    in
    iter_left 0 0 primes

let () = find_sum_of_longest_streak 1_000_000
         |> Printf.printf "%d\n"
