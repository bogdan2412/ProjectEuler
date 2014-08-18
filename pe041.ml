open Core.Std

(* Any permutation of 12345678 or 123456789 is divisible by 3 by the fact
 * that the sum of digits is divisible by 3 *)
let _, is_prime = Common.prime_sieve_upto 7_654_321

let is_pandigital_prime n = is_prime n && (
    let uniq = Int.to_string n |> String.to_list |> List.dedup in
    (List.hd_exn uniq = '1') &&
    (Char.to_int (List.last_exn uniq) - Char.to_int '0' = List.length uniq)
)

let () = Util.fold_range 3 7_654_321 ~init:2
            ~f:(fun acc v -> if is_pandigital_prime v then v else acc)
         |> Printf.printf "%d\n"
