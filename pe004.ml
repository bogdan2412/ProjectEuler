open Core.Std
open Util

let is_palindrome n =
    let rec reverse acc = function
        | 0 -> acc
        | n -> reverse (acc * 10 + n % 10) (n / 10)
    in
    reverse 0 n = n

let () = Option.value_exn (
            fmax_range
            ~f:(fun x ->
                fmax_range
                ~f:(fun y ->
                    let product = x * y in
                    Option.some_if (is_palindrome product) product)
                ~cmp:(Option.compare ~cmp:Int.compare)
                100 999)
            ~cmp:(Option.compare ~cmp:Int.compare)
            100 999)
         |> Printf.printf "%d\n"
