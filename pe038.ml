open Core.Std
open Core_extended.Std
open Util

let pandigital_products = fold_range
    ~init:[]
    ~f:(fun acc n ->
        let b_range_left = 1 in
        let b_range_right = (Common.pow 10 (9 / n)) - 1 in
        fold_range
            ~init:acc
            ~f:(fun acc p ->
                let rec iter acc = function
                    | cur when cur > n -> acc
                    | cur -> iter (acc ^ (Int.to_string (p * cur))) (cur + 1)
                in
                let str = iter "" 1 in
                let uniq = List.dedup (String.to_list str) in
                if String.length str = 9 &&
                   List.length uniq = 9 &&
                   List.hd_exn uniq = '1' then
                    (Int.of_string str, p, n) :: acc
                else
                    acc
            )
            b_range_left b_range_right
    )
    2 9

let () = List.map pandigital_products ~f:(fun (prod, _, _) -> prod)
         |> List.max_exn ~cmp:Int.compare
         |> Printf.printf "%d\n"
