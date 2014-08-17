open Core.Std
open Util

let pandigital_triples = fold_range
    ~init:[]
    ~f:(fun acc a ->
        let b_range_left = a + 1 in
        let b_range_right = if a <= 9 then 9_999 else 999 in
        fold_range
            ~init:acc
            ~f:(fun acc b ->
                let prod = a * b in
                let str = (Int.to_string a) ^ (Int.to_string b) ^
                          (Int.to_string prod) in
                let uniq = List.dedup (String.to_list str) in
                if String.length str = 9 &&
                   List.length uniq = 9 &&
                   List.hd_exn uniq = '1' then
                    (a, b, prod) :: acc
                else
                    acc
            )
            b_range_left b_range_right
    )
    1 99

let products = List.map pandigital_triples ~f:(fun (_, _, p) -> p)
               |> List.dedup

let () = List.fold ~init:0 ~f:(Int.(+)) products
         |> Printf.printf "%d\n"
