open Core.Std

let m = 10_000_000_000
let m_big_int = Big_int.big_int_of_int m

let add_mod a b = (a + b) % m

let rec pow_mod base = function
    | 0 -> 1
    | 1 -> base % m
    | power when power % 2 = 0 ->
        let aux = pow_mod base (power / 2) in
        Big_int.mod_big_int
            (Big_int.mult_int_big_int
                aux
                (Big_int.big_int_of_int aux))
            m_big_int
        |> Big_int.int_of_big_int
    | power ->
        Big_int.mod_big_int
            (Big_int.mult_int_big_int
                (pow_mod base (power - 1))
                (Big_int.big_int_of_int base))
            m_big_int
        |> Big_int.int_of_big_int

let () = Util.fold_range 1 1_000 ~init:0
            ~f:(fun acc i -> add_mod acc (pow_mod i i))
         |> Printf.printf "%d\n"
