open Core.Std
open Util

let is_curious a b =
    (a % 10 <> 0 && a % 10 = b % 10 && (a / 10) * b = (b / 10) * a) ||
    (a % 10 = b / 10 && (a / 10) * b = (b % 10) * a) ||
    (a / 10 = b % 10 && (a % 10) * b = (b / 10) * a) ||
    (a / 10 = b / 10 && (a % 10) * b = (b % 10) * a)

module Fraction : sig
    type t

    val create : int -> int -> t
    val mult : t -> t -> t
    val to_int_tuple : t -> int * int
end = struct
    type t = int * int

    let rec gcd a = function
        | 0 -> a
        | b -> gcd b (a % b)

    let create a b =
        let cur_gcd = gcd a b in
        (a / cur_gcd, b / cur_gcd)

    let mult (a, b) (c, d) =
        let gcdad = gcd a d in
        let gcdbc = gcd b c in
        ((a / gcdad) * (c / gcdbc), (b / gcdbc) * (d / gcdad))

    let to_int_tuple = Fn.id
end

let curious_fractions =
    fold_range 10 99 ~init:[]
        ~f:(fun acc a ->
            fold_range (a + 1) 99 ~init:acc
            ~f:(fun acc b ->
                if is_curious a b then
                    (Fraction.create a b) :: acc
                else
                    acc))

let product = List.fold curious_fractions
                  ~init:(Fraction.create 1 1) ~f:Fraction.mult

let () = Fraction.to_int_tuple product
         |> (fun (_, d) -> Printf.printf "%d\n" d)
