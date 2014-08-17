open Core.Std

let big_power = Big_int.power_int_positive_int 2 1000

let sum_of_digits =
    String.fold (Big_int.string_of_big_int big_power)
    ~init:0
    ~f:(fun acc chr -> acc + (Char.to_int chr) - (Char.to_int '0'))

let () = Printf.printf "%d\n" sum_of_digits
