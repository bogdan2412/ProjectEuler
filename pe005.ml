open Core.Std

let rec gcd a = function
    | 0 -> a
    | b -> gcd b (a % b)

let find_smallest_number_for_range left right =
    let rec iter acc cur =
        if cur > right then
            acc
        else
            let cur_gcd = gcd cur acc in
            iter (acc / cur_gcd * cur) (cur + 1)
    in
    iter 1 left

let () =
    find_smallest_number_for_range 2 20
    |> Printf.printf "%d\n"
