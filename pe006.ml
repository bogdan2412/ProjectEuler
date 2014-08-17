open Core.Std

let sum_of_squares n = n * (n + 1) * (2 * n + 1) / 6

let square_of_sum n =
    let sum = n * (n + 1) / 2 in
    sum * sum

let answer n = (square_of_sum n) - (sum_of_squares n)

let () = answer 100 |> Printf.printf "%d\n"
