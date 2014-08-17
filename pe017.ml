open Core.Std

let rec write_out = function
    | 0 -> "zero"
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | 20 -> "twenty"
    | 30 -> "thirty"
    | 40 -> "forty"
    | 50 -> "fifty"
    | 60 -> "sixty"
    | 70 -> "seventy"
    | 80 -> "eighty"
    | 90 -> "ninety"
    | n ->
        if n >= 1000 then
            write_out (n / 1000) ^ "thousand" ^
                (if n % 1000 <> 0 then write_out (n % 1000) else "")
        else if n >= 100 then
            write_out (n / 100) ^ "hundred" ^
                (if n % 100 <> 0 then "and" ^ (write_out (n % 100)) else "")
        else 
            write_out (n - (n % 10)) ^ write_out (n % 10)

let get_length_sum left right =
    let rec iter acc cur = 
        if cur > right then
            acc
        else
            iter (acc + (write_out cur |> String.length)) (cur + 1)
    in
    iter 0 left

let () = get_length_sum 1 1_000 |> Printf.printf "%d\n"
