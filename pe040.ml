open Core.Std

let get_digit pos =
    let rec iter left digit_cnt =
        let pow = Common.pow in
        let digits_covered =
            digit_cnt * ((pow 10 digit_cnt) - (pow 10 (digit_cnt - 1))) in
        if digits_covered <= left then
            iter (left - digits_covered) (digit_cnt + 1)
        else
            let number = left / digit_cnt + (pow 10 (digit_cnt - 1)) in
            let position = left % digit_cnt in
            (number / (pow 10 (digit_cnt - 1 - position))) % 10
    in
    iter (pos - 1) 1

let () = List.map [ 1; 10; 100; 1_000; 10_000; 100_000; 1_000_000 ]
            ~f:get_digit
         |> List.fold ~init:1 ~f:(Int.( * ))
         |> Printf.printf "%d\n"
