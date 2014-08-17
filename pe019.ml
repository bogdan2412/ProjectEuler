open Core.Std

let count_saturdays left right =
    let first_sat = Date.first_strictly_after (Date.add_days left (-1))
                        ~on:Day_of_week.sat in
    let rec iter acc cur =
        if cur > right then
            acc
        else
            iter (acc + (if Date.day cur = 1 then 1 else 0))
                 (Date.add_days cur 7)
    in
    iter 0 first_sat

let () = count_saturdays (Date.create_exn ~y:1901 ~m:Month.Jan ~d:1)
                         (Date.create_exn ~y:2000 ~m:Month.Dec ~d:31)
         |> Printf.printf "%d\n"
