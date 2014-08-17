open Core.Std

let factors n =
    let rec factors_iter left curr accum =
        if curr * curr > left then
            if left > 1 then
                left :: accum
            else
                accum
        else
            if left % curr = 0 then
                factors_iter (left / curr) curr (curr :: accum)
        else
            factors_iter left (curr + 1) accum
    in
    List.rev (factors_iter n 2 [])

let () = factors 600_851_475_143 |> List.last_exn |> Printf.printf "%d\n"
