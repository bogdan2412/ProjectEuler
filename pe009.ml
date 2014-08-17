open Core.Std

let find_three sum =
    let rec iter_first n =
        let rec iter_second m =
            let third = sum - n - m in
            if n * n + m * m < third * third then
                iter_second (m + 1)
            else if n * n + m * m = third * third then
                Some (n, m, third)
            else
                None
        in
        match iter_second n with
        | None -> iter_first (n + 1)
        | Some result -> result
    in
    iter_first 1

let () = (find_three 1000) |> fun (a, b, c) -> a * b * c |> Printf.printf "%d\n"
