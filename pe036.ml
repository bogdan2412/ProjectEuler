open Core.Std

let is_palindrome base n =
    let rec iter acc = function
        | 0 -> acc = n
        | cur -> iter (acc * base + (cur % base)) (cur / base)
    in
    iter 0 n

let () = Util.fold_range 1 1_000_000 ~init:0
            ~f:(fun acc v -> acc + (if is_palindrome 10 v &&
                                       is_palindrome 2 v then
                                        v
                                    else
                                        0))
         |> Printf.printf "%d\n"
