open Core.Std

let get_primes_upto n =
    let is_prime = Array.create ~len:((n - 3) / 2 + 1) true in
    let get_is_prime v = Array.get is_prime ((v - 3) / 2) in
    let set_is_prime v = Array.set is_prime ((v - 3) / 2) in
    let rec mark_not_prime stride = function
        | pos when pos > n -> ()
        | pos ->
            set_is_prime pos false;
            mark_not_prime stride (pos + stride)
    in
    let rec iter = function
        | cur when cur > n -> []
        | cur ->
            if get_is_prime cur then
                begin
                    mark_not_prime (2 * cur) (cur * cur);
                    cur :: (iter (cur + 2))
                end
            else
                iter (cur + 2)
    in
    if n < 2 then
        []
    else
        2 :: (iter 3)

let () = get_primes_upto 2_000_000
         |> List.fold ~init:0 ~f:(Int.(+))
         |> Printf.printf "%d\n"
