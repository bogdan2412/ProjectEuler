open Core.Std
open Util

let get_is_prime_upto n =
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
        | cur when cur > n -> ()
        | cur ->
            if get_is_prime cur then
                mark_not_prime (2 * cur) (cur * cur);
            iter (cur + 2)
    in
    iter 3;
    function
    | arg when arg > n -> failwithf "Argument %d is too big" arg ()
    | arg when arg < 2 -> false
    | 2 -> true
    | arg when arg % 2 = 0 -> false
    | arg -> get_is_prime arg

let is_prime = get_is_prime_upto 10_000_000

let get_prime_chain a b =
    let rec iter n =
        let v = n * n + a * n + b in
        if is_prime v then
            iter (n + 1)
        else
            n
    in
    iter 0

let () = fmax_range
            ~f:(fun a ->
                let b = argmax_range
                    ~f:(fun b -> get_prime_chain a b)
                    ~cmp:Int.compare
                    (-1_000) 1_000 in
                (get_prime_chain a b, a, b))
            ~cmp:(fun (v1, _, _) (v2, _, _) -> Int.compare v1 v2)
            (-1_000) 1_000
         |> (fun (_, a, b) -> Printf.printf "%d\n" (a * b))
