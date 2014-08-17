open Core.Std
open Util

let walk_norec walk = function
    | 1 -> 1
    | n -> if n % 2 = 0 then
               1 + walk (n / 2)
           else
               1 + walk (3 * n + 1)

let () = argmax_range ~f:(memo_rec walk_norec) ~cmp:Int.compare 1 1_000_000
         |> Printf.printf "%d\n"
