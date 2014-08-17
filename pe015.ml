open Core.Std
open Util

let walk_norec walk (x, y) =
    if x < 0 || y < 0 then
        0
    else
        if x = 0 && y = 0 then
            1
        else
            walk (x - 1, y) + walk (x, y - 1)

let () = (memo_rec walk_norec) (20, 20) |> Printf.printf "%d\n"
