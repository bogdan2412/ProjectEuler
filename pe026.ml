open Core.Std
open Util

let get_cycle_length n =
    let rec iter acc = function
        | 0 -> None
        | cur ->
            match Int.Map.find acc cur with
            | None ->
                iter (Int.Map.add acc ~key:cur ~data:(Int.Map.length acc))
                     (cur * 10 % n)
            | Some len ->
                Some (Int.Map.length acc - len)
    in
    iter Int.Map.empty 1

let () = argmax_range
             ~f:get_cycle_length
             ~cmp:(Option.compare ~cmp:Int.compare)
             1 999
         |> Printf.printf "%d\n"
