open Core.Std
open Util

let number_of_triangles perimeter =
    fold_range 1 (perimeter / 3) ~init:0
        ~f:(fun acc a ->
            fold_range a ((perimeter - a) / 2) ~init:acc
                ~f:(fun acc b ->
                    let c = perimeter - a - b in
                    if a * a + b * b = c * c then
                        acc + 1
                    else
                        acc))

let () = argmax_range 1 1_000 ~f:number_of_triangles ~cmp:Int.compare
         |> Printf.printf "%d\n"
