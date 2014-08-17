open Core.Std

(* Create a comparator for floats using an epsilon of 1e-10 *)
module Float_comparator_pre = struct
    type t = Float.t with sexp

    let compare a b =
        if Float.abs (a -. b) < 1e-10 then
            0
        else
            Float.compare a b
end
module Float_comparator = Comparator.Make(Float_comparator_pre)

let find_distinct alow ahigh blow bhigh =
    let rec iter acc = function
        | a when a > ahigh ->
            fun _ -> Set.length acc
        | a ->
            function
            | b when b > bhigh -> iter acc (a + 1) blow
            | b -> let v = (Float.of_int b) *. log (Float.of_int a) in
                   iter (Set.add acc v) a (b + 1)
    in
    iter (Set.empty ~comparator:Float_comparator.comparator) alow blow

let () = find_distinct 2 100 2 100 |> Printf.printf "%d\n"
