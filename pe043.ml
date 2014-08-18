open Core.Std

let rec select = function
    | [] -> []
    | hd :: tl ->
        (hd, tl) :: (select tl
                     |> List.map ~f:(fun (h, t) -> (h, hd :: t)))

let rec permute = function
    | [] -> []
    | [v] -> [[v]]
    | lst -> List.concat_map (select lst)
        ~f:(fun (hd, tl) ->
            List.map (permute tl) ~f:(fun lst -> hd :: lst))

let list_to_number lst =
    List.fold lst ~init:0 ~f:(fun acc v -> acc * 10 + v)

let is_interesting n =
    (n             % 1_000) % 17 = 0 &&
    (n /        10 % 1_000) % 13 = 0 &&
    (n /       100 % 1_000) % 11 = 0 &&
    (n /     1_000 % 1_000) %  7 = 0 &&
    (n /    10_000 % 1_000) %  5 = 0 &&
    (n /   100_000 % 1_000) %  3 = 0 &&
    (n / 1_000_000 % 1_000) %  2 = 0

let () = permute [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
         |> List.map ~f:list_to_number
         |> List.filter ~f:is_interesting
         |> List.fold ~init:0 ~f:(Int.(+))
         |> Printf.printf "%d\n"
