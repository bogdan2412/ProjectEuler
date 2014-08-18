open Core.Std

let word_list =
    let ic = open_in "pe042_words.txt" in
    let line = input_line ic in
    List.map (String.split line ~on:',')
        ~f:(fun str -> String.slice str 1 (-1))
    |> List.sort ~cmp:String.compare

let word_weight word =
    String.fold word
        ~init:0
        ~f:(fun acc chr ->
            acc + (Char.to_int chr) - (Char.to_int 'A') + 1)

let is_triangular_number n =
    let root = Common.binary_search ~from:0
        ~f:(fun p -> p * (p + 1) / 2 <= n)
    in
    root * (root + 1) / 2 = n

let () = List.fold word_list ~init:0
            ~f:(fun acc word ->
                if is_triangular_number (word_weight word) then
                    acc + 1
                else
                    acc)
         |> Printf.printf "%d\n"
