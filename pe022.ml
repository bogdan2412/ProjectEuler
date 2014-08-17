open Core.Std

let name_list =
    let ic = open_in "pe022_names.txt" in
    let line = input_line ic in
    List.map (String.split line ~on:',')
        ~f:(fun str -> String.slice str 1 (-1))
    |> List.sort ~cmp:String.compare

let word_weight word =
    String.fold word
        ~init:0
        ~f:(fun acc chr ->
            acc + (Char.to_int chr) - (Char.to_int 'A') + 1)

let () = List.foldi name_list
            ~init:0
            ~f:(fun index acc word -> acc + (index + 1) * (word_weight word))
         |> Printf.printf "%d\n"
