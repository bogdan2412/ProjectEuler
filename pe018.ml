open Core.Std
open Util

let table = [|
    [| 75; |];
    [| 95; 64; |];
    [| 17; 47; 82; |];
    [| 18; 35; 87; 10; |];
    [| 20; 04; 82; 47; 65; |];
    [| 19; 01; 23; 75; 03; 34; |];
    [| 88; 02; 77; 73; 07; 63; 67; |];
    [| 99; 65; 04; 28; 06; 16; 70; 92; |];
    [| 41; 41; 26; 56; 83; 40; 80; 70; 33; |];
    [| 41; 48; 72; 33; 47; 32; 37; 16; 94; 29; |];
    [| 53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14; |];
    [| 70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57; |];
    [| 91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48; |];
    [| 63; 66; 04; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31; |];
    [| 04; 62; 98; 27; 23; 09; 70; 98; 73; 93; 38; 53; 60; 04; 23; |]
|]

let walk_norec walk (x, y) =
    if x < 0 || y < 0 || x < y then
        0
    else
        let value = Array.get (Array.get table x) y in
        value + (max (walk (x - 1, y - 1)) (walk (x - 1, y)))

let walk = memo_rec walk_norec

let get_max row colleft colright =
    let rec iter acc cur =
        if cur > colright then
            acc
        else
            let cur_val = walk (row, cur) in
            iter (max acc cur_val) (cur + 1)
    in
    iter 0 colleft

let () = get_max 14 0 14 |> Printf.printf "%d\n"
