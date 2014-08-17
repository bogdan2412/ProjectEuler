open Core.Std

let count_norec count sum coins = match coins with
    | [] -> if sum = 0 then 1 else 0
    | coin::other_coins ->
        (count sum other_coins) +
        (if coin <= sum then count (sum - coin) coins else 0)

let count = Util.memo_rec_2args count_norec

let () = count 200 [ 1; 2; 5; 10; 20; 50; 100; 200 ] |> Printf.printf "%d\n"
