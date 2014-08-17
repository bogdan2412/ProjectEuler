open Core.Std

let get_value_at n (x, y) =
    assert (n % 2 = 1);
    assert (x >= 0 && x < n && y >= 0 && y < n);
    let layer_x = if x <= (n / 2) then x else (n - x - 1) in
    let layer_y = if y <= (n / 2) then y else (n - y - 1) in
    let layer = min layer_x layer_y in
    let inner_layers = n / 2 - layer in
    if inner_layers = 0 then
        1
    else
        let start = 1 + 4 * (inner_layers) * (inner_layers - 1) in
        let pos =
            if y = (n / 2) + inner_layers then
                if x = layer then
                    8 * inner_layers - 1
                else
                    x - layer - 1
            else if x = (n / 2) + inner_layers then
                2 * inner_layers + (n / 2) + inner_layers - y - 1
            else if y = layer then
                4 * inner_layers + (n / 2) + inner_layers - x - 1
            else
                6 * inner_layers + y - layer - 1
        in
            start + pos + 1

let sum_diag n =
    let rec iter acc = function
        | cur when cur >= n -> acc
        | cur -> iter (acc + (get_value_at n (cur, cur))
                           + (get_value_at n (cur, n - cur - 1)))
                      (cur + 1)
    in
    (iter 0 0) - 1

let () = sum_diag 1001 |> Printf.printf "%d\n"
