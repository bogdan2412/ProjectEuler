open Core.Std

let rec iter t p h =
    let tval = t * (t + 1) / 2 in
    let pval = p * (3 * p - 1) / 2 in
    let hval = h * (2 * h - 1) in
    if tval < pval || tval < hval then
        iter (t + 1) p h
    else
        if pval < tval || pval < hval then
            iter t (p + 1) h
        else
            if hval < tval || hval < pval then
                iter t p (h + 1)
            else
                tval

let () = iter 286 166 144
         |> Printf.printf "%d\n"
