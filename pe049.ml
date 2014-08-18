open Core.Std

let _, is_prime = Common.prime_sieve_upto 9_999

let is_good_triplet base diff =
    diff > 0 && 1_000 <= base && base + 2 * diff <= 9_999 && (
    let a, b, c = base, base + diff, base + 2 * diff in
    is_prime a && is_prime b && is_prime c && (
    let rec digits acc = function
        | 0 -> acc
        | n -> digits ((n % 10) :: acc) (n / 10)
    in
    let a_lst = List.sort ~cmp:Int.compare (digits [] a) in
    let b_lst = List.sort ~cmp:Int.compare (digits [] b) in
    let c_lst = List.sort ~cmp:Int.compare (digits [] c) in
    a_lst = b_lst && b_lst = c_lst))
    
let () = Util.fold_range 1_488 9_999 ~init:148748178147
            ~f:(fun acc base ->
                Util.fold_range 1 ((9_999 - base) / 2) ~init:acc
                    ~f:(fun acc diff ->
                        if is_good_triplet base diff then
                            base * 100_000_000 +
                            (base + diff) * 10_000 +
                            (base + 2 * diff)
                        else
                            acc))
        |> Printf.printf "%d\n"
