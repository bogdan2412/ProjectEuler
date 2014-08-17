open Core.Std
open Core_extended.Std

let prime_sieve_upto upto =
    let is_prime = Bitarray.create ((upto - 3) / 2 + 1) in
    let get_is_prime v = not (Bitarray.get is_prime ((v - 3) / 2)) in
    let set_is_prime v what = Bitarray.set is_prime ((v - 3) / 2) (not what) in
    let rec mark_not_prime stride = function
        | pos when pos > upto -> ()
        | pos ->
            set_is_prime pos false;
            mark_not_prime stride (pos + stride)
    in
    let rec iter primes = function
        | cur when cur > upto -> primes
        | cur ->
            if get_is_prime cur then
            begin
                mark_not_prime (2 * cur) (cur * cur);
                iter (cur :: primes) (cur + 2)
            end
            else
                iter primes (cur + 2)
    in
    List.rev (iter [2] 3),
    function
    | arg when arg > upto -> failwithf "Argument %d is too big" arg ()
    | arg when arg < 2 -> false
    | 2 -> true
    | arg when arg % 2 = 0 -> false
    | arg -> get_is_prime arg

let generic_sieve upto ~init ~update =
    let cache =
        Bigarray.Array1.create Bigarray.int Bigarray.c_layout (upto + 1) in
    let get p = Bigarray.Array1.unsafe_get cache p in
    let set p v = Bigarray.Array1.unsafe_set cache p v in
    let rec walk orig = function
        | pos when pos > upto -> ()
        | pos ->
            let prv = get pos in
            set pos (update orig pos prv);
            walk orig (pos + orig)
    in
    let rec iter = function
        | pos when pos > upto -> ()
        | pos ->
            walk pos (pos + pos);
            iter (pos + 1)
    in
    init ~fill:(Bigarray.Array1.fill cache) ~set:set;
    iter 2;
    get

let rec pow base = function
    | 0 -> 1
    | 1 -> base
    | power when power % 2 = 0 ->
        let aux = pow base (power / 2) in
        aux * aux
    | power ->
        base * (pow base (power - 1))
