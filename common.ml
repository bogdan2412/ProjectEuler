open Core.Std
open Core_extended.Std

(* Returns the list of primes upto a particular value and a function that
 * returns whether or not a given input upto the value is prime. *)
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

(* More generic implementation of Eratosthenes' sieve. *)
let generic_sieve upto ~init ~update =
    let cache =
        Bigarray.Array1.create Bigarray.int Bigarray.c_layout (upto + 1) in
    let get p = Bigarray.Array1.unsafe_get cache p in
    let set p v = Bigarray.Array1.unsafe_set cache p v in
    let rec walk orig =
        let orig_value = get orig in
        function
        | pos when pos > upto -> ()
        | pos ->
            let pos_value = get pos in
            set pos (update orig orig_value pos pos_value);
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

(* pow a b computes a raised to the power b *)
let rec pow base = function
    | 0 -> 1
    | 1 -> base
    | power when power % 2 = 0 ->
        let aux = pow base (power / 2) in
        aux * aux
    | power ->
        base * (pow base (power - 1))

(* binary searches for the last value greater than or equal to ~from for
 * which ~f is true. Assumes that ~f is such that it is true for all values
 * less than or equal to that and false for all values greater than that. *)
let binary_search ~from ~f =
    let max_step =
        let rec iter pow =
            if f (from + pow * 2) then
                iter (pow * 2)
            else
                pow
        in
        iter 1
    in
    let rec iter pos = function
        | 0 -> pos
        | step ->
            if f (pos + step) then
                iter (pos + step) (step / 2)
            else
                iter pos (step / 2)
    in
    iter from max_step
