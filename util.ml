open Core.Std

(* Takes in a function as argument and returns a memoized version of
 * the function. *)
let memoize f =
    let cache = Hashtbl.Poly.create () in
    (fun arg ->
        match Hashtbl.find cache arg with
        | Some result -> result
        | None ->
            let result = f arg in
            Hashtbl.add_exn cache ~key:arg ~data:result;
            result)

(* Takes in a recursive function as argument and returns a memoized version
 * of the function. The recursive function should not be specifically made
 * recursive, but should instead take itself as the first argument. 
 *
 * Example:
 * let fib_norec fib = function
 *   | 0 | 1 -> 1
 *   | n -> fib (n - 2) + fib (n - 1)
 *
 * let fact_norec fact =
 *     if n <= 1 then
 *         0
 *     else
 *         n * (fact (n - 1))
 * *)
let memo_rec f_norec =
    let fref = ref (fun _ -> assert false) in
    let f = memoize (fun arg -> f_norec !fref arg) in
    fref := f;
    fun arg -> f arg

(* Returns the argument for which ~f has the highest value within the range
 * [left, right] according to the comparator function ~cmp *)
let argmax_range ~f ~cmp left right =
    let rec iter acc_val acc_pos pos =
        if pos > right then
            acc_pos
        else
            let val_at_pos = Some (f pos) in
            let new_acc_val, new_acc_pos =
                if Option.compare ~cmp val_at_pos acc_val > 0 then
                    val_at_pos, Some pos
                else
                    acc_val, acc_pos
            in
            iter new_acc_val new_acc_pos (pos + 1)
    in
    Option.value_exn (iter None None left)

let argmin_range ~f ~cmp left right =
    argmax_range ~f ~cmp:(fun a b -> -(cmp a b)) left right

(* Returns the highest value of ~f for an argument within the range [left, right]
 * according to the comparator function ~cmp *)
let fmax_range ~f ~cmp left right =
    let rec iter acc pos =
        if pos > right then
            acc
        else
            let val_at_pos = Some (f pos) in
            let new_acc = 
                if Option.compare ~cmp val_at_pos acc > 0 then
                    val_at_pos
                else
                    acc
            in
            iter new_acc (pos + 1)
    in
    Option.value_exn (iter None left)

let fmin_range ~f ~cmp left right =
    fmax_range ~f ~cmp:(fun a b -> -(cmp a b)) left right

(* Perform a fold on a range of integers *)
let fold_range ~init ~f left right =
    let rec iter acc = function
        | pos when pos > right -> acc
        | pos -> iter (f acc pos) (pos + 1)
    in 
    iter init left
