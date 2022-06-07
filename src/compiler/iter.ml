type 'a t = ('a -> unit) -> unit

let map : ('a -> 'b) -> 'a t -> 'b t = fun f x k -> x (fun x -> k (f x))
let flat_map : ('a -> 'b t) -> 'a t -> 'b t = fun f x k -> x (fun x -> f x k)
let flatten i = flat_map (fun x -> x) i
let of_list l : _ t = fun k -> List.iter k l
let of_iter i k = i k
let filter f (i : _ t) : _ t = fun k -> i (fun x -> if f x then k x)
let empty : _ t = fun _k -> ()

let of_option (o : _ option) : _ t =
 fun k ->
  match o with
  | Some x -> k x
  | None -> ()

let filter_map f (i : _ t) : _ t =
 fun k ->
  i (fun x ->
      match f x with
      | Some y -> k y
      | None -> ())

let append (a : _ t) (b : _ t) : _ t =
 fun k ->
  a k;
  b k

let to_list_rev (self : _ t) : _ list =
  let l = ref [] in
  self (fun x -> l := x :: !l);
  !l

let to_list (self : _ t) : _ list = to_list_rev self |> List.rev
