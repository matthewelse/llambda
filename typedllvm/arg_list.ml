open! Core

type ('return, 'args) t =
  | Cons : ('a * ('return, 'b) t) -> ('return, 'a -> 'b) t
  | Returns : 'return -> ('return, 'return) t

let returns x = Returns x
let ( @-> ) l r = Cons (l, r)
