open! Core

type ('return, 'args) t = private
  | Cons : ('a * ('return, 'b) t) -> ('return, 'a -> 'b) t
  | Returns : 'return -> ('return, 'return) t

val returns : 'a -> ('a, 'a) t
val ( @-> ) : 'a -> ('b, 'c) t -> ('b, 'a -> 'c) t
