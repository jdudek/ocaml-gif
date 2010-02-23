
(* Modul implementuje listy leniwe bez spamietywania. *)

type 'a t = Cons of 'a * (unit -> 'a t) | Nil

val take : int -> 'a t -> 'a list

val iter : ('a -> unit) -> 'a t -> unit

val map : ('a -> 'b) -> 'a t -> 'b t

val flatten_map : ('a -> 'b list) -> 'a t -> 'b t

val of_stream : 'a Stream.t -> 'a t

val of_string : string -> char t


