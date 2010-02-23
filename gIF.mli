type t
val from_file : string -> t
val to_file : t -> string -> unit
val get_image : t -> int -> Image.image
val from_image : Image.image -> t

exception Error of string
