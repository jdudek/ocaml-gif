    type color = int * int * int
    type t = color array
    val size : 'a array -> int
    val create : int -> (int * int * int) array
    val get : 'a array -> int -> 'a
    val set : 'a array -> int -> 'a -> unit
    val of_list : 'a list -> 'a array