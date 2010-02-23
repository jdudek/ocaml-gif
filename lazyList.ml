
(* Modul implementuje listy leniwe bez spamietywania. *)

type 'a t = Cons of 'a * (unit -> 'a t) | Nil

(* zwraca zwykla liste z n pierwszych elementow leniwej listy xs *)
let rec take n xs  = 
  match n, xs with
    | 0, _ -> []
    | n, Nil -> []
    | n, Cons (x, xs) ->
	x :: take (n-1) (xs ()) 
;;

let rec iter f = function
    | Cons (x, xs) ->
	f x;
	iter f (xs ())
    | Nil -> ()
;;

let rec map f xs = match xs with
  | Cons (x, xs) ->
      Cons (f x, fun () -> map f (xs()) )
  | Nil ->
      Nil
;;

(* fkcja fn zwraca zwykle listy elementow. flatten_map aplikuje fn do
   kazedgo elementu listy wejsciowej i tworzy (leniwa) liste powstala
   przez splaszenie wyniku dzialania fn *)
let flatten_map fn inp =
  let rec flatten_map acc inp = 
    match acc with
      | (x :: xs) -> 
	  Cons (x, fun () -> flatten_map xs inp)
      | [] -> (
	  match inp with
	    | Cons (x, xs) -> flatten_map (fn x) (xs ())
	    | Nil -> Nil
	)
  in flatten_map [] inp
;;

let rec of_stream input =
  match Stream.peek input with
    | None -> Nil
    | Some a ->
	Stream.junk input ;
	Cons (a, fun () -> of_stream input)
;;

let of_string input =
  of_stream (Stream.of_string input)
;;
