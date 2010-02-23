
(* --- dekompresja ------------------------------------------------ *)

(* konwertuje bajt (int < 256) na liste bitow (int 0-1), poczawszy od
   najmlodszego bitu *)
let byte_to_bits =
  let conv byte = 
    [ (byte land 1)        ; (byte land 2) lsr 1   ;
      (byte land 4) lsr 2  ; (byte land 8) lsr 3   ;
      (byte land 16) lsr 4 ; (byte land 32) lsr 5  ;
      (byte land 64) lsr 6 ; (byte land 128) lsr 7 ]
  in let arr = Array.create 256 []
  in
    for i = 0 to 255 do
      arr.(i) <- conv i;
    done;
    let b2b byte = arr.(byte)
    in b2b
;;

(* konwertuje liste bajtow na liste bitow, w kolejnosci zgodnej z
   kolejnoscia bitow w kodach LZW *)
let bytes_to_bits =
  LazyList.flatten_map byte_to_bits
;;

let bits_from_string str =
  let len = String.length str * 8 in
  let rec next n =
    if n < len then
      LazyList.Cons (
	((int_of_char(str.[n / 8]) lsr (n mod 8)) land 1),
	fun () -> next (n+1)
      )
    else
      LazyList.Nil
  in
    next 0
;;

(* z listy bitow wyciaga kod (int) o wskazanej dlugosci *)
let rec get_next_code input length = match input, length with
  | _, 0 -> (0, input)
  | LazyList.Cons (x, xs), n ->
      let (code', input') = get_next_code (xs ()) (n-1) in
	(x + (code' lsl 1), input')
  | LazyList.Nil, _ -> failwith "get_next_code"
;;

(* wlasciwa dekompresja: dla listy bitow z kolejnymi kodami zwraca
   liste zdekodowanych bajtow *)
let decode_bits input init_code_size =
  let clear_code = 1 lsl init_code_size in
  let end_code = clear_code + 1 in
  let dict = Array.make 4096 [] in
  let rec decode input code_size avail_code prev_entry =
    let (code, tail) = get_next_code input code_size in
      if code = clear_code then
	let new_code_size = (init_code_size + 1) in
	let (next_code, tail') = get_next_code tail new_code_size in
	  LazyList.Cons (
	    [next_code],
	    fun () -> 
	      decode tail' new_code_size (clear_code + 2) [next_code]
	  )
      else if code = end_code then
	LazyList.Nil
      else
	let entry =    
	  if code < clear_code then
	    [code]
	  else if code < avail_code then
	    dict.(code)
	  else
	    prev_entry @ [List.hd prev_entry]
	in
	  dict.(avail_code) <- (prev_entry @ [List.hd entry]) ;
	  let new_avail_code = (avail_code + 1) in
	  let new_code_size =
	    if new_avail_code  >= (1 lsl code_size) then
	      code_size + 1 
	    else
	      code_size
	  in
	    LazyList.Cons (
	      entry,
	      fun () ->
		decode tail new_code_size new_avail_code entry
	    )
  in
    decode input (init_code_size + 1) (clear_code + 2) []
;;

(* odpala dekompresje dla danego wejscia w postaci listy bajtow *)
let decode input code_size =
  decode_bits (bytes_to_bits input) code_size
;;


(* --- kompresja -------------------------------------------------- *)

(* Kompresja operuje na symbolach (symbolami sa numery kolorow z
   palety). Symbole sa skladane w ciagi, reprezentowane za pomoca
   list. Ciagom symboli przypisywane sa kody. Do mapowania list
   symboli na ich kody korzystamy ze slownika opartego o modul Map. *)

module EncDictOrderedType : (Map.OrderedType with type t = int list) =
struct
  type t = int list
  let compare = Pervasives.compare
end ;;

module EncDict =
struct
  include Map.Make (EncDictOrderedType)

  (* Jesli ciag symboli ma tylko jeden element, to kodem takiego ciagu
     jest po prostu wartosc tego symbolu. Takich ciagow nie
     przechowujemy w slowniku. *)
  let find_word word dict =
    match word with
      | [c] -> c
      | _ -> find word dict
  ;;
end ;;

(* Kompresja: z listy symboli (bajtow) funkcja tworzy liste
   kodow. Wynikowa list ma postac par: (kod, ilosc bitow), gdzie kod
   to int < 4096, ilosc bitow <= 12. *)
let make_codes input init_code_size =
  let clear_code = 1 lsl init_code_size in
  let end_code = clear_code + 1 in
  let rec encode input dict word code_size avail_code =
    match input with
      | LazyList.Cons (char, input_tail) ->
	  if word = [] then
	    LazyList.Cons (
	      (clear_code, code_size),
	      fun () -> 
		(encode (input_tail ()) dict [char] 
		   (init_code_size + 1) (clear_code + 2)
		)
	    )
	  else
	    let word_char = char :: word in
	      if EncDict.mem word_char dict then
		encode
		  (input_tail ()) dict word_char code_size avail_code
	      else
		let code = EncDict.find_word word dict in
		let new_avail_code = avail_code + 1 in
		let new_code_size = 
		  if new_avail_code > (1 lsl code_size) then
		    code_size + 1
		  else
		    code_size in
		let new_dict = EncDict.add word_char avail_code dict in
		  (* jesli wykorzystano juz wszystkie 12-bitowe kody,
		     zwracamy clear_code, czyscimy slownik i resetujemy
		     dlugosc kodu do dlugosci poczatkowej *)
		  if new_avail_code >= 0xFFF then
		    LazyList.Cons (
		      (code, code_size),
		      fun () -> LazyList.Cons (
			(clear_code, code_size),
			fun () -> encode (input_tail()) EncDict.empty
			  [char] (init_code_size + 1) (clear_code + 2)
		      )
		    )
		  else
		    LazyList.Cons (
		      (code, code_size),
		      fun () -> encode
			(input_tail ()) new_dict [char] 
			new_code_size new_avail_code
		    )
      | LazyList.Nil ->
	  (* kiedy skonczy sie wejscie, zwracamy kod dla symboli
	     bedacych w buforze (o ile sa takie) oraz kod oznaczajacy
	     koniec *)
	  let ending =
	    LazyList.Cons ((end_code, code_size), fun () -> LazyList.Nil)
	  in
	    if word != [] then
	      LazyList.Cons (
		(EncDict.find_word word dict, code_size),
		fun () -> ending
	      )
	    else ending
  in
    encode input EncDict.empty [] (init_code_size + 1) (clear_code + 2)
;;

(* konwertuje kod (12-bitow int) na liste bitow, poczawszy od
   najmlodszego bitu *)
let code_to_bits =
  let conv byte = 
    [ (byte land 1)           ; (byte land 2) lsr 1   ;
      (byte land 4) lsr 2     ; (byte land 8) lsr 3   ;
      (byte land 16) lsr 4    ; (byte land 32) lsr 5  ;
      (byte land 64) lsr 6    ; (byte land 128) lsr 7 ;
      (byte land 256) lsr 8   ; (byte land 512) lsr 9 ;
      (byte land 1024) lsr 10 ; (byte land 2048) lsr 11 ]
  in let arr = Array.create 4096 []
  in
    for i = 0 to 4095 do
      arr.(i) <- conv i;
    done;
    let b2b byte = arr.(byte)
    in b2b
;;

(* Zwraca pierwsze n elementow listy. Rekursja nie jest ogonowa, ale
   uzywamy tej funkcji tylko na 12-elementowych listach. *)
let rec list_take n xs =
  match n, xs with
    | 0, xs -> []
    | n, [] -> []
    | n, x::xs -> x :: list_take (n-1) xs 
;;

(* Z listy kodow w tworzy liste reprezentujacych je bitow *)
let make_output codes_stream =
  let fn (code, size) =
    list_take size (code_to_bits code)
  in
    LazyList.flatten_map fn codes_stream
;;

(* Liste bitow sklada w bajty *)
let pack_codes_bits bits =
  let rec pack bits acc n =
    match bits with
      | LazyList.Cons (bit, tail) ->
	  if n < 8 then
	    pack (tail ()) (acc + (bit lsl n)) (n + 1)
	  else
	    LazyList.Cons (acc, fun () -> pack (tail ()) bit 1)
      | LazyList.Nil ->
	  LazyList.Cons (acc, fun () -> LazyList.Nil)
  in
    pack bits 0 0
;;

let encode pixels_list code_size =
    (pack_codes_bits (make_output (make_codes pixels_list code_size)))
;;
