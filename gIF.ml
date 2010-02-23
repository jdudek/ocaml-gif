
(* Modul obslugujacy pliki GIF. Udostepnia typ t, reprezentujacy plik
   ("kontener") GIF. Pozostale typy to wewnetrzna reprezentacja
   zawartosci GIFa.
*)


(* --- typy danych ------------------------------------------------ *)

exception Error of string

type version = GIF87a | GIF89a

type info = { 
  version : version ;
  screen_width : int ;
  screen_height : int ;
  global_color_table : ColorTable.t option;
  global_color_table_sort : bool ;
  global_color_table_size : int ; (* liczba kolorow = 2 ^ (gct_size+1) *)
  original_color_res : int ;
  original_aspect_ratio : int ;
  background_color_index : int ;
}

type image_descriptor = {
  image_left_pos : int ;
  image_top_pos : int ;
  image_width : int ;
  image_height: int ;
  local_color_table : ColorTable.t option ;
  local_color_table_sort : bool ;
  local_color_table_size : int ;
  interlace_flag : bool ;
}

type graphic_control_ext = {
  disposal_method : int ;
  user_input : bool ;
  transparent_color : int option; (* indeks w tabeli kolorow *)
  delay_time : int ;
}

type image_block = {
  image_descriptor : image_descriptor ;
  image_control : graphic_control_ext option ;
  image_data : int LazyList.t ;
  image_lzw_code_size : int ;
}

type app_ext = {
  app_identifier : string ;
  app_auth_code : string ;
  app_data : string ;
}

type block = 
  | ImageBlock of image_block
  | CommentBlock of string
  | UnknownExtBlock of string
  | AppExtBlock of app_ext
  | Trailer
;;


type t = {
  stream_descriptor : info ;
  blocks : block list ;
}

let default_info = {
  version = GIF89a ;
  screen_width = 0 ;
  screen_height = 0 ;
  global_color_table = None ;
  global_color_table_sort = false ;
  global_color_table_size = 0 ;
  original_color_res = 0 ;
  original_aspect_ratio = 0 ;
  background_color_index = 0 ;
} ;;


(* --- funkcje pomocniczne ---------------------------------------- *)

(* wczytuje z pliku dwubajtowa liczbe bez znaku, pierwszy bajt
   jest mniej znaczacy *)
let read_uint16 inp =
  let b1 = input_byte inp in
  let b2 = input_byte inp in
    b1 + 256 * b2
;;

let write_uint16 f n =
  output_byte f n;
  output_byte f (n lsr 8);
;;

let int_of_option = function
  | Some _ -> 1
  | None -> 0
;;

let int_of_bool = function
  | true -> 1
  | false -> 0
;;

let input_string inp n =
  let buf = String.create n in
    really_input inp buf 0 n ;
    buf
;;

(* wyciaga kolejne pola bitowe z danego bajtu. Drugi argument
  to lista dlugosci poszczegolnych pol, poczawszy od bitu najbardziej
   znaczacego, np. [1; 3; 3; 1] *)
let unpack_fields byte lengths =
  let sum = List.fold_left (+) 0 
  and byte = byte
  in
  let rec unpack = function
    | [] -> []
    | x :: xs ->
	(byte lsr (sum xs)) land ((1 lsl x) - 1) :: unpack xs
  in unpack lengths
;;

(* odwrotnie. values i lengths musza miec taka sama dlugosc. wartosci
   w lengths sumuja sie do 8 *)
let pack_fields values lengths =
  let rec pack vs ls offset = match vs, ls with
    | (v :: vs), (l :: ls) ->
	((v land ((1 lsl l) - 1)) lsl (offset - l)) + 
	  pack vs ls (offset -l)
    | [], [] -> 0
    | _, _ -> failwith "pack_fields"
  in
    pack values lengths 8
;;


(* --- wczytywanie gifa ------------------------------------------- *)

let read_header info inp =
  let buf = String.create 6 in
    really_input inp buf 0 6 ;
    match buf with
      | "GIF87a" -> { info with version = GIF87a }
      | "GIF89a" -> { info with version = GIF89a }
      | _ -> raise (Error "Unsupported format")
;;

let read_logical_screen_descriptor info inp =
  let w = read_uint16 inp in
  let h = read_uint16 inp in
  let (gct, cres, gct_sort, gct_size) = 
    match unpack_fields (input_byte inp) [1; 3; 1; 3] with
      | [v1; v2; v3; v4] -> (v1, v2, v3, v4)
      | _ -> failwith "read_logical_screen_descriptor" in
  let bgc = input_byte inp in
  let ratio = input_byte inp in
    { info with
	screen_width = w ;
	screen_height = h ;
	global_color_table = 
  	if gct = 1 then Some (ColorTable.create 0) else None ;
	global_color_table_sort = (gct_sort = 1) ;
	global_color_table_size = gct_size ;
	original_color_res = cres ;
	original_aspect_ratio = ratio ;
	background_color_index = bgc ;
    }
;;

let read_color_table n inp =
  let tbl = ColorTable.create n
  in
    for i = 0 to n-1 do
      let r = input_byte inp in
      let g = input_byte inp in
      let b = input_byte inp in
	ColorTable.set tbl i (r, g, b)
    done ;
    tbl
;;

let read_global_color_table info s =
  let n = (2 lsl info.global_color_table_size) in
    { info with 
	global_color_table = Some (read_color_table n s )
    }
;;

let read_info s = 
  let hd_info = read_header default_info s in
  let desc = read_logical_screen_descriptor hd_info s in
    if (desc.global_color_table != None) then
      read_global_color_table desc s
    else
      desc
;;

let read_image_descriptor inp =
  let l = read_uint16 inp in
  let t = read_uint16 inp in
  let w = read_uint16 inp in
  let h = read_uint16 inp in
  let (lct, il, lct_sort, lct_size) =
    match unpack_fields (input_byte inp) [1; 1; 1; 2; 3] with
      | [v1; v2; v3; _; v4] -> (v1, v2, v3, v4)
      | _ -> failwith "read_image_descriptor"
  in
    { image_left_pos = l ;
      image_top_pos = t ;
      image_width = w ;
      image_height = h ;
      local_color_table =
	if (lct = 1) then Some (ColorTable.create 0) else None ;
      local_color_table_sort = (lct_sort = 1) ;
      local_color_table_size = lct_size ;
      interlace_flag = (il = 1) ;
    }
;;

(* Data subblocks to porcje danych dlugosc max 255 bajtow, poprzedzone
   jednym bajtem z wielkoscia danego bloku. *)
let read_data_subblocks inp = 
  let buf = Buffer.create 255 in
  let rec read () =
    match input_byte inp with
      | 0x00 -> ()
      | size ->
	  let data = input_string inp size in
	    Buffer.add_string buf data ;
	    read ()
  in
    try
      read ();
      Buffer.contents buf
    with End_of_file -> failwith "dupa" 
;;

let read_image_block inp =
  let desc =
    let desc = read_image_descriptor inp in
      if (desc.local_color_table != None) then
	let n = (2 lsl desc.local_color_table_size) in
	  { desc with local_color_table = Some (read_color_table n inp) }
      else
	desc
  in
  let lzw_cs = input_byte inp in
  let data = read_data_subblocks inp in
    ImageBlock {
      image_descriptor = desc ;
      image_control = None ;
      image_data = LazyList.map int_of_char (LazyList.of_string data) ;
      image_lzw_code_size = lzw_cs
    }
;;

let read_graphic_control_ext inp =
  ignore (input_byte inp); (* block size = 0x04, mozna pominac *)
  let (dm, ui, tc) =
    match unpack_fields (input_byte inp) [3; 3; 1; 1] with
      | [_; v1; v2; v3] -> (v1, v2, v3)
      | _ -> failwith "read_graphic_control_ext" in
  let delay = read_uint16 inp in
  let col_idx = input_byte inp in
    ignore (input_byte inp); (* block terminator = 0x00 *)
    { disposal_method = dm ;
      user_input = (ui = 1) ;
      transparent_color =
	if tc = 1 then Some col_idx else None ;
      delay_time = delay ;
    }
;;

let read_app_ext inp =
  ignore (input_byte inp); (* block size = 0x0B *)
  let app_id = input_string inp 8 in
  let app_code = input_string inp 3 in
  let data = read_data_subblocks inp
  in
    AppExtBlock {
      app_identifier = app_id ;
      app_auth_code = app_code ;
      app_data = data ;
    }
;;

let read_comment_ext inp =
  CommentBlock (read_data_subblocks inp)
;;

let read_unknown_ext inp =
  UnknownExtBlock (read_data_subblocks inp)
;;

let rec read_block inp =
  match input_byte inp with
    | 0x2C -> read_image_block inp
    | 0x21 -> (
	match input_byte inp with
	  | 0xF9 ->
	      let ctl = read_graphic_control_ext inp in (
		  match read_block inp with
		    | ImageBlock im -> 
			ImageBlock { im with image_control = Some ctl }
		    | b -> b
		)
	  | 0xFF -> read_app_ext inp
	  | 0xFE -> read_comment_ext inp
	  | _ -> read_unknown_ext inp
      )
    | 0x3B -> Trailer
    | 0x00 -> read_block inp
    | _ -> raise (Error "Unrecognized data")
;;

let from_file file_name = 
  let file = open_in_bin file_name in
  let info = read_info file in
  let rec read_blocks () =
    match read_block file with
      | Trailer -> []
      | b -> b :: read_blocks ()
  in let gif =
      { stream_descriptor = info ;
	blocks = read_blocks () ;
      }
  in
    close_in file ;
    gif
;;


(* --- zapis gifa ------------------------------------------------- *)

let write_header info out =
  match info.version with
    | GIF87a -> output_string out "GIF87a"
    | GIF89a -> output_string out "GIF89a"
;;

let write_logical_screen_descriptor info out =
  write_uint16 out info.screen_width;
  write_uint16 out info.screen_height;
  output_byte out
    (pack_fields 
       [ int_of_option (info.global_color_table) ;
	 info.original_color_res ;
	 int_of_bool (info.global_color_table_sort) ;
	 info.global_color_table_size ]
       [1; 3; 1; 3]);
  output_byte out info.background_color_index;
  output_byte out info.original_aspect_ratio;
;;

let write_color_table tbl out =
  for i = 0 to Array.length tbl - 1 do
    let (r, g, b) = tbl.(i) in
      output_byte out r ;
      output_byte out g ;
      output_byte out b ;
  done
;;

let write_info info out = 
  write_header info out ;
  write_logical_screen_descriptor info out ;
  match info.global_color_table with
    | Some gct -> write_color_table gct out
    | None -> ()
;;

let write_data_subblocks data out =
  let len = String.length data in
  let rec write from =
    if from < len - 255 then (
      output_byte out 255 ;
      output_string out (String.sub data from 255) ;
      write (from + 255)
    )
    else
      if from = len - 1 then
	output_byte out 0x00
      else (
	output_byte out (len - from) ;
	output_string out (String.sub data from (len - from)) ;
	output_byte out 0x00
      )
  in
    write 0
;;

let write_data_subblocks_from_list data out =
  let n = ref 0
  and buf = String.create 255 in
  let rec fn data =
    if !n = 255 then (
      output_byte out 0xFF ;
      output_string out buf ;
      n := 0 ;
      fn data
    )
    else
      match data with
	| LazyList.Cons (byte, tail) ->
	    buf.[!n] <- char_of_int (byte) ;
	    incr n ;
	    fn (tail ())
	| LazyList.Nil ->
	    output_byte out !n ;
	    output_string out (String.sub buf 0 !n ) ;
	    if !n > 0 then
	      output_byte out 0x00 ;
  in fn data
;;

let write_image_block img out =
  output_byte out 0x2C ;
  write_uint16 out img.image_descriptor.image_left_pos ;
  write_uint16 out img.image_descriptor.image_top_pos ;
  write_uint16 out img.image_descriptor.image_width ;
  write_uint16 out img.image_descriptor.image_height ;
  output_byte out
    (pack_fields 
       [ int_of_option (img.image_descriptor.local_color_table) ;
	 int_of_bool (img.image_descriptor.interlace_flag) ;
	 int_of_bool (img.image_descriptor.local_color_table_sort) ;
	 0 ;
	 img.image_descriptor.local_color_table_size 
       ]
       [1; 1; 1; 2; 3]) ;
  (match img.image_descriptor.local_color_table with
     | Some lct -> write_color_table lct out
     | None -> ()
  );
  output_byte out img.image_lzw_code_size ;
  write_data_subblocks_from_list img.image_data out ;
;;

let write_graphic_control_ext ctl out =
  output_byte out 0x21 ;
  output_byte out 0xF9 ;
  output_byte out 0x04 ; (* block size *)
  output_byte out
    (pack_fields 
       [0 ;
	ctl.disposal_method ;
	int_of_bool ctl.user_input ;
	int_of_option ctl.transparent_color ]
       [3; 3; 1; 1]);
  write_uint16 out ctl.delay_time ;
  output_byte out
    (match ctl.transparent_color with
       | Some c -> c
       | None -> 0
    );
  output_byte out 0x00 ; (* block terminator *)
;;

let rec write_block out block = match block with
  | ImageBlock img ->
      (match img.image_control with
	 | Some ctl -> write_graphic_control_ext ctl out
	 | None -> ()
      );
      write_image_block img out
  | CommentBlock data ->
      output_byte out 0x21 ;
      output_byte out 0xFE ;
      write_data_subblocks data out
  | UnknownExtBlock data ->
      () (* nieznane rozszerzenia ignorujemy *)
  | AppExtBlock app_ext ->
      output_byte out 0x21 ;
      output_byte out 0xFF ;
      output_byte out 0x0B ; (* block size *)
      output_string out app_ext.app_identifier ;
      output_string out app_ext.app_auth_code ;
      write_data_subblocks app_ext.app_data out
  | Trailer ->
      output_byte out 0x3B
;;

let to_file gif file =
  let out = open_out_bin file in
    write_info gif.stream_descriptor out ;
    List.iter (write_block out) gif.blocks ;
    write_block out Trailer ;
    close_out out 
;;


(* --- operacje na wczytanym gifie -------------------------------- *)

let get_frames gif =
  let flt = function
    | ImageBlock _ -> true
    | _ -> false
  and get_img = function
    | ImageBlock b -> b
    | _ -> failwith ""
  in
    List.map get_img (List.filter flt gif.blocks)
;;

(* Obrazki z przeplotem maja zmieniona kolejnosc wierszy. Ta funkcja
   zwraca kopie obrazka z prawidlowo uporzadkowanymi wierszami. *)
let deinterlace img =
  let new_pixels = 
    Array.make (img.Image.width * img.Image.height) (0, 0, 0)
  in
  let copy_row src dest =
    for i = 0 to img.Image.width - 1 do
      new_pixels.(src * img.Image.width + i) <-
	img.Image.pixels.(dest * img.Image.width + i) 
    done
  in
  let i = ref 0 and j = ref 0 in
    while !j < img.Image.height - 1 do
      copy_row !j !i ;
      incr i ;
      j := !j + 8 ;
    done ;
    j := 4 ;
    while !j < img.Image.height - 1 do
      copy_row !j !i ;
      incr i ;
      j := !j + 8 ;
    done ;
    j := 2 ;
    while !j < img.Image.height - 1 do
      copy_row !j !i ;
      incr i ;
      j := !j + 4 ;
    done ;
    j := 1 ;
    while !j < img.Image.height - 1 do
      copy_row !j !i ;
      incr i ;
      j := !j + 2 ;
    done ;
    { img with Image.pixels = new_pixels }
;;

(* Zwraca n-ta ramke obrazu zawartego w danym pliku GIF *)
let get_image gif n =
  let img =
    try
      (List.nth (get_frames gif) n)
    with Failure "nth" ->
      raise (Error "get_image: frame not found")
  in
  let w = img.image_descriptor.image_width
  and h = img.image_descriptor.image_height in
  let ct = match img.image_descriptor.local_color_table with
    | Some ct -> ct
    | None -> (
	match gif.stream_descriptor.global_color_table with 
	  | Some ct -> ct
	  | None -> raise (Error "No color table")
      )
  in
  let pixels = Array.make (w * h) (0, 0, 0) in
  let set_pixels list = 
    let counter = ref 0 in
      LazyList.iter
	(List.iter (fun px -> pixels.(!counter) <- ct.(px); incr counter))
	list
  in
    set_pixels (GifLzw.decode img.image_data img.image_lzw_code_size);
    let image =
      { Image.width =  w;
	Image.height =  h;
	Image.palette = ct ;
	Image.pixels = pixels
      }
    in
      if img.image_descriptor.interlace_flag then
	deinterlace image
      else
	image
;;


(* --- tworzenie gifa z obrazka ----------------------------------- *)

(* Dla danej palety tworzy funkcje, ktora wyszukuje indeks koloru
   zadanego przez (r, g, b). Zeby przeszukiwanie dzialalo w czasie
   stalym, tworzymy duza tablice *)
let find_color_fn palette = 
  let colors = 
    Bigarray.Array3.create Bigarray.int Bigarray.c_layout 256 256 256
  in
    Array.iteri
      (fun i (r, g, b) -> colors.{r,g,b} <- i)
      palette ;
    (function (r, g, b) -> Bigarray.Array3.get colors r g b)
;;

(* Zwraca skompresowane dane obrazka w postaci listy leniwej *)
let encode_image img code_size =
  let find_color = find_color_fn img.Image.palette in
  let pixels_list =
    let n = ref 0 in
    let rec fn () =
      if !n < Array.length img.Image.pixels then
	LazyList.Cons(
	  (incr n; find_color (img.Image.pixels.(!n - 1))),
	  fn
	)
      else LazyList.Nil
    in fn ()
  in GifLzw.encode (pixels_list) code_size
;;

(* Z danego obrazka tworzy caly kontener GIF z jedna ramka. Wymaga, by
   obrazek mial palete <= niz 256 kolorow *)
let from_image img =
  let ct_size =
    let n = Array.length img.Image.palette in
      if n <= 2 then 0
      else if n <= 4 then 1
      else if n <= 8 then 2
      else if n <= 16 then 3
      else if n <= 32 then 4
      else if n <= 64 then 5
      else if n <= 128 then 6
      else if n <= 256 then 7
      else raise (Error "from_image: too many colors")
  in
    (* minimalny rozmiar kodu to 2 *)
  let code_size = if ct_size < 2 then 2 else ct_size + 1 in
  let new_palette =
    let p = ColorTable.create (2 lsl (ct_size + 1)) in
      Array.iteri
	(fun n c -> ColorTable.set p n c)
	img.Image.palette ;
      p
  in
  let info =
    { default_info with
	screen_width = img.Image.width ;
	screen_height = img.Image.height ;
	global_color_table = Some (new_palette) ;
	global_color_table_size = ct_size ;
    }
  in
  let ctl =
    { disposal_method = 0;
      user_input = false;
      transparent_color = None;
      delay_time = 0 }
  in
  let blocks = 
    [ ImageBlock {
	image_descriptor = {
	  image_left_pos = 0;
	  image_top_pos = 0;
	  image_width = img.Image.width ;
	  image_height = img.Image.height ;
	  local_color_table = None ;
	  local_color_table_sort = false;
	  local_color_table_size = 0;
	  interlace_flag = false
	} ;
	image_control = Some ctl ;
	image_data = encode_image img code_size;
	image_lzw_code_size = code_size
      }]
  in {
      stream_descriptor = info ;
      blocks = blocks ;
    }
;;
