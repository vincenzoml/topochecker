(* This file ripped off from camlimages, all credits goes to the original authors *)

exception Wrong_file_type

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004                                                *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

let (<<) = Bytes.set

let (&) = fun x y -> x y	     

(*
  Caml representation of a bmp bit map image.
  Fields are Caml values, decoded versions of raw data in the file.

  Structure of bitmaps files on disk :
   - BITMAPFILEHEADER    : bytes 0 to 14 excluded
   - BITMAPINFOHEADER    : bytes 14 to 54 excluded
   - RGBQUAD []          : color map
   - BYTES []            : bit map
*)

type rgb = { mutable r : int; mutable g : int; mutable b : int }

type bmp = {
  bmpFileHeader : bitmapfileheader;           (* Bytes <0  14< *)
  bmpInfoHeader : bitmapinfoheader;           (* Bytes <14 54< *)
  bmpRgbQuad : rgb array;                     (* Bytes <54 ... *)
  bmpBytes : string;                          (* Bytes <bfOffBits ... *)
}

and bitmapfileheader = {
    (* WORD: that is 2 bytes *) bfType : int;  (* Bytes <0   2< *)
    (* DWORD: that is 2 WORDs *) bfSize : int; (* Bytes <2   6< *)
    (* WORD *) bfReserved1 : int;              (* Bytes <6   8< *)
    (* WORD *) bfReserved2 : int;              (* Bytes <8  10< *)
    (* DWORD *) bfOffBits : int;               (* Bytes <10 14< *)
}

and bitmapinfoheader = {
    (* DWORD *) biSize : int;                  (* Bytes <14 18< *)
    (* DWORD *) biWidth : int;                 (* Bytes <18 22< *)
    (* DWORD *) biHeight : int;                (* Bytes <22 26< *)
    (* WORD *) biPlanes  : int;                (* Bytes <26 28< *)
    (* WORD *) biBitCount : bibitcount;        (* Bytes <28 30< *)
    (* DWORD *) biCompression : bicompression; (* Bytes <30 34< *)
    (* DWORD *) biSizeImage : int;             (* Bytes <34 38< *)
    (* DWORD *) biXPelsPerMeter : int;         (* Bytes <38 42< *)
    (* DWORD *) biYPelsPerMeter : int;         (* Bytes <42 46< *)
    (* DWORD *) biClrUsed : int;               (* Bytes <46 50< *)
    (* DWORD *) biClrImportant : int           (* Bytes <50 54< *)
}

and bicompression =
  | BI_RGB
    (* Specifies that the bitmap is not compressed. *)
  | BI_RLE8
    (* Specifies a run-length encoded format for bitmaps with 8 bits
       per pixel. The compression format is a two-bytes format
       consisting of a count byte followed by a byte containing a color
       index. *)
  | BI_RLE4
    (* Specifies a run-length encoded format for bitmaps with 4 bits
       per pixel. The compression format is a two-byte format consisting of
       a count byte followed by two word-length color indexes. *)

and bibitcount =
  | Monochrome
  (* 1	The bitmap is monochrome, and the bmiColors field must 
     contain two entries. Each bit in the bitmap array represents a 
     pixel. If the bit is clear, the pixel is displayed with the
     color of the first entry in the bmiColors table; if the bit is
     set, the pixel has the color of the second entry in the
     table. *)
  | Color16
  (* 4	The bitmap has a maximum of 16 colors, and the bmiColors 
     field contains up to 16 entries. Each pixel in the bitmap is 
     represented by a four-bit index into the color table.
     For example, if the first byte in the bitmap is 0x1F,  then the 
     byte represents two pixels. The first pixel contains the color 
     in the second table entry, and the second pixel contains the 
     color in the 16th table entry. *)
  | Color256
  (* 8	The bitmap has a maximum of 256 colors, and the bmiColors 
     field contains up to 256 entries. In this case, each byte in the 
     array represents a single pixel. *)
  | ColorRGB
  (* 24 The bitmap has a maximum of 2^24 colors. The bmiColors
     field is NULL, and each three bytes in the bitmap array
     represents the relative intensities of red, green, and blue,
     respectively, of a pixel. *)
  | ColorRGBA
  (* 32 The bitmap, RGBA *)


(* =================================================================== *)
(* ============= Reading bmp files as Caml images. =================== *)
(* =================================================================== *)


(* # of bytes read during loading : should be equal to the size of the file *)
let bytes_read = ref 0

let read_byte ic = incr bytes_read; input_byte ic
let skip_byte ic = incr bytes_read; ignore (input_byte ic)


let read_word ic =
 let b0 = read_byte ic in
 let b1 = read_byte ic in
 (* little-endian form *)
 (b1 lsl 8) + b0


let read_dword ic =
 let b0 = read_byte ic in
 let b1 = read_byte ic in
 let b2 = read_byte ic in
 let b3 = read_byte ic in
 (* little-endian form *)
 (b3 lsl 24) + (b2 lsl 16) + (b1 lsl 8) + b0


let read_bit_count ic =
 match read_word ic with
 | 1 -> Monochrome | 4 -> Color16 | 8 -> Color256 | 24 -> ColorRGB | 32 -> ColorRGBA
 | n -> failwith ("invalid colors number : " ^ string_of_int n)


(*
#define BI_RGB      0L
#define BI_RLE8     1L
#define BI_RLE4     2L *)
let read_compression ic =
 match read_dword ic with
 | 0 -> BI_RGB | 1 -> BI_RLE8 | 2 -> BI_RLE4
 | n -> failwith ("invalid compression mode : " ^ string_of_int n)



(* Entries of color maps stored on disk have the following format
typedef struct tagRGBQUAD {
   BYTE    rgbBlue;
   BYTE    rgbGreen;
   BYTE    rgbRed;
   BYTE    rgbReserved;
} RGBQUAD;

The RGBQUAD structure contains the following fields:

Field         Description
rgbBlue       Specifies the intensity of blue in the color.
rgbGreen      Specifies the intensity of green in the color.
rgbRed        Specifies the intensity of red in the color.
rgbReserved   Is not used and must be set to zero.

When loading a bmp we simply skip the rgbReserved field. *)
let load_rgbquad ic =
 let b = read_byte ic in
 let g = read_byte ic in
 let r = read_byte ic in
 let _u = read_byte ic in
 { b = b; g = g; r = r; }


let load_bitmapfileheader ic =
  let bfType = read_word ic in
  if bfType <> 19778 (* BM *) then failwith "Invalid file tag";
  let bfSize = read_dword ic in
  let bfReserved1 = read_word ic in
  let bfReserved2 = read_word ic in
  let bfOffBits = read_dword ic in
  { bfType; bfSize; bfReserved1; bfReserved2; bfOffBits; }


let load_bitmapinfoheader ic =
  try
  (* Found a tagBITMAPINFO *)
  let biSize = read_dword ic in
  let biWidth = read_dword ic in
  let biHeight = read_dword ic in
  let biPlanes = read_word ic in
  let biBitCount = read_bit_count ic in
  let biCompression = read_compression ic in
  let biSizeImage = read_dword ic in
  let biXPelsPerMeter = read_dword ic in
  let biYPelsPerMeter = read_dword ic in
  let biClrUsed = read_dword ic in
  let biClrImportant = read_dword ic in
  (* header = tagBITMAPINFOHEADER *)
  { biSize; biWidth; biHeight;
    biPlanes; biBitCount;
    biCompression; biSizeImage;
    biXPelsPerMeter; biYPelsPerMeter;
    biClrUsed; biClrImportant;
  }
  with
  | (Failure s as e) -> 
      prerr_endline s;
      raise e


let load_colors bfh _bih ic =
 (* Reading RGBQUADs *)
 (* If biClrUsed = 0 then the whole color range is used, else only *)
 (* the amount given by biClrUsed is effectively used in the bmp.  *)
 (* But in any case, the size of the color map is stored in the file.  *)
 let cmaplength =
     (* Color map starts from byte number 54, and ends at *)
     (* beginning of the bitmap (actual image data, i.e. pixels of the bmp).*)
     (* bfOffBits = offset from bfh to actual image data. *)
     (* 40 = sizeof (bfh), 14 = sizeof (bih).             *)
     (* Hence color map length is bfOffBits - 54. *)
     (* Useful to load images with biClrUsed handled incorrectly *)
     (* In fact, some savers store the whole colormap, instead of *)
     (* the number of entries given by biClrUsed... *)
     (bfh.bfOffBits - 54) / 4 in
 Array.init cmaplength (fun _i -> load_rgbquad ic)

(* Loads image data when image has 8 bit depth *)
let load_image8data bih ic =
 let bitmap = Bytes.create (bih.biWidth * bih.biHeight) in
 match bih.biCompression with
 | BI_RGB ->
     (* No compression : lines are stored in reverse order *)
     (* 'bih.biWidth' is padded to be a multiple of 4 pixels (32 bits) *)
     let pad = ((bih.biWidth + 3) / 4) * 4 in
     (* Reading *)
     for i = bih.biHeight - 1 downto 0 do
       let bitmapindex = ref (i * bih.biWidth) in
       for j = 0 to pad - 1 do
         let c = Char.chr (read_byte ic) in
         if j < bih.biWidth then Bytes.set bitmap !bitmapindex c;
         incr bitmapindex
         done
       done;
     bitmap
 | BI_RLE8 ->
     (* Run-length encoded format for bitmaps with 8 bits per pixel *)
     (* Coordinates of the current point in the image *)
     let x = ref 0 in
     let y = ref 0 in
     let bitmapindex = ref (!x + (bih.biHeight - !y - 1) * bih.biWidth) in
     while !y < bih.biHeight do
       match read_byte ic with
       (* Absolute mode, if second byte is between 03H and FFH.
          Encoded mode, with escape code otherwise. *)
       | 0 ->
           (* Escape codes mode *)
           begin
           match read_byte ic with
           | 0 ->
               (* End of line code *)
               x := 0;
               incr y;
               bitmapindex := !x + (bih.biHeight - !y - 1) * bih.biWidth
           | 1 ->
               (* End of bitmap : force exit *)
               y := bih.biHeight
           | 2 ->
               (* Delta *)
               let c1 = read_byte ic in
               x := !x + c1;
               let c2 = read_byte ic in
               y := !y + c2;
               bitmapindex := !x + (bih.biHeight - !y - 1) * bih.biWidth
           | c ->
               (* c should be between 03H and FFH *)
               (* Absolute mode:
                  c represents the number of bytes which follow,
                  each of which contains the color index of a single pixel. *)
               for _i = 0 to c - 1 do
                 let c1 = read_byte ic in
                 Bytes.set bitmap !bitmapindex (Char.chr c1);
                 incr x;
                 incr bitmapindex
               done;
               (* Odd length run: read an extra pad byte *)
               if c land 1 <> 0 then skip_byte ic
           end
       | c ->
           (* Encoded mode *)
           let c1 = read_byte ic in
           for _i = 0 to c - 1 do
             Bytes.set bitmap !bitmapindex (Char.chr c1);
             incr x;
             incr bitmapindex
           done
     done;
     bitmap
 | BI_RLE4 ->
     failwith ("Invalid compression mode : BI_RLE4")

let load_image1data bih ic =
 let bitmap = Bytes.create (bih.biWidth * bih.biHeight) in
 let c = ref 0 in
 (* each scan line 'w', is padded to be a multiple of 32 *)
 let pad = ((bih.biWidth + 31) / 32) * 32 in

 for i = bih.biHeight - 1 downto 0 do
   let bitmapindex = ref (i * bih.biWidth) in
   let bnum = ref 0 in
   for j = 0 to pad - 1 do
     if !bnum land 7 = 0 then
      begin
       c := read_byte ic;
       bnum := 0;
      end;
     if j < bih.biWidth then
      begin
       Bytes.set bitmap !bitmapindex (if !c land 0x80 <> 0 then '\001' else '\000');
       incr bitmapindex;
       c := !c lsl 1;
      end;
     incr bnum
    done
 done;
 bitmap


let load_image4data bih ic =
 let bitmap = Bytes.create (bih.biWidth * bih.biHeight) in
 match bih.biCompression with
 | BI_RGB ->
    (* 'w' is padded to be a multiple of 8 pixels (32 bits) *)
    let pad = ((bih.biWidth + 7) / 8) * 8 in
    let c = ref 0 in

    for i = bih.biHeight - 1 downto 0 do
      let bitmapindex = ref (i * bih.biWidth) in
      let nyblenum = ref 0 in
      for j = 0 to pad -1 do
        if !nyblenum land 1 = 0 then
          begin
          (* Read the next byte *)
          c := read_byte ic;
          nyblenum := 0
          end;
        if j < bih.biWidth then
          begin
          Bytes.set bitmap !bitmapindex (Char.chr ((!c land 0xf0) lsr 4));
          incr bitmapindex;
          c := !c lsl 4
          end;
        incr nyblenum
      done
    done;
    bitmap
 | BI_RLE4 ->
    let x = ref 0 in
    let y = ref 0 in
    let bitmapindex = ref (!x + (bih.biHeight - !y - 1) * bih.biWidth) in
    let c1 = ref 0 in
    while !y < bih.biHeight do
      match read_byte ic with
      | 0 ->
          (* Escape codes *)
          begin
          match read_byte ic with
          | 0 ->
              (* End of line *)
              x := 0;
              incr y;
              bitmapindex := !x + (bih.biHeight - !y - 1) * bih.biWidth
          | 1 ->
              (* End of bitmap : force exit *)
              y := bih.biHeight
          | 2 ->
              (* Delta *)
              let c' = read_byte ic in
              x := !x + c';
              let c'' = read_byte ic in
              y := !y + c'';
              bitmapindex := !x + (bih.biHeight - !y - 1) * bih.biWidth
          | c ->
              (* Absolute mode *)
              for i = 0 to c - 1 do
                if i land 1 = 0 then c1 := read_byte ic;
                let c = if i land 1 <> 0 then !c1 else !c1 lsr 4 in
                Bytes.set bitmap !bitmapindex (Char.chr (c land 0x0F));
                incr x;
                incr bitmapindex
              done;
              (* Read pad byte *)
              if c land 3 = 1 || c land 3 = 2 then skip_byte ic
          end

      |  c ->
          (* Encoded mode *)
          let c1 = read_byte ic in
          let col1 = c1 land 0x0F
          and col2 = (c1 lsr 4) land 0x0F in
          for i = 0 to c - 1 do
            let c = if i land 1 <> 0 then col1 else col2 in
            Bytes.set bitmap !bitmapindex & Char.chr c;
            incr x;
            incr bitmapindex
          done
    done;
    bitmap
 | BI_RLE8 ->
    failwith ("Invalid compression mode : BI_RLE8")


let load_image24data bih ic =
  (* Bitmap is a string of RGB bytes *)
  let bitmap = Bytes.create ((bih.biWidth * bih.biHeight) * 3) in
  let pad = (4 - ((bih.biWidth * 3) mod 4)) land 0x03 in
  let pp = ref 0 in
  for i = bih.biHeight - 1 downto 0 do
    pp := (i * bih.biWidth * 3);
    for _j = 0 to bih.biWidth - 1 do
      bitmap << !pp + 2 & Char.chr (read_byte ic);   (* Blue *)
      bitmap << !pp + 1 & Char.chr (read_byte ic);   (* Green *)
      bitmap << !pp     & Char.chr (read_byte ic);   (* Red *)
      pp := !pp + 3
    done;
    for _j = 0 to pad - 1 do skip_byte ic done;
  done;
  bitmap


let load_image32data bih ic =
  (* Bitmap is a string of RGB bytes *)
  let bitmap = Bytes.create ((bih.biWidth * bih.biHeight) * 4) in
(*
  let pad = (4 - ((bih.biWidth * 4) mod 4)) land 0x03 in
  let pad = 1 in
*)
  let pp = ref 0 in
  for i = bih.biHeight - 1 downto 0 do
    pp := (i * bih.biWidth * 4);
    for _j = 0 to bih.biWidth - 1 do
      bitmap << !pp + 2 & Char.chr (read_byte ic);   (* Blue *)
      bitmap << !pp + 1 & Char.chr (read_byte ic);   (* Green *)
      bitmap << !pp + 0 & Char.chr (read_byte ic);   (* Red *)
      bitmap << !pp + 3 & Char.chr (read_byte ic);   (* Alpha *)
      pp := !pp + 4
    done;
(*
    for j = 0 to pad - 1 do skip_byte ic done;
*)
  done;
  bitmap


let load_imagedata bih ic =
 (* The bits in the array are packed together, but each scan line *)
 (* must be zero-padded to end on a LONG boundary. *)
 match bih.biBitCount with
 | Monochrome -> load_image1data bih ic
 | Color16 -> load_image4data bih ic
 | Color256 -> load_image8data bih ic
 | ColorRGB -> load_image24data bih ic
 | ColorRGBA -> load_image32data bih ic


let skip_to ic n =
  while !bytes_read <> n do skip_byte ic done

type 'a header = { header_width : int; header_height : int; header_infos : 'a list }

let check_header fname =
  let ic = open_in_bin fname in
  bytes_read := 0;
  try
    let _bfh = load_bitmapfileheader ic in
    let bih = load_bitmapinfoheader ic in
    close_in ic;
    { header_width = bih.biWidth;
      header_height = bih.biHeight;
      header_infos = []; }
  with
  | _ ->
      close_in ic;
      raise Wrong_file_type


let read_bmp ic =
  bytes_read := 0;
  let bfh = load_bitmapfileheader ic in
  let bih = load_bitmapinfoheader ic in
  let colormap = load_colors bfh bih ic in
  skip_to ic bfh.bfOffBits;
  let bitmap = load_imagedata bih ic in
  { bmpFileHeader = bfh;
    bmpInfoHeader = bih;
    bmpRgbQuad = colormap;
    bmpBytes = bitmap; }


let load_bmp fname =
  let ic = open_in_bin fname in
  let bmp = read_bmp ic in
  close_in ic;
  bmp


(* =================================================================== *)
(* ============= Writting images as bmp files. ======================= *)
(* =================================================================== *)


let bytes_written = ref 0

let write_byte oc b = incr bytes_written; output_byte oc b

let output_word oc w =
 (* little-endian form *)
 let b0 = w land 255 in
 let b1 = (w lsr 8) land 255 in
 output_byte oc b0;
 output_byte oc b1


let write_word oc w =
 output_word oc w;
 bytes_written := !bytes_written + 2


let output_dword oc dw =
 (* little-endian form *)
 let b0 = dw land 255 in
 let b1 = (dw lsr 8) land 255 in
 let b2 = (dw lsr 16) land 255 in
 let b3 = (dw lsr 24) land 255 in
 output_byte oc b0;
 output_byte oc b1;
 output_byte oc b2;
 output_byte oc b3


let write_dword oc dw =
 output_dword oc dw;
 bytes_written := !bytes_written + 4


let write_bit_count oc bc =
 let byte = match bc with
 | Monochrome -> 1 | Color16 -> 4 | Color256 -> 8 | ColorRGB -> 24 | ColorRGBA -> 32 in
 write_word oc byte


let write_compression oc c =
 let dword = match c with
 | BI_RGB -> 0 | BI_RLE8 -> 1 | BI_RLE4 -> 2 in
 write_dword oc dword


let write_rgbquad oc rgb =
 let b = rgb.b in
 let g = rgb.g in
 let r = rgb.r in
 let u = 0 in
 write_byte oc b;
 write_byte oc g;
 write_byte oc r;
 write_byte oc u


let write_bmpFileHeader oc = function {
    (* WORD *) bfType = bft;
    (* DWORD *) bfSize = bfs;
    (* WORD *) bfReserved1 = bfr1;
    (* WORD *) bfReserved2 = bfr2;
    (* DWORD *) bfOffBits = bfob
 } ->
   let start_index = !bytes_written in
   write_word oc bft;
   let bfSize_index = !bytes_written in
   write_dword oc bfs;
   write_word oc bfr1;
   write_word oc bfr2;
   let bfOffBits_index = !bytes_written in
   write_dword oc bfob;
   let end_bmpFileHeader = !bytes_written in
   start_index, bfSize_index, bfOffBits_index, end_bmpFileHeader

let write_bmpInfoHeader oc = function {
    (* DWORD *) biSize = bis;
    (* DWORD *) biWidth = biw;
    (* DWORD *) biHeight = bih;
    (* WORD *) biPlanes  = bip;
    (* WORD *) biBitCount = bibc;
    (* DWORD *) biCompression = bic;
    (* DWORD *) biSizeImage = bisi;
    (* DWORD *) biXPelsPerMeter = bixpm;
    (* DWORD *) biYPelsPerMeter = biypm;
    (* DWORD *) biClrUsed = bicu;
    (* DWORD *) biClrImportant = bici
 } ->
   let biSize_index = !bytes_written in
   write_dword oc bis;
   write_dword oc biw;
   write_dword oc bih;
   write_word oc bip;
   write_bit_count oc bibc;
   write_compression oc bic;
   let biSizeImage_index = !bytes_written in
   write_dword oc bisi;
   write_dword oc bixpm;
   write_dword oc biypm;
   write_dword oc bicu;
   write_dword oc bici;
   let end_bmpInfoHeader = !bytes_written in
   biSize_index, biSizeImage_index, end_bmpInfoHeader


let write_colors oc color_map =
 (* If color_map is empty, should output a NULL character *)
 if Array.length color_map = 0 then write_byte oc 0
 (* Otherwise write the rgb colors of the colormap *)
 else Array.iter (write_rgbquad oc) color_map


(* To denote the end of a scan line *)
let write_end_of_scan_line oc = write_byte oc 0; write_byte oc 0

(* To denote the end of the bitmap *)
let write_end_of_bitmap oc = write_byte oc 0; write_byte oc 1

(* Writing padding bytes. *)
let write_pad oc n = for _i = 0 to n - 1 do write_byte oc 0 done

(* Run length encoding: write the number n of pixels encoded *)
(* the color number given by color index c *)
let rec write_rle_code oc n c =
  if n <= 255 then begin
   write_byte oc n;
   write_byte oc c
  end else begin
   write_rle_code oc 255 c;
   write_rle_code oc (n - 255) c end


let write_rle oc n char = write_rle_code oc n (Char.code char)

(* In biRLE4 encoded mode the color byte is interpreted as two 4 bits
   colors to alternatively write even and odd pixels.
   Color is a char with 4 significant bytes.
   We duplicate them to get 2 identical colors, for run-length encoding. *)
let write_rle4 oc n char =
  let code = Char.code char in
  write_rle_code oc n (code lsl 4 + code)


(* (4 - (n mod 4)) mod 4 *)
let pad_bytes n = (4 - (n mod 4)) land 0x03

let write_image1data bmp oc =
 let bih = bmp.bmpInfoHeader in
 if bih.biCompression <> BI_RGB
  then failwith "invalid compression for a monochrome bitmap" else

 let start_bitmap_index = !bytes_written in
 let bitmap = bmp.bmpBytes in
 let width = bih.biWidth in
 let height = bih.biHeight in

 let extra_padding_bytes = pad_bytes ((width + 7) / 8) in

 for i = height - 1 downto 0 do
  (* For each pixel in the line *)
  let start = i * width in
  let lim = (i + 1) * width - 1 in
  let rec write_line x count accu =
   if count = 8 then begin
     write_byte oc accu;
     if x <= lim then write_line x 0 0 end else
   let cur = bitmap.[x] in
   let chunk = Char.code cur lsl (7 - count) in
   let new_accu = chunk + accu in
   if x = lim then write_byte oc new_accu
   else write_line (x + 1) (count + 1) new_accu in

  write_line start 0 0;
  (* No end of scan line in bi_RGB mode *)
  (* Padding *)
  write_pad oc extra_padding_bytes;
 done;
 let end_bitmap_index = !bytes_written in
 start_bitmap_index, end_bitmap_index


let write_image24data bmp oc =
 let bih = bmp.bmpInfoHeader in
 if bih.biCompression <> BI_RGB
  then failwith "invalid compression for a rgb bitmap" else

 let start_bitmap_index = !bytes_written in
 let bitmap = bmp.bmpBytes in
 let width = bih.biWidth in
 let height = bih.biHeight in

 let extra_padding_bytes = pad_bytes (width * 3) in

 for i = height - 1 downto 0 do
  (* For each pixel in the line *)
  let start = i * width * 3 in
  let lim = (i + 1) * width * 3 - 1 in
  let rec write_line x =
   write_byte oc (Char.code bitmap.[x + 2]);   (* Blue *)
   write_byte oc (Char.code bitmap.[x + 1]);   (* Green *)
   write_byte oc (Char.code bitmap.[x]);       (* Red *)
   let new_x = x + 3 in
   if new_x < lim then write_line new_x in

  write_line start;
  (* No end of scan line in bi_RGB mode *)
  (* Padding *)
  write_pad oc extra_padding_bytes;
 done;
 let end_bitmap_index = !bytes_written in
 start_bitmap_index, end_bitmap_index


let write_image32data bmp oc =
  let bih = bmp.bmpInfoHeader in
  if bih.biCompression <> BI_RGB
  then failwith "invalid compression for a rgba bitmap" else

  let start_bitmap_index = !bytes_written in
  let bitmap = bmp.bmpBytes in
  let width = bih.biWidth in
  let height = bih.biHeight in

(*
  let extra_padding_bytes = pad_bytes (width * 4) in
*)

  for i = height - 1 downto 0 do
  (* For each pixel in the line *)
    let start = i * width * 3 in
    let lim = (i + 1) * width * 4 - 1 in
    let rec write_line x =
      write_byte oc (Char.code bitmap.[x + 3]);   (* Alpha *)
      write_byte oc (Char.code bitmap.[x + 2]);   (* Blue *)
      write_byte oc (Char.code bitmap.[x + 1]);   (* Green *)
      write_byte oc (Char.code bitmap.[x]);       (* Red *)
      let new_x = x + 4 in
      if new_x < lim then write_line new_x in

    write_line start;
  (* No end of scan line in bi_RGB mode *)
(*
  (* Padding *)
    write_pad oc extra_padding_bytes;
*)
  done;
  let end_bitmap_index = !bytes_written in
  start_bitmap_index, end_bitmap_index


let write_image4data bmp oc =
 let bih = bmp.bmpInfoHeader in

 let start_bitmap_index = !bytes_written in
 let bitmap = bmp.bmpBytes in
 let width = bih.biWidth in
 let height = bih.biHeight in

 match bih.biCompression with

 | BI_RGB ->
    (* 'w' is padded to be a multiple of 8 pixels (32 bits) *)
    let extra_padding_bytes = pad_bytes ((width + 1) / 2) in

    for i = height - 1 downto 0 do
     (* For each pixel in the line *)
     let start = i * width in
     let lim = (i + 1) * width - 1 in
     let rec write_line x count accu =
      if count = 2 then begin
       write_byte oc accu;
       if x <= lim then write_line x 0 0 end else
      let cur = bitmap.[x] in
      let chunk = Char.code cur lsl (4 - count) in
      let new_accu = chunk + accu in
      if x = lim then write_byte oc new_accu
      else write_line (x + 1) (count + 1) new_accu in

     write_line start 0 0;
     (* Padding *)
     write_pad oc extra_padding_bytes;
    done;
    let end_bitmap_index = !bytes_written in
    start_bitmap_index, end_bitmap_index

 | BI_RLE4 ->

    (* We compress in encoded mode, not in absolute mode. *)
    (* So we do not have to align each run. *)
    (* However, each scan line is padded to be a multiple of 8 *)
    (* pixels (32 bits) *)
    (* For each line *)
    for i = height - 1 downto 0 do
     (* For each pixel in the line *)
     let start = i * width in
     let lim = (i + 1) * width - 1 in
     let rec write_line x count pred =
      let cur = bitmap.[x] in
      if cur = pred then
       if x = lim then write_rle4 oc (count + 1) pred
       else write_line (x + 1) (count + 1) pred
      else begin
       write_rle4 oc count pred;
       if x = lim then write_rle4 oc 1 cur
       else write_line (x + 1) 1 cur
      end in
     write_line start 0 bitmap.[start];
     write_end_of_scan_line oc;
     (* No padding in this mode *)
    done;
    write_end_of_bitmap oc;
    let end_bitmap_index = !bytes_written in
    start_bitmap_index, end_bitmap_index

 | BI_RLE8 ->
    failwith ("Invalid compression mode : BI_RLE8")

let write_image8data bmp oc =
 let bih = bmp.bmpInfoHeader in

 let start_bitmap_index = !bytes_written in
 let bitmap = bmp.bmpBytes in
 let width = bih.biWidth in
 let height = bih.biHeight in

 match bih.biCompression with

 | BI_RGB ->
    (* 'w' is padded to be a multiple of 8 pixels (32 bits) *)
    let extra_padding_bytes = pad_bytes width in

    for i = height - 1 downto 0 do
     (* For each pixel in the line *)
     let start = i * width in
     let lim = (i + 1) * width - 1 in
     let rec write_line x =
      let cur = bitmap.[x] in
      write_byte oc (Char.code cur);
      if x < lim then write_line (x + 1) in

     write_line start;
     (* Padding *)
     write_pad oc extra_padding_bytes;
    done;
    let end_bitmap_index = !bytes_written in
    start_bitmap_index, end_bitmap_index

 | BI_RLE8 ->

    (* We compress in encoded mode, not in absolute mode. *)
    (* So we do not have to align each run. *)
    (* However, each scan line is padded to be a multiple of 8 *)
    (* pixels (32 bits) *)
    (* For each line *)
    for i = height - 1 downto 0 do
     (* For each pixel in the line *)
     let start = i * width in
     let lim = (i + 1) * width - 1 in
     let rec write_line x count pred =
      let cur = bitmap.[x] in
      if cur = pred then
       if x = lim then write_rle oc (count + 1) pred
       else write_line (x + 1) (count + 1) pred
      else begin
       write_rle oc count pred;
       if x = lim then write_rle oc 1 cur
       else write_line (x + 1) 1 cur
      end in
     write_line start 0 bitmap.[start];
     write_end_of_scan_line oc;
     (* No padding in this mode *)
    done;
    write_end_of_bitmap oc;
    let end_bitmap_index = !bytes_written in
    start_bitmap_index, end_bitmap_index

 | BI_RLE4 ->
    failwith ("Invalid compression mode : BI_RLE8")

let write_image_data oc bmp =
  let bih = bmp.bmpInfoHeader in
  match bih.biBitCount with
  | Monochrome -> write_image1data bmp oc
  | Color16 -> write_image4data bmp oc
  | Color256 -> write_image8data bmp oc
  | ColorRGB -> write_image24data bmp oc
  | ColorRGBA -> write_image32data bmp oc


let write_bmp oc = function
 { bmpFileHeader = bmpFileHeader;
   bmpInfoHeader = bmpInfoHeader;
   bmpRgbQuad = colormap;
   bmpBytes = _bitmap } as bmp ->
 bytes_written := 0;
 let start_index, bfSize_index, bfOffBits_index, end_bmpFileHeader =
   write_bmpFileHeader oc bmpFileHeader in
 let start_bmpInfoHeader = end_bmpFileHeader in
 let biSize_index, biSizeImage_index, end_bmpInfoHeader =
   write_bmpInfoHeader oc bmpInfoHeader in

 write_colors oc colormap;

 let start_bitmap_index, end_bitmap_index =
  write_image_data oc bmp in

 (* Correcting sizes: bfSize, bfOffBits, biSize, bisizeImage *)
 let bfSize = (* Given in bytes! not in DWORDs *)
       !bytes_written - start_index in
 seek_out oc bfSize_index;
 output_dword oc bfSize;

 let bfOffBits = (* Given in bytes *)
       start_bitmap_index - start_index in
 seek_out oc bfOffBits_index;
 output_dword oc bfOffBits;

 let biSize = (* Given in bytes *)
       end_bmpInfoHeader - start_bmpInfoHeader in
 seek_out oc biSize_index;
 output_dword oc biSize;

 let biSizeImage = (* Given in bytes *)
       end_bitmap_index - start_bitmap_index in
 seek_out oc biSizeImage_index;
 output_dword oc biSizeImage


let save_bmp fname bmp =
 let oc = open_out_bin fname in
 write_bmp oc bmp;
 close_out oc

