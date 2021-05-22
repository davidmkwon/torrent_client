open Base
open Bencode_parser

type t =
  { announce: string (*what to do if torrent file doesn't have these fields?*)
  ; info_hash: Bytes.t
  ; piece_hashes: Bytes.t array
  ; piece_length: int
  ; length: int
  ; name: string
  }

let create file =
  let open Bencode in
  let bencode_dict = as_dict_exn (decode file) in
  let announce = fst (List.hd_exn bencode_dict) in
  let info_dict =
    match List.find bencode_dict ~f:(fun (x,_) -> String.equal x "info") with
    | Some dict_t -> dict_t
    | None -> failwith "did not find info field in torrent file"
  in
  ignore (announce);
  ignore (info_dict);
  file

(* super backwards way to get original info bencode because of original parsing,
 * need to manually add the prefaces. adding back the prefices is done via
 * [Bencode.to_original_string] *)
let info_hash info_dict =
  match info_dict with
  | (ln,_)::(nm,_)::(pl,_)::(pc,_)::[] when
      List.equal String.equal [ln;nm;pl;pc] ["length";"name";"piece
      length";"pieces"] ->
    let buf = Buffer.create 16 in (* 16 is some arbitrary initial size *)
    let info_dict_string = Bencode.to_original_string (Dict info_dict) in
    Buffer.add_string buf info_dict_string;
    Digestif.SHA1.digest_bytes (Buffer.contents_bytes buf)
  | _ -> failwith "info does not match desired format hurhur"
