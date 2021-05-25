open Base
open Bencode_parser

type t =
  { announce: string (*what to do if torrent file doesn't have these fields?*)
  ; info_hash: Bytes.t
  ; piece_hashes: Bytes.t array
  ; piece_length: int64
  ; length: int64
  ; name: string
  }

(* super backwards way to get original info bencode because of original parsing,
 * need to manually add the prefaces. adding back the prefices is done via
 * [Bencode.to_original_string] *)
let info_hash info_dict =
  match info_dict with
  | ("length",_)::("name",_)::("piece length",_)::("pieces",_)::[] ->
    let buf = Buffer.create 16 in (* 16 is some arbitrary initial size *)
    let info_dict_string = Bencode.to_original_string (Dict info_dict) in
    Buffer.add_string buf info_dict_string;
    Digestif.SHA1.(
      digest_bytes (Buffer.contents_bytes buf) |> to_hex |> Bytes.of_string
    )
  | _ -> failwith "info does not match desired format hurhur"

(* helper to get the nth element as a string in info dict *)
let extract_nth_string info_dict n =
  List.nth_exn info_dict n |> snd |> Bencode.as_string_exn

(* helper to get the nth element as a int in info dict *)
let extract_nth_int info_dict n =
  List.nth_exn info_dict n |> snd |> Bencode.as_int_exn

(* returns array of SHA1 hashes for each of the pieces in the file *)
let piece_hashes info_dict =
  let hash_len = 20 in
  let p_bytes =
    extract_nth_string info_dict 3 |> Bytes.of_string in
  if Bytes.length p_bytes % hash_len <> 0 then
    failwith "pieces hashes not correct length"
  else
    let num_hashes = Bytes.length p_bytes / hash_len in
    Array.init num_hashes ~f:(fun i ->
        Bytes.sub p_bytes ~pos:(hash_len*i) ~len:hash_len)

(* create type t from filepath. will raise various exceptions if the format is
 * not right, as of now this is pretty strict *)
let create file =
  let open Bencode in
  let bencode_dict = as_dict_exn (decode file) in
  let announce = fst (List.hd_exn bencode_dict) in
  let info_dict =
    match List.find bencode_dict ~f:(fun (x,_) -> String.equal x "info") with
    | Some (_, dict_t) -> as_dict_exn dict_t
    | None -> failwith "did not find info field in torrent file"
  in
  let info_hash = info_hash info_dict in
  let piece_hashes = piece_hashes info_dict in
  let piece_length = extract_nth_int info_dict 2 in
  let length = extract_nth_int info_dict 0 in
  let name = extract_nth_string info_dict 1 in
  { announce
  ; info_hash
  ; piece_hashes
  ; piece_length
  ; length
  ; name
  }
