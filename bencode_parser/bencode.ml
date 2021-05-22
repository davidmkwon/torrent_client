open Base
open Stdio

include Types

let decode file =
  let in_ch = In_channel.create file in
  let t = Parser.prog Lexer.read (Lexing.from_channel in_ch) in
  In_channel.close in_ch;
  match t with
  | None -> failwith "bad torrent file hurhur"
  | Some t -> t

let empty_string ~len = String.make len ' '

let spaces level = empty_string ~len:(level * 2)

let format_list l ~f =
  let buf = Buffer.create 10 in
  Buffer.add_string buf "[\n";
  l |> List.iter ~f:(fun e ->
      f buf e;
    );
  Buffer.add_char buf ']';
  Buffer.contents buf

let pretty_print =
  let rec loop level = function
    | Int x -> Int64.to_string x
    | String x -> Printf.sprintf "<string:%d>" (String.length x)
    | List l ->
      format_list l ~f:(fun buf e ->
          Buffer.add_string buf (spaces level);
          Buffer.add_string buf (loop (Int.succ level) e);
          Buffer.add_string buf ";\n"
        )
    | Dict t ->
      let format_tuple s t =
        Printf.sprintf "(\"%s\", %s)" s (loop (Int.succ level) t) in
      format_list t ~f:(fun buf (s, t) ->
          Buffer.add_string buf (spaces level);
          Buffer.add_string buf (format_tuple s t);
          Buffer.add_string buf ";\n")
  in loop 1

let to_original_string =
  let _format_rec l begin_char ~f =
    let buf = Buffer.create 10 in
    Buffer.add_char buf begin_char;
    l |> List.iter ~f:(fun e ->
        f buf e;
      );
    Buffer.add_char buf 'e';
    Buffer.contents buf
  in
  let format_string s = Printf.sprintf "%d:%s" (String.length s) s
  in
  let rec loop = function
    | Int i -> Printf.sprintf "i%Lde" i
    | String s -> format_string s
    | List l ->
      _format_rec l 'l' ~f:(fun buf e ->
          Buffer.add_string buf (loop e);
        )
    | Dict t ->
      let format_tuple s t = Printf.sprintf "%s%s" (format_string s) (loop t) in
      _format_rec t 'd' ~f:(fun buf (s, t) ->
          Buffer.add_string buf (format_tuple s t);
        )
  in loop

let as_int_exn t =
  match t with
  | Int i -> i
  | _ -> failwith ("bencode type\n" ^ (pretty_print t) ^ "is not a dictionary")

let as_string_exn t =
  match t with
  | String s -> s
  | _ -> failwith ("bencode type\n" ^ (pretty_print t) ^ "is not a dictionary")

let as_list_exn t =
  match t with
  | List l -> l
  | _ -> failwith ("bencode type\n" ^ (pretty_print t) ^ "is not a dictionary")

let as_dict_exn t =
  match t with
  | Dict d -> d
  | _ -> failwith ("bencode type\n" ^ (pretty_print t) ^ "is not a dictionary")
