(*open Base*)
open Stdio

include Types

let format_list l ~f =
  let buf = Buffer.create 10 in
  Buffer.add_string buf "[\n";
  l |> List.iter (fun e ->
      f buf e;
    );
  Buffer.add_char buf ']';
  Buffer.contents buf

let decode file =
  let in_ch = In_channel.create file in
  let t = Parser.prog Lexer.read (Lexing.from_channel in_ch) in
  In_channel.close in_ch;
  t

let empty_string ~len = String.make len ' '

let spaces level = empty_string ~len:(level * 2)

let pretty_print =
  let rec loop level = function
    | Int x -> Int64.to_string x
    | String x -> Printf.sprintf "<string:%d>" (String.length x)
    | List l ->
      format_list l ~f:(fun buf e ->
          Buffer.add_string buf (spaces level);
          Buffer.add_string buf (loop (succ level) e);
          Buffer.add_string buf ";\n"
        )
    | Dict t ->
      let format_tuple s t =
        Printf.sprintf "(\"%s\", %s)" s (loop (succ level) t) in
      format_list t ~f:(fun buf (s, t) ->
          Buffer.add_string buf (spaces level);
          Buffer.add_string buf (format_tuple s t);
          Buffer.add_string buf ";\n")
  in loop 1

let as_int = function
  | Int i -> Some i
  | _ -> None

let as_string = function
  | String s -> Some s
  | _ -> None

let as_list = function
  | List l -> Some l
  | _ -> None

let as_dict = function
  | Dict d -> Some d
  | _ -> None
