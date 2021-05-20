let path = "test.torrent"
let bt = Option.get (Bencode.decode path)
let () = Printf.printf "%s\n" (Bencode.pretty_print bt)
