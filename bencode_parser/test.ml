open Stdio
let path = "../test_files/test.torrent"
let bt = Bencode.decode path
let file = Out_channel.create "../test_files/test2.torrent"
let () = Out_channel.output_string file (Bencode.to_original_string bt)
let () = Out_channel.close file
