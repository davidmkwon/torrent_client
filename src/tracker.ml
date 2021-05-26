open Base

(* constructs url with proper query params to send to tracker *)
let build_tracker_url (tf: Torrentfile.t) (peer_id: Bytes.t) port =
  let query_params = [
    ("info_hash", Bytes.to_string tf.info_hash);
    ("peer_id", Bytes.to_string peer_id);
    ("port", Int.to_string port);
    ("uploaded", "0");
    ("downloaded", "0");
    ("compact", "1");
    ("left", Int64.to_string tf.length);
  ] in
  let uri = Uri.of_string tf.announce in
  Uri.add_query_params' uri query_params |> Uri.to_string
