open Base

type t =
  { ip : Ipaddr.t
  ; port: int
  }

(* deserializes the blob of bytes representing the addresses of the peers *)
let deserialize peers_bytes =
  let peer_size = 6 in
  let ip_size = 4 in
  let port_size = 2 in
  if Bytes.length peers_bytes % peer_size <> 0 then
    failwith "received peer bytes is inproper"
  else ();
  let num_peers = Bytes.length peers_bytes / peer_size in
  Array.init num_peers ~f:(fun i ->
      let pos = i * peer_size in
      let ip =
        Bytes.sub peers_bytes ~pos ~len:ip_size
        |> Bytes.to_string
        |> Ipaddr.of_string_exn in
      let port =
        Bytes.sub peers_bytes ~pos:(pos + ip_size) ~len:port_size
        |> Bytes.to_string
        |> Int.of_string in
      { ip; port })
