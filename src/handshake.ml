type t =
  { pstr : string
  ; info_hash : Bytes.t
  ; peer_id : Bytes.t
  }

let serialize t = t
