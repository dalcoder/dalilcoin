(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017-2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

(* Most of this code is taken directly from Egal. *)

open Json
open Secp256k1
open Hash

val base58 : Z.t -> string
val frombase58 : string -> Z.t
val dalilwif : Z.t -> bool -> string
val ltcwif : Z.t -> bool -> string
val privkey_from_wif : string -> Z.t * bool
val privkey_from_btcwif : string -> Z.t * bool
val pubkey_hexstring : (Z.t * Z.t) -> bool -> string
val hexstring_pubkey : string -> (Z.t * Z.t) * bool
val pubkey_hashval : Z.t * Z.t -> bool -> hashval
val pubkey_md160 : Z.t * Z.t -> bool -> md160
val md160_from_addrstr : string -> md160
val md160_btcaddrstr : md160 -> string
val addr_daliladdrstr : addr -> string
val daliladdrstr_addr : string -> addr
val btcaddrstr_addr : string -> addr
val fraenks_of_cants : int64 -> string
val cants_of_fraenks : string -> int64
val ltc_of_litoshis : int64 -> string
val litoshis_of_ltc : string -> int64

val addr_from_json : jsonval -> addr
val payaddr_from_json : jsonval -> payaddr
val cants_from_json : jsonval -> int64
val fraenks_from_json : jsonval -> string
val json_fraenks : string -> jsonval
val json_cants : int64 -> jsonval

