(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Ser
open Hashaux
open Hash
open Sha256
open Json
open Db

let ltc_oldest_to_consider = hexstring_hashval "de03ff6e66697188fb628c81c137fb502cc1085522258252c0ef9171ae168b59"
let ltc_oldest_to_consider_time = 1506536388L
let ltc_oldest_to_consider_height = 202390L

let ltc_bestblock = ref (0l,0l,0l,0l,0l,0l,0l,0l)

let burntx : (hashval,string) Hashtbl.t = Hashtbl.create 100

type poburn =
  | Poburn of md256 * md256 * int64 (** ltc block hash id, ltc tx hash id, number of litoshis burned **)

let hashpoburn p =
  match p with
  | Poburn(h,k,x) -> hashtag (hashpair (hashpair h k) (hashint64 x)) 194l

let seo_poburn o p c =
  match p with
  | Poburn(h,k,x) ->
      let c = seo_md256 o h c in
      let c = seo_md256 o k c in
      let c = seo_int64 o x c in
      c

let sei_poburn i c =
  let (h,c) = sei_md256 i c in
  let (k,c) = sei_md256 i c in
  let (x,c) = sei_int64 i c in
  (Poburn(h,k,x),c)

type ltcdacstatus = LtcDacStatusPrev of hashval | LtcDacStatusNew of (hashval * hashval * hashval * int64 * int64) list list

let seo_ltcdacstatus o s c =
  match s with
  | LtcDacStatusPrev(h) ->
      let c = o 1 0 c in
      seo_hashval o h c
  | LtcDacStatusNew(l) ->
      let c = o 1 1 c in
      seo_list (seo_list (seo_prod5 seo_hashval seo_hashval seo_hashval seo_int64 seo_int64)) o l c

let sei_ltcdacstatus i c =
  let (x,c) = i 1 c in
  if x = 0 then
    let (h,c) = sei_hashval i c in
    (LtcDacStatusPrev(h),c)
  else
    let (l,c) = sei_list (sei_list (sei_prod5 sei_hashval sei_hashval sei_hashval sei_int64 sei_int64)) i c in
    (LtcDacStatusNew(l),c)

module DbLtcDacStatus = Dbbasic2 (struct type t = ltcdacstatus let basedir = "ltcdacstatus" let seival = sei_ltcdacstatus seic let seoval = seo_ltcdacstatus seoc end)

let rec ltcdacstatus_dbget h =
  try
    let z = DbLtcDacStatus.dbget h in
    match z with
    | LtcDacStatusPrev(k) ->
	ltcdacstatus_dbget k
    | LtcDacStatusNew(l) -> (h,l)
  with Not_found -> (ltc_oldest_to_consider,[])

let json_assoc_string k al =
  match List.assoc k al with
  | JsonStr(x) -> x
  | _ -> raise Not_found

let json_assoc_int64 k al =
  match List.assoc k al with
  | JsonNum(x) -> Int64.of_string x
  | _ -> raise Not_found

let json_assoc_int k al =
  match List.assoc k al with
  | JsonNum(x) -> int_of_string x
  | _ -> raise Not_found

let litecoins_of_litoshis v =
  let w = Int64.div v 100000000L in
  let d = Int64.to_string (Int64.rem v 100000000L) in
  let dl = String.length d in
  let ez = ref 0 in
  begin
    try
      for i = dl-1 downto 0 do
	if d.[i] = '0' then
	  incr ez
	else
	  raise Exit
      done
    with Exit -> ()
  end;
  let b = Buffer.create 20 in
  Buffer.add_string b (Int64.to_string w);
  Buffer.add_char b '.';
  for i = 1 to 11 - dl do
    Buffer.add_char b '0'
  done;
  for i = 0 to dl - (1 + !ez) do
    Buffer.add_char b d.[i]
  done;
  Buffer.contents b

let litoshis_of_litecoins s =
  let f = ref 0L in
  let w = ref true in
  let c = ref 0L in
  let d = ref 10000000L in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let cc = Char.code s.[!i] in
    incr i;
    if !w then
      if cc = 46 then
	w := false
      else if cc >= 48 && cc < 58 then
	f := Int64.add (Int64.mul !f 10L) (Int64.of_int (cc-48))
      else
	raise (Failure ("cannot interpret " ^ s ^ " as a number of litecoins"))
    else
      if cc >= 48 && cc < 58 then
	begin
	  c := Int64.add !c (Int64.mul !d (Int64.of_int (cc-48)));
	  d := Int64.div !d 10L
	end
      else
	raise (Failure ("cannot interpret " ^ s ^ " as a number of litecoins"))
  done;
  Int64.add (Int64.mul !f 100000000L) !c

let json_assoc_litoshis k al =
  match List.assoc k al with
  | JsonNum(x) -> litoshis_of_litecoins x
  | _ -> raise Not_found

let ltc_getbestblockhash () =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"gbbh\", \"method\": \"getbestblockhash\", \"params\": [] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) -> json_assoc_string "result" al
    | _ ->
	Printf.fprintf !Utils.log "problem return from ltc getbestblockhash:\n%s\n" l;
	raise Not_found
  with _ ->
    raise Not_found

let dalilcoin_candidate_p h =
  String.length h = 64 && h.[0] = '4' && h.[1] = '4' && h.[2] = '6' && h.[3] = '1'

let ltc_getblock h =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"gb\", \"method\": \"getblock\", \"params\": [\"" ^ h ^ "\"] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin
	  match List.assoc "result" al with
	  | JsonObj(bl) ->
	      begin
		let pbh = json_assoc_string "previousblockhash" bl in
		let tm = json_assoc_int64 "mediantime" bl in
		let hght = json_assoc_int64 "height" bl in
		let txl = ref [] in
		match List.assoc "tx" bl with
		| JsonArr(txs) ->
		    begin
		      List.iter
			(fun jtxh ->
			  match jtxh with
			  | JsonStr(txh) when dalilcoin_candidate_p txh -> txl := txh::!txl
			  | _ -> ())
			txs;
		      (pbh,tm,hght,!txl)
		    end
		| _ ->
		    Printf.fprintf !Utils.log "problem return from ltc getblock:\n%s\n" l;
		    raise Not_found
	      end
	  | _ ->
	      Printf.fprintf !Utils.log "problem return from ltc getblock:\n%s\n" l;
	      raise Not_found
	end
    | _ ->
	Printf.fprintf !Utils.log "problem return from ltc getblock:\n%s\n" l;
	raise Not_found
  with _ ->
    raise Not_found

let ltc_listunspent () =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let addrl = Buffer.create 40 in
    let fstaddr = ref true in
    List.iter
      (fun a ->
	if !fstaddr then fstaddr := false else Buffer.add_char addrl ',';
	Buffer.add_char addrl '"';
	Buffer.add_string addrl a;
	Buffer.add_char addrl '"')
      !Config.ltcaddresses;
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"lu\", \"method\": \"listunspent\", \"params\": [1,9999999,[" ^ (Buffer.contents addrl) ^ "]] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    let utxol = ref [] in
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin
	  match List.assoc "result" al with
	  | JsonArr(ul) ->
	      begin
		List.iter
		  (fun u ->
		    match u with
		    | JsonObj(bl) ->
			begin
			  try
			    let txh = json_assoc_string "txid" bl in
			    let vout = json_assoc_int "vout" bl in
			    let rs = json_assoc_string "redeemScript" bl in
			    let spk = json_assoc_string "scriptPubKey" bl in
			    let amt = json_assoc_litoshis "amount" bl in
			    utxol := (txh,vout,rs,spk,amt)::!utxol
			  with Not_found ->
			    ()
			end
		    | _ -> ())
		  ul;
		!utxol
	      end
	  | _ ->
	      Printf.fprintf !Utils.log "problem return from ltc listunspent:\n%s\n" l;
	      raise Not_found
	end
    | _ ->
	Printf.fprintf !Utils.log "problem return from ltc listunspent:\n%s\n" l;
	raise Not_found
  with _ ->
    raise Not_found

exception InsufficientLtcFunds

let le_num24 x =
  let strb = Buffer.create 3 in
  Buffer.add_char strb (Char.chr (x land 255));
  Buffer.add_char strb (Char.chr ((x lsr 8) land 255));
  Buffer.add_char strb (Char.chr ((x lsr 16) land 255));
  Buffer.contents strb

let finddatx txs1 txs2 =
  let i = ref (-1) in
  let rtxid = ref (0l,0l,0l,0l,0l,0l,0l,0l) in
  let txs = ref "" in
  let daid ri =
    let (_,_,_,_,_,_,_,x) = ri in
    Int32.logand x 0xffffl = 0x6144l
  in
  while not (daid !rtxid) do
    incr i;
    if !i >= 16777216 then raise Not_found; (** probably will never happen **)
    txs := txs1 ^ (le_num24 !i) ^ txs2;
    rtxid := Sha256.sha256dstr !txs
  done;
  (!i,!rtxid,!txs);;

let blnum32 x =
  [Int32.to_int (Int32.logand x 255l);
   Int32.to_int (Int32.logand (Int32.shift_right_logical x 8) 255l);
   Int32.to_int (Int32.logand (Int32.shift_right_logical x 16) 255l);
   Int32.to_int (Int32.logand (Int32.shift_right_logical x 24) 255l)]

let blnum64 x =
  [Int64.to_int (Int64.logand x 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 8) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 16) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 24) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 32) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 40) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 48) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 56) 255L)]

let ltc_createburntx h1 h2 toburn =
  let utxol = ltc_listunspent () in
  let toburn_plus_fee = Int64.add toburn !Config.ltctxfee in
  try
    Hashtbl.find burntx h2
  with Not_found ->
    try
      let (txid,vout,rs,spk,amt) = List.find (fun (_,_,_,_,amt) -> amt >= toburn_plus_fee) utxol in (*** only consider single spends ***)
      let txs1b = Buffer.create 100 in
      let txs2b = Buffer.create 100 in
      let txs3b = Buffer.create 100 in
      Buffer.add_string txs1b "\001"; (*** assume one input ***)
      let txidrh = hashval_rev (hexstring_hashval txid) in
      ignore (seo_hashval seosb txidrh (txs1b,None));
      List.iter (fun z -> Buffer.add_char txs1b (Char.chr z)) (blnum32 (Int32.of_int vout));
      let txs1 = Buffer.contents txs1b in
      Buffer.add_char txs1b '\023';
      Buffer.add_char txs1b '\022';
      Buffer.add_string txs1b (hexstring_string rs);
      Buffer.add_string txs2b "\255\255\255\255\002";
      List.iter (fun z -> Buffer.add_char txs2b (Char.chr z)) (blnum64 toburn);
      Buffer.add_char txs2b (Char.chr 69);
      Buffer.add_char txs2b (Char.chr 0x6a); (*** OP_RETURN ***)
      Buffer.add_char txs2b (Char.chr 67); (*** PUSH 67 ***)
      ignore (seo_hashval seosb h1 (txs2b,None));
      ignore (seo_hashval seosb h2 (txs2b,None));
      List.iter (fun z -> Buffer.add_char txs3b (Char.chr z)) (blnum64 (Int64.sub amt toburn_plus_fee));
      let spks = hexstring_string spk in
      Buffer.add_char txs3b (Char.chr (String.length spks));
      Buffer.add_string txs3b spks;
      Buffer.add_string txs3b "\000\000\000\000"; (*** locktime ***)
      let txs2 = Buffer.contents txs2b in
      let txs3 = Buffer.contents txs3b in
      let (i,rtxid,txs) = finddatx ("\002\000\000\000" ^ (Buffer.contents txs1b) ^ txs2) txs3 in
      let txsb = Buffer.create 100 in
      Buffer.add_string txsb "\002\000\000\000";
      Buffer.add_string txsb txs1;
      Buffer.add_string txsb "\000";
      Buffer.add_string txsb txs2;
      Buffer.add_string txsb (le_num24 i);
      Buffer.add_string txsb txs3;
      let s = Buffer.contents txsb in
      Hashtbl.add burntx h2 s;
      s
    with Not_found -> raise InsufficientLtcFunds

let ltc_signrawtransaction txs =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"srtx\", \"method\": \"signrawtransaction\", \"params\": [\"" ^ txs ^ "\"] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin 
	  match List.assoc "result" al with
	  | JsonObj(bl) -> json_assoc_string "hex" bl
	  | _ ->
	      Printf.fprintf !Utils.log "problem return from ltc signrawtransaction:\n%s\n" l;
	      raise Not_found
	end
    | _ ->
	Printf.fprintf !Utils.log "problem return from ltc signrawtransaction:\n%s\n" l;
	raise Not_found
  with _ -> raise Not_found

let ltc_sendrawtransaction txs =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"srtx\", \"method\": \"sendrawtransaction\", \"params\": [\"" ^ txs ^ "\"] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) -> json_assoc_string "result" al
    | _ ->
	Printf.fprintf !Utils.log "problem return from ltc sendrawtransaction:\n%s\n" l;
	raise Not_found
  with _ -> raise Not_found

let ltc_gettransactioninfo h =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"grtx\", \"method\": \"getrawtransaction\", \"params\": [\"" ^ h ^ "\",1] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin
	  match List.assoc "result" al with
	  | JsonObj(bl) ->
	      begin
		match List.assoc "vout" bl with
		| JsonArr(JsonObj(vout1)::_) ->
		    let litoshisburned = json_assoc_litoshis "value" vout1 in
		    begin
		      match List.assoc "scriptPubKey" vout1 with
		      | JsonObj(cl) ->
			  let hex = json_assoc_string "hex" cl in
			  if String.length hex >= 132 && hex.[0] = '6' && hex.[1] = 'a' then
			    (litoshisburned,hexstring_hashval (String.sub hex 4 64),hexstring_hashval (String.sub hex 68 64))
			  else
			    begin
			      Printf.fprintf !Utils.log "problem return from ltc getrawtransaction:\n%s\n" l;
			      raise Not_found
			    end
		      | _ ->
			  Printf.fprintf !Utils.log "problem return from ltc getrawtransaction:\n%s\n" l;
			  raise Not_found
		    end
		| _ ->
		    Printf.fprintf !Utils.log "problem return from ltc getrawtransaction:\n%s\n" l;
		    raise Not_found
	      end
	  | _ ->
	      Printf.fprintf !Utils.log "problem return from ltc getrawtransaction:\n%s\n" l;
	      raise Not_found
	end
    | _ ->
	Printf.fprintf !Utils.log "problem return from ltc getrawtransaction:\n%s\n" l;
	raise Not_found
  with _ -> raise Not_found

module DbHeaderLtcBurn = Dbbasic2 (struct type t = poburn * hashval option * int64 let basedir = "headerltcburn" let seival = sei_prod3 sei_poburn (sei_option sei_hashval) sei_int64 seic let seoval = seo_prod3 seo_poburn (seo_option seo_hashval) seo_int64 seoc end)
module DbLtcBurnTx = Dbbasic2 (struct type t = int64 * hashval * hashval let basedir = "ltcburntx" let seival = sei_prod3 sei_int64 sei_hashval sei_hashval seic let seoval = seo_prod3 seo_int64 seo_hashval seo_hashval seoc end)

module DbLtcBlock = Dbbasic2 (struct type t = hashval * int64 * int64 * hashval list let basedir = "ltcblock" let seival = sei_prod4 sei_hashval sei_int64 sei_int64 (sei_list sei_hashval) seic let seoval = seo_prod4 seo_hashval seo_int64 seo_int64 (seo_list seo_hashval) seoc end)

let rec ltc_process_block h =
  let hh = hexstring_hashval h in
  if not (hh = ltc_oldest_to_consider) && not (DbLtcBlock.dbexists hh) then
    begin
      let (prev,tm,hght,txhs) = ltc_getblock h in
      if not (txhs = []) then
	begin
	  Printf.fprintf !Utils.log "getblock %s had %d candidate txs:\n" h (List.length txhs);
	  List.iter (fun txh -> Printf.fprintf !Utils.log "candidate %s\n" txh) txhs;
	end;
      ltc_process_block prev;
      let prevh = hexstring_hashval prev in
      let genl = ref [] in
      let succl = ref [] in
      let txhhs = ref [] in
      List.iter
	  (fun txh ->
	    let txhh = hexstring_hashval txh in
	    if not (DbLtcBurnTx.dbexists txhh) then
	      begin
		try
		  let (burned,dprev,dnxt) = ltc_gettransactioninfo txh in
		  if DbHeaderLtcBurn.dbexists dnxt then
		    Printf.fprintf !Utils.log "Ignoring burn %s for header %s, since a previous burn was already done for this header\n" txh (hashval_hexstring dnxt)
		  else if dprev = (0l,0l,0l,0l,0l,0l,0l,0l) then
		    begin
		      Printf.fprintf !Utils.log "Adding burn %s for genesis header %s\n" txh (hashval_hexstring dnxt);
		      DbHeaderLtcBurn.dbput dnxt (Poburn(hh,txhh,burned),None,1L);
		      DbLtcBurnTx.dbput txhh (burned,dprev,dnxt);
		      txhhs := txhh :: !txhhs;
		      genl := (txhh,burned,dnxt)::!genl
		    end
		  else
		    begin
		      Printf.fprintf !Utils.log "Adding burn %s for header %s\n" txh (hashval_hexstring dnxt);
		      let (_,_,pblkh) = DbHeaderLtcBurn.dbget dprev in
		      Printf.fprintf !Utils.log "New height %Ld\n" (Int64.add pblkh 1L);
		      DbHeaderLtcBurn.dbput dnxt (Poburn(hh,txhh,burned),Some(dprev),Int64.add pblkh 1L);
		      DbLtcBurnTx.dbput txhh (burned,dprev,dnxt);
		      txhhs := txhh :: !txhhs;
		      succl := (dprev,txhh,burned,dnxt)::!succl
		    end
		with Not_found ->
		  Printf.fprintf !Utils.log "Ignoring tx %s which does not appear to be a Dalilcoin burn tx\n" txh
	      end
	    else
	      txhhs := txhh :: !txhhs)
	txhs;
      begin
	let (prevkey,pbds) = ltcdacstatus_dbget prevh in
	let change = ref false in
	let bds = ref [] in
	if not (!genl = []) then
	  begin
	    if tm > Int64.add !Config.genesistimestamp 604800L then
	      begin
		Printf.fprintf !Utils.log "Ignoring unexpected genesis blocks burned during what appears to be after the genesis phase:\n";
		List.iter (fun (txhh,burned,dnxt) -> Printf.printf "%s %Ld %s\n" (hashval_hexstring txhh) burned (hashval_hexstring dnxt)) !genl
	      end
	    else (*** there has already been a genesis block created during the genesis phase, but a competing one (or more) was created; include it too ***)
	      begin
		Printf.fprintf !Utils.log "%d genesis block(s) found.\n" (List.length !genl);
		let pbdl = List.map (fun (txhh,burned,dnxt) -> (dnxt,hh,txhh,tm,hght)) !genl in
		change := true;
		bds := [pbdl]
	      end
	  end;
	List.iter
	  (fun pbdl ->
	    let pbdl2 =
	      List.filter
		(fun (bh,lbh,ltx,ltm,lhght) -> if Int64.sub tm ltm <= 604800L && Int64.sub hght lhght <= 4032L then true else (change := true; false))
		pbdl
	    in
	    if not (pbdl2 = []) then bds := pbdl2 :: !bds;
	    let pbdl3 = ref [] in
	    List.iter
	      (fun (bh,lbh,ltx,ltm,lhght) ->
		try
		  let (dprev,txhh,burned,dnxt) = List.find (fun (dprev,_,_,_) -> bh = dprev) !succl in
		  pbdl3 := (dnxt,hh,txhh,tm,hght)::!pbdl3;
		  change := true
		with Not_found -> ())
	      pbdl2;
	    if not (!pbdl3 = []) then bds := !pbdl3 :: !bds)
	  (List.rev pbds);
	if !change then
	  begin
	    Printf.fprintf !Utils.log "bds %d\n" (List.length !bds);
	    DbLtcDacStatus.dbput hh (LtcDacStatusNew(!bds))
	  end
	else if not (prevkey = ltc_oldest_to_consider) then
	  DbLtcDacStatus.dbput hh (LtcDacStatusPrev(prevkey)) (*** pointer to last ltc block where dalilcoin status changed ***)
      end;
      DbLtcBlock.dbput hh (prevh,tm,hght,!txhhs)
    end

let ltc_medtime () =
  try
    let (_,mtm,_,_) = DbLtcBlock.dbget !ltc_bestblock in
    mtm
  with Not_found -> Int64.of_float (Unix.time())

let ltc_synced () =
  try
    Printf.fprintf !Utils.log "Checking if ltc synced ; bestblock %s\n" (hashval_hexstring !ltc_bestblock); flush !Utils.log;
    let (_,tm,_,_) = DbLtcBlock.dbget !ltc_bestblock in
    Printf.fprintf !Utils.log "tm of ltc bestblock %Ld offset from now %f\n" tm (Unix.time() -. Int64.to_float tm); flush !Utils.log;
    if Unix.time() -. Int64.to_float tm < 1200.0 then
      true
    else
      false
  with Not_found -> false
