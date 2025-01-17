(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017-2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Ser
open Sha256
open Ripemd160
open Hash
open Net
open Db
open Zarithint
open Logic
open Mathdata
open Assets
open Signat
open Cryptocurr
open Tx
open Ctre
open Ctregraft

type poburn =
  | Poburn of md256 * md256 * int64 * int64 (** ltc block hash id, ltc tx hash id, ltc block median time, number of litoshis burned **)

let hashpoburn p =
  match p with
  | Poburn(h,k,x,y) -> hashtag (hashpair (hashpair h k) (hashpair (hashint64 x) (hashint64 y))) 194l

let seo_poburn o p c =
  match p with
  | Poburn(h,k,x,y) ->
      let c = seo_md256 o h c in
      let c = seo_md256 o k c in
      let c = seo_int64 o x c in
      let c = seo_int64 o y c in
      c

let sei_poburn i c =
  let (h,c) = sei_md256 i c in
  let (k,c) = sei_md256 i c in
  let (x,c) = sei_int64 i c in
  let (y,c) = sei_int64 i c in
  (Poburn(h,k,x,y),c)

(*** 256 bits ***)
type stakemod = hashval

(*** The genesis stakemod should be the 32 bytes (64 hex chars) of the block hash for a particular litecoin block with height preannounced.
 ***)
let genesisstakemod : stakemod ref = ref (hexstring_hashval "0000000000000000000000000000000000000000000000000000000000000000")

(*** max target/min difficulty: 2^200 (for mainnet) ***)
let max_target = ref (shift_left_big_int unit_big_int 200)
let genesistarget = ref (shift_left_big_int unit_big_int 196) (* current estimate for initial difficulty *)
let genesisledgerroot : hashval ref = ref (hexstring_hashval "d4b10e4b72eaa8a61427b805f206e828b22bb59192373b83fe0df501e68a5bed");;

(*** base reward of 50 fraenks (5 trillion cants) like bitcoin, but assume the first 350000 blocks have passed. ***)
let basereward = 5000000000000L

(*** the block reward begins at 25 fraenks and halves with each era until era 43 when it is 0 ***)
let rewfn blkh = Int64.shift_right basereward (Utils.era blkh)

let seo_stakemod o sm c = seo_hashval o sm c

let sei_stakemod i c = sei_hashval i c

(*** one round of sha256 combining the timestamp (least significant 32 bits only), the hash value of the stake's assetid and the stake modifier, then converted to a big_int to do arithmetic ***)
let hitval tm h sm =
  let d = hashpair (hashtag sm (Int64.to_int32 tm)) h in
  md256_big_int d

(*** target (big_int, but assumed to be at most 256 bits ***)
type targetinfo = Z.t

let targetinfo_string tar = string_of_big_int tar

let eq_tinfo z w = eq_big_int z w

let hashtargetinfo tar = big_int_hashval tar

let seo_targetinfo o ti c =
  seo_big_int_256 o ti c

let sei_targetinfo i c =
  sei_big_int_256 i c

let poburn_stakemod p =
  match p with
  | Poburn(h,k,x,y) -> hashpair h k

let verbose_blockcheck = ref None

(*** optional messages sent to an out_channel while checking validity of blocks ***)
let vbc f =
  match !verbose_blockcheck with
  | Some(c) -> f c; flush c
  | None -> ()

let vbcv v f = vbc f; v

let vbcb v f g = if v then vbc f else vbc g; v

type blockheaderdata = {
    prevblockhash : (hashval * poburn) option;
    newtheoryroot : hashval option;
    newsignaroot : hashval option;
    newledgerroot : hashval;
    stakeaddr : p2pkhaddr;
    stakeassetid : hashval;
    timestamp : int64;
    deltatime : int32;
    tinfo : targetinfo;
    prevledger : ctree;
    blockdeltaroot : hashval;
  }

type blockheadersig = {
    blocksignat : signat;
    blocksignatrecid : int;
    blocksignatfcomp : bool;
    blocksignatendorsement : (p2pkhaddr * int * bool * signat) option;
  }

type blockheader = blockheaderdata * blockheadersig

(*** a fake blockheader to use when some data structure needs to be initialized ***)
let fake_blockheader : blockheader =
  ({ prevblockhash = None;
     newtheoryroot = None;
     newsignaroot = None;
     newledgerroot = (0l,0l,0l,0l,0l,0l,0l,0l);
     stakeaddr = (0l,0l,0l,0l,0l);
     stakeassetid = (0l,0l,0l,0l,0l,0l,0l,0l);
     timestamp = 0L;
     deltatime = 0l;
     tinfo = zero_big_int;
     prevledger = CHash(0l,0l,0l,0l,0l,0l,0l,0l);
     blockdeltaroot = (0l,0l,0l,0l,0l,0l,0l,0l);
   },
   { blocksignat = (zero_big_int,zero_big_int);
     blocksignatrecid = 0;
     blocksignatfcomp = false;
     blocksignatendorsement = None;
   })

let seo_blockheaderdata o bh c =
  let c = seo_option (seo_prod seo_hashval seo_poburn) o bh.prevblockhash c in
  let c = seo_option seo_hashval o bh.newtheoryroot c in
  let c = seo_option seo_hashval o bh.newsignaroot c in
  let c = seo_hashval o bh.newledgerroot c in
  let c = seo_md160 o bh.stakeaddr c in (*** p2pkh addresses are md160 ***)
  let c = seo_hashval o bh.stakeassetid c in
  let c = seo_int64 o bh.timestamp c in
  let c = seo_int32 o bh.deltatime c in
  let c = seo_targetinfo o bh.tinfo c in
  let c = seo_ctree o bh.prevledger c in
  let c = seo_hashval o bh.blockdeltaroot c in
  c

let sei_blockheaderdata i c =
  let (x0,c) = sei_option (sei_prod sei_hashval sei_poburn) i c in
  let (x1,c) = sei_option sei_hashval i c in
  let (x2,c) = sei_option sei_hashval i c in
  let (x3,c) = sei_hashval i c in
  let (x4,c) = sei_md160 i c in (*** p2pkh addresses are md160 ***)
  let (x5,c) = sei_hashval i c in
  let (x7,c) = sei_int64 i c in
  let (x8,c) = sei_int32 i c in
  let (x9,c) = sei_targetinfo i c in
  let (x10,c) = sei_ctree i c in
  let (x11,c) = sei_hashval i c in
  let bhd : blockheaderdata =
      { prevblockhash = x0;
	newtheoryroot = x1;
	newsignaroot = x2;
	newledgerroot = x3;
	stakeaddr = x4;
	stakeassetid = x5;
	timestamp = x7;
	deltatime = x8;
	tinfo = x9;
	prevledger = x10;
	blockdeltaroot = x11;
      }
  in
  (bhd,c)

let seo_blockheadersig o bhs c = 
  let c = seo_signat o bhs.blocksignat c in
  let c = o 2 bhs.blocksignatrecid c in
  let c = seo_bool o bhs.blocksignatfcomp c in
  let c = seo_option (seo_prod4 seo_md160 seo_varintb seo_bool seo_signat) o bhs.blocksignatendorsement c in
  c

let sei_blockheadersig i c = 
  let (x,c) = sei_signat i c in
  let (r,c) = i 2 c in
  let (f,c) = sei_bool i c in
  let (e,c) = sei_option (sei_prod4 sei_md160 sei_varintb sei_bool sei_signat) i c in
  let bhs : blockheadersig =
    { blocksignat = x;
      blocksignatrecid = r;
      blocksignatfcomp = f;
      blocksignatendorsement = e;
    }
  in
  (bhs,c)

let seo_blockheader o bh c = seo_prod seo_blockheaderdata seo_blockheadersig o bh c
let sei_blockheader i c = sei_prod sei_blockheaderdata sei_blockheadersig i c

type blockdelta = {
    stakeoutput : addr_preasset list;
    prevledgergraft : cgraft;
    blockdelta_stxl : stx list
  }

type block = blockheader * blockdelta

let seo_blockdelta o bd c =
  let c = seo_list seo_addr_preasset o bd.stakeoutput c in
  let c = seo_cgraft o bd.prevledgergraft c in
  let c = seo_list seo_stx o bd.blockdelta_stxl c in
  c

let sei_blockdelta i c =
  let (stko,c) = sei_list sei_addr_preasset i c in
  let (cg,c) = sei_cgraft i c in
  let (stxl,c) = sei_list sei_stx i c in
  ({ stakeoutput = stko;
     prevledgergraft = cg;
     blockdelta_stxl = stxl;
   },
   c)

let seo_block o b c = seo_prod seo_blockheader seo_blockdelta o b c
let sei_block i c = sei_prod sei_blockheader sei_blockdelta i c

module DbBlockHeader = Dbbasic2 (struct type t = blockheader let basedir = "blockheader" let seival = sei_blockheader seic let seoval = seo_blockheader seoc end)
module DbBlockDelta = Dbbasic2 (struct type t = blockdelta let basedir = "blockdelta" let seival = sei_blockdelta seic let seoval = seo_blockdelta seoc end)
module DbInvalidatedBlocks = Dbbasic2 (struct type t = bool let basedir = "invalidatedblocks" let seival = sei_bool seic let seoval = seo_bool seoc end)

let get_blockheaderdata h = 
  try
    let (d,s) = DbBlockHeader.dbget h in d
  with Not_found -> (*** request it and fail ***)
    find_and_send_requestdata GetHeader h;
    raise GettingRemoteData

let get_blockheadersig h = 
  try
    let (d,s) = DbBlockHeader.dbget h in
    s
  with Not_found -> (*** request it and fail ***)
    find_and_send_requestdata GetHeader h;
    raise GettingRemoteData

let get_blockheader h = 
  try
    DbBlockHeader.dbget h
  with Not_found -> (*** request it and fail ***)
    find_and_send_requestdata GetHeader h;
    raise GettingRemoteData

let get_blockdelta h = 
  try
    DbBlockDelta.dbget h
  with Not_found -> (*** request it and fail ***)
    if DbBlockHeader.dbexists h then
      find_and_send_requestdata GetBlockdelta h
    else
      find_and_send_requestdata GetHeader h;
    raise GettingRemoteData

(***
 hitval computes a big_int by hashing the timestamp (in seconds), the stake's asset id and the current stake modifier.
 If there is no proof of burn, then there's a hit if the hitval is less than the target times the stake.
 If there is proof of burn, the number of litecoin satoshis * 1000000 is added to the stake.
***)
let check_hit_b blkh bday obl v csm tar tmstmp stkid stkaddr burned =
  let v =
    Int64.add v (Int64.mul (min burned 100000000000L) 1000000L) (*** burning more than 1000 ltc still counts as "only" 1000 ***)
  in
  lt_big_int (hitval tmstmp stkid csm) (mult_big_int tar (coinage blkh bday obl v))

let check_hit blkh csm tinf bh bday obl v burned =
  check_hit_b blkh bday obl v csm tinf bh.timestamp bh.stakeassetid bh.stakeaddr burned

let coinstake b =
  let ((bhd,bhs),bd) = b in
  ([p2pkhaddr_addr bhd.stakeaddr,bhd.stakeassetid],bd.stakeoutput)

let hash_blockheaderdata bh =
  hashtag
    (hashopair2
       (match bh.prevblockhash with
       | None -> None
       | Some(h,pob) -> Some(hashpair h (hashpoburn pob)))
       (hashlist
	  [hashopair2 bh.newtheoryroot
	     (hashopair2 bh.newsignaroot bh.newledgerroot);
	   hashctree bh.prevledger;
	   bh.blockdeltaroot;
	   hashaddr (p2pkhaddr_addr bh.stakeaddr);
	   bh.stakeassetid;
	   hashtargetinfo bh.tinfo;
	   hashint64 bh.timestamp;
	   hashint32 bh.deltatime]))
    1028l

let hash_blockheadersig bhs =
  hashopair1
    (hashpair
       (hashsignat bhs.blocksignat)
       (hashtag
	  (hashint32 (Int32.of_int bhs.blocksignatrecid))
	  (if bhs.blocksignatfcomp then 1029l else 1030l)))
    (match bhs.blocksignatendorsement with
    | None -> None
    | Some(alpha,i,b,s) ->
	Some(hashtag
	       (hashlist [hashaddr (p2pkhaddr_addr alpha);hashint32 (Int32.of_int i); hashsignat s])
	       (if b then 1031l else 1032l)))

let hash_blockheader (bhd,bhs) =
  hashpair (hash_blockheaderdata bhd) (hash_blockheadersig bhs)

let blockheader_id bh = hash_blockheader bh

let valid_blockheader_allbutsignat blkh csm tinfo bhd (aid,bday,obl,u) lmedtm burned =
  if bhd.timestamp > lmedtm then
    vbcv false (fun c -> Printf.fprintf c "Block header has timestamp %Ld after median litecoin time %Ld\n" bhd.timestamp lmedtm)
  else if not (bhd.stakeassetid = aid) then
    vbcv false (fun c -> Printf.fprintf c "Block header asset id mismatch. Found %s. Expected %s.\n" (hashval_hexstring bhd.stakeassetid) (hashval_hexstring aid))
  else if blkh >= 730L && bday = 0L then (** additional constraint to avoid potential bugs when staking with airdrop assets whose value has halved at least once **)
    vbcv false (fun c -> Printf.fprintf c "Airdrop assets cannot be used for staking after height 730.\n")
  else
    match u with
    | Currency(v) ->
	begin
	  if not (check_hit blkh csm tinfo bhd bday obl v burned) then
	    vbcv false (fun c -> Printf.fprintf c "Block header not a hit.\nblkh = %Ld\ncsm = %s\ntinfo = %s\nbday = %Ld\nobl = %s\nv = %Ld\nburned = %Ld\n" blkh (hashval_hexstring csm) (targetinfo_string tinfo) bday (obligation_string obl) v burned)
	  else if not (bhd.deltatime > 0l) then
	    vbcv false (fun c -> Printf.fprintf c "Block header has a bad deltatime %ld\n" bhd.deltatime)
	  else
	    true
      end
  | _ ->
      vbcv false (fun c -> Printf.fprintf c "staked asset %s of block header was not a currency asset\n" (hashval_hexstring aid))

let valid_blockheader_signat (bhd,bhs) (aid,bday,obl,v) =
  let bhdh = hash_blockheaderdata bhd in (*** check that it signs the hash of the data part of the header, distinct form blockheader_id which combines hash of data with hash of sig ***)
  if (try DbInvalidatedBlocks.dbget bhdh with Not_found -> false) then (*** explicitly marked as invalid ***)
    vbcv false (fun c -> Printf.fprintf c "Block %s explicitly marked as invalid.\n" (hashval_hexstring bhdh))
  else
    begin
      match bhs.blocksignatendorsement with
      | None -> verify_p2pkhaddr_signat (hashval_big_int bhdh) bhd.stakeaddr bhs.blocksignat bhs.blocksignatrecid bhs.blocksignatfcomp
      | Some(beta,recid,fcomp,esg) -> (*** signature via endorsement ***)
	  begin
	    (verifybitcoinmessage bhd.stakeaddr recid fcomp esg ("endorse " ^ (addr_daliladdrstr (p2pkhaddr_addr beta)))
	       &&
	     verify_p2pkhaddr_signat (hashval_big_int bhdh) beta bhs.blocksignat bhs.blocksignatrecid bhs.blocksignatfcomp)
	  || (!Config.testnet (*** allow fake endorsements in testnet ***)
		&&
	      verifybitcoinmessage (-916116462l, -1122756662l, 602820575l, 669938289l, 1956032577l) recid fcomp esg ("fakeendorsement " ^ (addr_daliladdrstr (p2pkhaddr_addr beta)) ^ " (" ^ (addr_daliladdrstr (p2pkhaddr_addr bhd.stakeaddr)) ^ ")")
		&&
	      verify_p2pkhaddr_signat (hashval_big_int bhdh) beta bhs.blocksignat bhs.blocksignatrecid bhs.blocksignatfcomp)
	  end
    end

let valid_blockheader_a blkh csm tinfo (bhd,bhs) (aid,bday,obl,v) lmedtm burned =
  let b1 = valid_blockheader_signat (bhd,bhs) (aid,bday,obl,v) in
  if b1 then
    valid_blockheader_allbutsignat blkh csm tinfo bhd (aid,bday,obl,v) lmedtm burned
  else
    vbcv false (fun c -> Printf.fprintf c "block header has invalid signature\n")

exception HeaderNoStakedAsset
exception HeaderStakedAssetNotMin

let blockheader_stakeasset bhd =
  let bl = addr_bitseq (p2pkhaddr_addr bhd.stakeaddr) in
  match ctree_lookup_asset false false false bhd.stakeassetid bhd.prevledger bl with (** do not be strict here precisely because we *want* to skip abstracted assets **)
  | Some(a) ->
      let (aid,_,_,_) = a in
      vbc (fun c -> Printf.fprintf c "found staked asset %s\n" (hashval_hexstring aid));
      if minimal_asset_supporting_ctree bhd.prevledger bl aid 50 then (*** ensure that the ctree contains no extra information; this is a condition to prevent headers from being large by including unnecessary information; also only allow the first 50 assets held at an address to be used for staking ***)
	a
      else
	begin
	  vbc (fun c -> Printf.fprintf c "ctree in header not minimal for staked asset\n");
	  raise HeaderStakedAssetNotMin
	end
  | _ -> 
      vbc (fun c -> Printf.fprintf c "No staked asset found\n");
      raise HeaderNoStakedAsset
	
let valid_blockheader blkh csm tinfo (bhd,bhs) lmedtm burned =
  try
    vbc (fun c -> Printf.fprintf c "valid_blockheader %Ld %Ld %Ld\n" blkh lmedtm burned);
    valid_blockheader_a blkh csm tinfo (bhd,bhs) (blockheader_stakeasset bhd) lmedtm burned
  with
  | HeaderStakedAssetNotMin -> false
  | HeaderNoStakedAsset -> false

(** just check that the staked asset is there and that the signature is valid **)
let sanity_check_header (bhd,bhs) =
  try
    valid_blockheader_signat (bhd,bhs) (blockheader_stakeasset bhd)
  with _ -> false

let ctree_of_block (b:block) =
  let ((bhd,bhs),bd) = b in
  ctree_cgraft bd.prevledgergraft bhd.prevledger

let rec stxs_allinputs stxl =
  match stxl with
  | ((inpl,_),_)::stxr -> inpl @ stxs_allinputs stxr
  | [] -> []

let rec stxs_alloutputs stxl =
  match stxl with
  | ((_,outpl),_)::stxr -> outpl @ stxs_alloutputs stxr
  | [] -> []

(*** all txs of the block combined into one big transaction; used for checking validity of blocks ***)
let tx_of_block b =
  let ((bhd,_),bd) = b in
  let (ci,co) = coinstake b in
  (ci @ stxs_allinputs bd.blockdelta_stxl,co @ stxs_alloutputs bd.blockdelta_stxl)

let txl_of_block b =
  let (_,bd) = b in
  (coinstake b,List.map (fun (tx,_) -> tx) bd.blockdelta_stxl)

let rec stxl_hashroot stxl =
  merkle_root (List.map hashstx stxl)

let blockdelta_hashroot bd =
  hashpair
    (hashlist (List.map hash_addr_preasset bd.stakeoutput))
    (hashopair1
       (hashcgraft bd.prevledgergraft)
       (stxl_hashroot bd.blockdelta_stxl))

let valid_block_a tht sigt blkh csm tinfo b ((aid,bday,obl,u) as a) stkaddr lmedtm burned =
  let ((bhd,bhs),bd) = b in
  (*** The header is valid. ***)
  if (vbcb (valid_blockheader_a blkh csm tinfo (bhd,bhs) (aid,bday,obl,u) lmedtm burned) (fun c -> Printf.fprintf c "header valid\n") (fun c -> Printf.fprintf c "header invalid\n")
	&&
      vbcb (tx_outputs_valid bhd.timestamp bd.stakeoutput) (fun c -> Printf.fprintf c "stakeoutput valid\n") (fun c -> Printf.fprintf c "stakeoutput invalid\n")
        &&
      vbcb (blockdelta_hashroot bd = bhd.blockdeltaroot) (fun c -> Printf.fprintf c "delta Merkle root valid\n") (fun c -> Printf.fprintf c "delta Merkle root invalid\n") (*** the header commits to the blockdelta (including all txs and their signatures) ***)
	&&
      (*** ensure that if the stake has an explicit obligation (e.g., it is borrowed for staking), then the obligation isn't changed; otherwise the staker could steal the borrowed stake; unchanged copy should be first output ***)
      begin
	match a with
	| (_,_,Some(beta,n,r),Currency(v)) -> (*** stake may be on loan for staking ***)
	    begin
	      match bd.stakeoutput with
	      | (alpha2,(Some(beta2,n2,r2),Currency(v2)))::remouts -> (*** the first output must recreate the loaned asset. It's a reward iff it was already a reward. ***)
		  r2 = r
		    &&
		  alpha2 = stkaddr
		    &&
		  beta2 = beta
		    &&
		  n2 = n
		    &&
		  v2 = v
		    &&
		  begin
		    try (*** all other outputs must be marked as rewards; they also must acknowledge they cannot be spent for at least reward_locktime many blocks ***)
		      ignore (List.find (fun (alpha3,(obl,v)) -> match obl with Some(_,n,r) when r && n >= Int64.add blkh reward_locktime -> false | _ -> true) remouts);
		      false
		    with Not_found -> true
		  end
	      | _ ->
		  false
	    end
	| (_,_,None,Currency(v)) -> (*** stake has the default obligation ***)
	    begin (*** the first output is optionally the stake with the default obligation (not a reward, immediately spendable) with all other outputs must be marked as rewards; they also must acknowledge they cannot be spent for at least reward_locktime many blocks ***)
	      match bd.stakeoutput with
	      | (alpha2,(_,Currency(v2)))::remouts -> (*** allow the staker to choose the new obligation for the staked asset [Feb 2016] ***)
		  begin
		    alpha2 = stkaddr
		      &&
		    v2 = v
		      &&
		    try
		      ignore (List.find (fun (alpha3,(obl,v)) -> match obl with Some(_,n,r) when r && n >= Int64.add blkh reward_locktime -> false | _ -> true) remouts);
		      false
		    with Not_found -> true
		  end
	      | _ ->
		  try
		    ignore (List.find (fun (alpha3,(obl,v)) -> match obl with Some(_,n,r) when r && n >= Int64.add blkh reward_locktime -> false | _ -> true) bd.stakeoutput);
		    false
		  with Not_found -> true
	    end
	| _ -> false (*** this means the staked asset isn't currency, which is not allowed ***)
      end)
  then
    let tr = ctree_of_block b in (*** let tr be the ctree of the block, used often below ***)
    if ((try let z = ctree_supports_tx false false false tht sigt blkh (coinstake b) tr in (*** the ctree must support the tx without the need to expand hashes using the database or requesting from peers ***)
    z >= rewfn blkh
    with NotSupported -> false)
	  &&
	(*** There are no duplicate transactions. (Comparing the signatures would be an error since they contain abstract values.) ***)
	no_dups (List.map (fun (tau,_) -> tau) bd.blockdelta_stxl)
	  &&
	(*** The cgraft is valid. ***)
	cgraft_valid bd.prevledgergraft
	  &&
	let stakein = (stkaddr,bhd.stakeassetid) in
	(*** Each transaction in the delta has supported elaborated assets and is appropriately signed. ***)
	(*** Also, each transaction in the delta is valid and supported without a reward. ***)
	(*** Also, no transaction has the stake asset as an input. ***)
	(*** None of the outputs say they are rewards. ***)
	begin
	  try
	    List.fold_left
	      (fun sgvb stau ->
		match stau with
		| ((inpl,outpl) as tau,_) ->
		    let norew =
		      begin
			try
			  ignore (List.find 
				    (fun (a,(obl,v)) ->
				      match obl with
				      | Some(_,_,r) when r -> true
				      | _ -> false)
				    outpl);
			  false
			with Not_found -> true
		      end
		    in
		    let aal = ctree_lookup_input_assets false false false inpl tr (fun _ _ -> ()) in
		    let al = List.map (fun (_,a) -> a) aal in
		    norew
		      && sgvb
		      && not (List.mem stakein inpl)
		      && tx_signatures_valid blkh bhd.timestamp al stau
		      && tx_valid bhd.timestamp tau
		      && ctree_supports_tx_2 false false false tht sigt blkh tau aal al tr <= 0L
	      )
	      true
	      bd.blockdelta_stxl
	  with NotSupported -> false
	end
	  &&
	(*** No distinct transactions try to spend the same asset. ***)
	(*** Also, ownership is not created for the same address alpha by two txs in the block. ***)
	begin
	  try
	    let stxlr = ref bd.blockdelta_stxl in
	    while not (!stxlr = []) do
	      match !stxlr with
	      | ((inpl1,outpl1),_)::stxr ->
		  let hl1 = List.map (fun (_,h) -> h) inpl1 in
		  let oo1 = ref [] in
		  let op1 = ref [] in
		  List.iter
		    (fun (alpha1,(obl1,u1)) ->
		      match u1 with
		      | OwnsObj(_,_,_) -> oo1 := alpha1::!oo1
		      | OwnsProp(_,_,_) -> op1 := alpha1::!op1
		      | _ -> ())
		    outpl1;
		  stxlr := stxr;
		  List.iter
		    (fun ((inpl2,outpl2),_) ->
		      List.iter
			(fun (_,h) ->
			  if List.mem h hl1 then
			    raise NotSupported (*** This is a minor abuse of this exception. There could be a separate exception for this case. ***)
			) inpl2;
		      List.iter
			(fun (alpha2,(obl2,u2)) ->
			  match u2 with
			  | OwnsObj(_,_,_) ->
			      if List.mem alpha2 !oo1 then raise NotSupported
			  | OwnsProp(_,_,_) ->
			      if List.mem alpha2 !op1 then raise NotSupported
			  | _ -> ()
			)
			outpl2
		    )
		    stxr
	      | [] -> ()
	    done;
	    true
	  with NotSupported -> false
	end
	  &&
	(*** Ownership is not created for the same address alpha by the coinstake and a tx in the block. ***)
	begin
	  try
	    List.iter
	      (fun (alpha,(obl,u)) ->
		match u with
		| OwnsObj(_,_,_) ->
		    List.iter
		      (fun ((_,outpl2),_) ->
			List.iter
			  (fun (alpha2,(obl2,u2)) ->
			    if alpha = alpha2 then
			      match u2 with
			      | OwnsObj(_,_,_) -> raise NotSupported
			      | _ -> ())
			  outpl2)
		      bd.blockdelta_stxl
		| OwnsProp(_,_,_) ->
		    List.iter
		      (fun ((_,outpl2),_) ->
			List.iter
			  (fun (alpha2,(obl2,u2)) ->
			    if alpha = alpha2 then
			      match u2 with
			      | OwnsProp(_,_,_) -> raise NotSupported
			      | _ -> ())
			  outpl2)
		      bd.blockdelta_stxl
		| _ -> ()
	      )
	      bd.stakeoutput;
	    true
	  with NotSupported -> false
	end)
    then
      if (begin (*** The root of the transformed ctree is the newledgerroot in the header. ***)
	let (cstk,txl) = txl_of_block b in (*** the coinstake tx is performed last, i.e., after the txs in the block. ***)
	try
	  match tx_octree_trans false false blkh cstk (txl_octree_trans false false blkh txl (Some(tr))) with (*** "false false" disallows database lookups and remote requests ***)
	  | Some(tr2) ->
	      bhd.newledgerroot = ctree_hashroot tr2
	  | None -> false
	with MaxAssetsAtAddress -> false (** extra condition preventing addresses from holding too many assets **)
      end)
      then
	(*** The total inputs and outputs match up with the declared fee. ***)
	let tau = tx_of_block b in (*** let tau be the combined tx of the block ***)
	let (inpl,outpl) = tau in
	let aal = ctree_lookup_input_assets false false false inpl tr (fun _ _ -> ()) in
	let al = List.map (fun (_,a) -> a) aal in
	(*** Originally I added totalfees to the out_cost, but this was wrong since the totalfees are in the stake output which is already counted in out_cost. I don't really need totalfees to be explicit. ***)
	if out_cost outpl = Int64.add (asset_value_sum blkh al) (rewfn blkh) then
	  let newtht = txout_update_ottree outpl tht in
	  let newsigt = txout_update_ostree outpl sigt in
	  if bhd.newtheoryroot = ottree_hashroot newtht
	      &&
	    bhd.newsignaroot = ostree_hashroot newsigt
	  then
	    Some(newtht,newsigt)
	  else
	    None
	else
	  None
      else
	None
    else
      None
  else
    None

let valid_block tht sigt blkh csm tinfo (b:block) lmedtm burned =
  let ((bhd,_) as bh,_) = b in
  vbc (fun c -> Printf.fprintf c "Checking if block %s at height %Ld is valid.\n" (hashval_hexstring (blockheader_id bh)) blkh);
  let stkaddr = p2pkhaddr_addr bhd.stakeaddr in
  try
    valid_block_a tht sigt blkh csm tinfo b (blockheader_stakeasset bhd) stkaddr lmedtm burned
  with
  | HeaderStakedAssetNotMin -> None
  | HeaderNoStakedAsset -> None

type blockchain = block * block list
type blockheaderchain = blockheader * blockheader list

let blockchain_headers bc =
  let ((bh,bd),bl) = bc in
  (bh,List.map (fun b -> let (bh,bd) = b in bh) bl)

let ledgerroot_of_blockchain bc =
  let (((bhd,bhs),bd),bl) = bc in
  bhd.newledgerroot

(*** retargeting at each step
 (July 2017, changed to very slow block time target of 6 hours, 21600 seconds)
 ***)
let retarget_orig tar deltm =
  min_big_int
    !max_target
    (div_big_int
       (mult_big_int tar
	  (big_int_of_int32 (Int32.add 10000l deltm)))
       (big_int_of_int (10000 + 21600)))

let retarget_dampened tar deltm =
  min_big_int
    !max_target
    (div_big_int
       (mult_big_int tar
	  (big_int_of_int32 (Int32.add 1000000l deltm)))
       (big_int_of_int (1000000 + 21600)))

(* target 1 hour blocks on testnet *)
let retarget_testnet tar deltm =
  min_big_int
    !max_target
    (div_big_int
       (mult_big_int tar
	  (big_int_of_int32 (Int32.add 1000000l deltm)))
       (big_int_of_int (1000000 + 3600)))

let retarget blktm tar deltm =
  if blktm < !Utils.may2019hardforktime then (*** before May 2019 hard fork ***)
    retarget_orig tar deltm
  else if !Config.testnet && blktm > 1553970000L then
    retarget_testnet tar deltm
  else
    retarget_dampened tar deltm

let difficulty tar =
  div_big_int !max_target tar

let blockheader_succ_a prevledgerroot tmstamp1 tinfo1 bh2 =
  let (bhd2,bhs2) = bh2 in
  vbc (fun c -> Printf.fprintf c "Checking if header is valid successor\n");
  vbcb (ctree_hashroot bhd2.prevledger = prevledgerroot) (fun c -> Printf.fprintf c "hashroot of prevledger matches\n") (fun c -> Printf.fprintf c "prevledger mismatch: computed in bhd2 %s ; prevledgerroot given as %s\n" (hashval_hexstring (ctree_hashroot bhd2.prevledger)) (hashval_hexstring prevledgerroot))
    &&
  vbcb (bhd2.timestamp = Int64.add tmstamp1 (Int64.of_int32 bhd2.deltatime)) (fun c -> Printf.fprintf c "timestamp matches\n") (fun c -> Printf.fprintf c "timestamp mismatch bhd2 %Ld is not prev %Ld plus delta %ld\n" bhd2.timestamp tmstamp1 bhd2.deltatime)
    &&
  vbcb (eq_big_int bhd2.tinfo (retarget bhd2.timestamp tinfo1 bhd2.deltatime)) (fun c -> Printf.fprintf c "target info matches\n") (fun c -> Printf.fprintf c "target info mismatch %s vs (retarget %s %ld) = %s\n" (string_of_big_int bhd2.tinfo) (string_of_big_int tinfo1) bhd2.deltatime (string_of_big_int (retarget bhd2.timestamp tinfo1 bhd2.deltatime)))

let blockheader_succ_b pbh1 lr1 tmstmp1 tar1 bh2 =
  let (bhd2,_) = bh2 in
  match bhd2.prevblockhash with
  | Some(pbh,Poburn(lblkh,ltxh,lmedtm,burned)) ->
      tmstmp1 <= lmedtm
	&&
      pbh = pbh1 (*** the next block must also commit to the previous header with signature since the id hashes both the data and signature ***)
	&&
      blockheader_succ_a lr1 tmstmp1 tar1 bh2
  | None -> false

let blockheader_succ bh1 bh2 =
  let (bhd1,_) = bh1 in
  blockheader_succ_b (blockheader_id bh1) bhd1.newledgerroot bhd1.timestamp bhd1.tinfo bh2

let rec valid_blockchain_aux blkh bl lmedtm burned =
  match bl with
  | ((bh,bd)::(pbh,pbd)::br) ->
      if blkh > 1L then
	begin
	  let (bhd,bhs) = bh in
	  let (pbhd,pbhs) = pbh in
	  match bhd.prevblockhash with
	  | None -> raise NotSupported
	  | Some(bhprev,(Poburn(lblk,ltx,lmedtm1,burned1) as pob)) ->
	      let (tht,sigt) = valid_blockchain_aux (Int64.sub blkh 1L) ((pbh,pbd)::br) lmedtm1 burned1 in
	      let csm = poburn_stakemod pob in
	      if blockheader_succ pbh bh then
		begin
		  match valid_block tht sigt blkh csm pbhd.tinfo (bh,bd) lmedtm burned with
		  | Some(tht2,sigt2) -> (tht2,sigt2)
		  | None -> raise NotSupported
		end
	      else
		raise NotSupported
	end
      else
	raise NotSupported
  | [(bh,bd)] ->
      let (bhd,bhs) = bh in
      if blkh = 1L && bhd.prevblockhash = None
	  && blockheader_succ_a !genesisledgerroot !Config.genesistimestamp !genesistarget bh
      then
	begin
	  match valid_block None None blkh !genesisstakemod !genesistarget (bh,bd) lmedtm burned with
	  | Some(tht2,sigt2) -> (tht2,sigt2)
	  | None -> raise NotSupported
	end
      else
	raise NotSupported
  | [] -> raise NotSupported

let valid_blockchain blkh bc lmedtm burned =
  try
    let (b,bl) = bc in
    ignore (valid_blockchain_aux blkh (b::bl) lmedtm burned);
    true
  with NotSupported -> false

let rec valid_blockheaderchain_aux blkh bhl lmedtm burned =
  match bhl with
  | (bh::pbh::bhr) ->
      if blkh > 1L then
	begin
	  let (bhd,bhs) = bh in
	  let (pbhd,pbhs) = pbh in
	  match bhd.prevblockhash with
	  | None -> false
	  | Some(bhprev,(Poburn(lblk,ltx,lmedtm1,burned1) as pob)) ->
	      let csm = poburn_stakemod pob in
	      valid_blockheaderchain_aux (Int64.sub blkh 1L) (pbh::bhr) lmedtm1 burned1
		&& blockheader_succ pbh bh
		&& valid_blockheader blkh csm pbhd.tinfo bh lmedtm burned
	end
      else
	false
  | [(bhd,bhs)] ->
      blkh = 1L
	&&
      valid_blockheader blkh !genesisstakemod !genesistarget (bhd,bhs) lmedtm burned
	&&
      bhd.prevblockhash = None
	&&
      ctree_hashroot bhd.prevledger = !genesisledgerroot
	&&
      blockheader_succ_a !genesisledgerroot !Config.genesistimestamp !genesistarget (bhd,bhs)
  | [] -> false

let valid_blockheaderchain blkh bhc burned =
  match bhc with
  | (bh,bhr) -> valid_blockheaderchain_aux blkh (bh::bhr) burned

let rec collect_header_inv_nbhd m h tosend =
  if m > 0 then
    begin
      try
	let (bhd,_) = DbBlockHeader.dbget h in
	collect_header_inv_nbhd_2 m h bhd tosend
      with Not_found -> ()
    end
and collect_header_inv_nbhd_2 m h bhd tosend =
  tosend := (int_of_msgtype Headers,h)::!tosend;
  if DbCTreeElt.dbexists bhd.newledgerroot then tosend := (int_of_msgtype CTreeElement,bhd.newledgerroot)::!tosend;
  if DbBlockDelta.dbexists h then tosend := (int_of_msgtype Blockdelta,h)::!tosend;
  begin
    match bhd.prevblockhash with
    | Some(k,_) -> collect_header_inv_nbhd (m-1) k tosend
    | _ -> ()
  end

let unburned_headers : (hashval,(hashval * hashval) -> unit) Hashtbl.t = Hashtbl.create 100
let unburned_deltas : (hashval,(hashval * hashval) -> unit) Hashtbl.t = Hashtbl.create 100

