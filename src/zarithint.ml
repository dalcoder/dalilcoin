(* Copyright (c) 2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

let z_of_big_int x =
  Z.of_string (Big_int.string_of_big_int x)

let big_int_of_z x =
  Big_int.big_int_of_string (Z.to_string x)

let assert_eq msg x y =
  if not (Z.to_string x = Big_int.string_of_big_int y) then
    raise (Failure (Printf.sprintf "Z Big_int disagreement\n%s\n%s\n%s\n" msg (Z.to_string x) (Big_int.string_of_big_int y)))

let assert_eq_2 msg (x1,x2) (y1,y2) =
  assert_eq msg x1 y1;
  assert_eq msg x2 y2
  
let zero_big_int = Z.zero
let unit_big_int = Z.one

let big_int_of_string x =
  let r = Z.of_string x in
  let r2 = Big_int.big_int_of_string x in
  (*  assert_eq (Printf.sprintf "big_int_of_string %s" x) r r2; *)
  r

let string_of_big_int x =
  let r = Z.to_string x in
  (*  assert_eq (Printf.sprintf "big_int_of_string %s" r) x (Big_int.big_int_of_string r); *)
  r

let int_of_big_int x = Z.to_int x
let int32_of_big_int x = Z.to_int32 x
let int64_of_big_int x = Z.to_int64 x

let big_int_of_int x =
  let r = Z.of_int x in
  (*  assert_eq (Printf.sprintf "big_int_of_int %d" x) r (Big_int.big_int_of_int x); *)
  r

let big_int_of_int32 x =
  let r = Z.of_int32 x in
  (*  assert_eq (Printf.sprintf "big_int_of_int32 %ld" x) r (Big_int.big_int_of_int32 x); *)
  r

let big_int_of_int64 x =
  let r = Z.of_int64 x in
  (*  assert_eq (Printf.sprintf "big_int_of_int64 %Ld" x) r (Big_int.big_int_of_int64 x); *)
  r

let eq_big_int x y = Z.equal x y
let le_big_int x y = Z.leq x y
let ge_big_int x y = Z.geq x y
let lt_big_int x y = Z.lt x y
let gt_big_int x y = Z.gt x y
let sign_big_int x = Z.sign x

let succ_big_int x =
  let r = Z.succ x in
  (*  assert_eq (Printf.sprintf "succ_big_int %s" (Z.to_string x)) r (Big_int.succ_big_int (big_int_of_z x)); *)
  r

let add_big_int x y =
  let r = Z.add x y in
  (*  assert_eq (Printf.sprintf "add_big_int %s %s" (Z.to_string x) (Z.to_string y)) r (Big_int.add_big_int (big_int_of_z x) (big_int_of_z y)); *)
  r

let add_int_big_int x y = add_big_int (Z.of_int x) y
  
let sub_big_int x y =
  let r = Z.sub x y in
  (*  assert_eq (Printf.sprintf "sub_big_int %s %s" (Z.to_string x) (Z.to_string y)) r (Big_int.sub_big_int (big_int_of_z x) (big_int_of_z y)); *)
  r
  
let mult_big_int x y =
  let r = Z.mul x y in
  (*  assert_eq (Printf.sprintf "mult_big_int %s %s" (Z.to_string x) (Z.to_string y)) r (Big_int.mult_big_int (big_int_of_z x) (big_int_of_z y)); *)
  r

let mult_int_big_int x y = mult_big_int (Z.of_int x) y

let div_big_int x y =
  let r = Z.div x y in
  (*  assert_eq (Printf.sprintf "div_big_int %s %s" (Z.to_string x) (Z.to_string y)) r (Big_int.div_big_int (big_int_of_z x) (big_int_of_z y)); *)
  r

let mod_big_int x y =
  let r =
    if Z.sign x < 0 then
      Z.add y (Z.rem x y)
    else
      Z.rem x y
  in
  (*  assert_eq (Printf.sprintf "mod_big_int %s %s" (Z.to_string x) (Z.to_string y)) r (Big_int.mod_big_int (big_int_of_z x) (big_int_of_z y)); *)
  r
  
let quomod_big_int x y =
  let r = Z.div_rem x y in
  (*  assert_eq_2 (Printf.sprintf "quomod_big_int %s %s" (Z.to_string x) (Z.to_string y)) r (Big_int.quomod_big_int (big_int_of_z x) (big_int_of_z y)); *)
  r
  
let power_big_int_positive_int x y =
  let r = Z.pow x y in
  (*  assert_eq (Printf.sprintf "power_big_int_positive_int %s %d" (Z.to_string x) y) r (Big_int.power_big_int_positive_int (big_int_of_z x) y); *)
  r

let min_big_int x y =
  let r = Z.min x y in
  (*  assert_eq (Printf.sprintf "min_big_int %s %s" (Z.to_string x) (Z.to_string y)) r (Big_int.min_big_int (big_int_of_z x) (big_int_of_z y)); *)
  r

let and_big_int x y =
  let r = Z.logand x y in
  (*  assert_eq (Printf.sprintf "and_big_int %s %s" (Z.to_string x) (Z.to_string y)) r (Big_int.and_big_int (big_int_of_z x) (big_int_of_z y)); *)
  r

let or_big_int x y =
  let r = Z.logor x y in
  (*  assert_eq (Printf.sprintf "or_big_int %s %s" (Z.to_string x) (Z.to_string y)) r (Big_int.or_big_int (big_int_of_z x) (big_int_of_z y)); *)
  r

let shift_left_big_int x y =
  let r = Z.shift_left x y in
  (*  assert_eq (Printf.sprintf "shift_left_big_int %s %d" (Z.to_string x) y) r (Big_int.shift_left_big_int (big_int_of_z x) y); *)
  r
  
let shift_right_big_int x y =
  let r = Z.shift_right x y in
  (*  assert_eq (Printf.sprintf "shift_right_big_int %s %d" (Z.to_string x) y) r (Big_int.shift_right_big_int (big_int_of_z x) y); *)
  r

let shift_right_towards_zero_big_int x y =
  let r = Z.shift_right_trunc x y in
  (*  assert_eq (Printf.sprintf "shift_right_towards_zero_big_int %s %d" (Z.to_string x) y) r (Big_int.shift_right_towards_zero_big_int (big_int_of_z x) y); *)
  r
