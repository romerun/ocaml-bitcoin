(********************************************************************************)
(*	Test.ml
	Copyright (c) 2012 Dario Teixeira (dario.teixeira@yahoo.com)
*)
(********************************************************************************)

open OUnit
open Printf
open Counterparty

(********************************************************************************)
(**	{1 Inner modules}							*)
(********************************************************************************)

module Testcoin_connection =
struct
	let default = Some
		{
		inet_addr = Unix.inet_addr_loopback;
        host = "localhost";
		port = 4000;
		username = "rpcuser";
		password = "graffitiseis";
		}
end


module Testcoin = Counterparty.Make (Counterparty_ocamlnet.Httpclient) (Testcoin_connection)

(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type global_t =
	{
	  address1: address_t;
	  address2: address_t;
	  address3: address_t;
	}

(********************************************************************************)
(**	{1 Setup functions}							*)
(********************************************************************************)

let () = Random.self_init ()

let global = {
  address1 = "mu7pML1JVJ8fukGw9sjqyAbrUW5GEKR1wF";
  address2 = "n29wjkYhPQprHRVZWxi345EMK6jDHz17Ly";
  address3 = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF";
}

(********************************************************************************)
(**	{1 Test functions}							*)
(********************************************************************************)

let (!!) test = fun () -> test global

let test_error global =
  try
    ignore (Testcoin.create_send ~source:global.address3 ~destination:global.address2 XCP 1000000L);
    assert_failure "Error json parsing"
  with
  | Counterparty.Counterparty_error(-32000, "SendError", "insufficient funds") -> ()
(*  | _ -> assert_failure "Error json parsing"*)

let test_transmit global =
  try
    let tx_hex = Testcoin.create_send ~source:global.address1 ~destination:global.address2 XCP 1000000L in
    let tx_id = Testcoin.transmit tx_hex in
    eprintf "sent - %s\n" tx_id
  with
  | Counterparty.Counterparty_error(-32000, "BalanceError", _) -> ()
(*  | _ -> assert_failure "Error json parsing"*)

let test_get_credits global =
  try
    let credits = Testcoin.get_credits ~filters:[("asset",Counterparty.Filter.EQ,"CAKE")] () in
    eprintf "xxx\n"
  with
  | Counterparty.Counterparty_error(-32000, "BalanceError", _) -> ()
(*  | _ -> assert_failure "Error json parsing"*)
    

(********************************************************************************)
(**	{1 Main functions and values}						*)
(********************************************************************************)

let suite = "OCaml-couterparty" >:::
	[
	  "test_error" >:: !!test_error;
      "test_transmit" >:: !!test_transmit;
      "test_get_credits" >:: !!test_get_credits;
	]

let _ =
  run_test_tt_main suite

