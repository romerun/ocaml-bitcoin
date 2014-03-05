(********************************************************************************)
(*	Counterparty.mli
	Copyright (c) 2012 Dario Teixeira (dario.teixeira@yahoo.com)
*)
(********************************************************************************)

(**	OCaml interface to the official Counterparty client API.
*)

(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Unspecified_connection			               (** Raised when connection parameter is not given and no default connection exists. *)
exception Counterparty_error of int * string * string      (** Error reported by the Counterparty client. *)
exception Httpclient_error of exn                          (** Connection error reported by the {!HTTPCLIENT} *)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type tx_hex_t = string
type tx_id_t = string
type address_t = string
type quantity_t = int64
type asset_t = XCP | BTC | ASSET of string
type assoc_t = (string * Yojson.Safe.json) list

type conn_t =
	{
	inet_addr: Unix.inet_addr;
	host: string;
	port: int;
	username: string;				(* Should match [rpcuser] value in $HOME/.counterparty/counterparty.conf *)
	password: string;				(* Should match [rpcpassword] value in $HOME/.counterparty/counterparty.conf *)
	}


(********************************************************************************)
(**	{1 Public module types}							*)
(********************************************************************************)

(**	Interface that any module offering HTTP POST client calls must obey.
	Note that the module may require POST calls to be wrapped under a
	custom monad, which must also be provided (use the identity monad
	if an actual monad is not required).
*)
module type HTTPCLIENT =
sig
	module Monad:
	sig
		type 'a t

		val return: 'a -> 'a t
		val fail: exn -> 'a t
		val bind: 'a t -> ('a -> 'b t) -> 'b t
		val catch: (unit -> 'a t) -> (exn -> 'a t) -> 'a t
	end

	val post_string:
		headers:(string * string) list ->
		inet_addr:Unix.inet_addr ->
        host:string ->
		port:int ->
		uri:string ->
		string ->
		string Monad.t
end


(**	Module encapsulating all connection information.
*)
module type CONNECTION =
sig
	val default: conn_t option
end


(**	Actual engine offering the Counterparty API.
*)
module type ENGINE =
sig
	type 'a monad_t

	val create_send: ?conn:conn_t -> source:address_t -> destination:address_t -> asset_t -> quantity_t -> tx_hex_t monad_t
	val transmit: ?conn:conn_t -> ?is_signed:bool -> tx_hex_t -> tx_id_t monad_t
end


(********************************************************************************)
(**	{1 Public functors}							*)
(********************************************************************************)

(**	Functor that takes a concrete implementation of a {!HTTPCLIENT} and actual
	{!CONNECTION} information, and creates a module with signature {!ENGINE}
	offering an API for communicating with a Counterparty client.
*)
module Make:
	functor (Httpclient: HTTPCLIENT) ->
	functor (Connection: CONNECTION) ->
	ENGINE with type 'a monad_t = 'a Httpclient.Monad.t

