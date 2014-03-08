(********************************************************************************)
(*	Counterparty.ml
	Copyright (c) 2012 Dario Teixeira (dario.teixeira@yahoo.com)
*)
(********************************************************************************)


(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Unspecified_connection
exception Counterparty_error of int * string * string
exception Httpclient_error of exn


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type tx_hex_t = string
type tx_id_t = string
type address_t = string
type quantity_t = int64
type amount_t = int64
type block_index_t = int
type asset_t = XCP | BTC | ASSET of string
let string_of_asset = function XCP -> "XCP" | BTC -> "BTC" | ASSET s -> s
let asset_of_string = function "XCP" -> XCP | "BTC" -> BTC | s -> ASSET s
type assoc_t = (string * Yojson.Safe.json) list

type order_by_t = string
type order_dir_t = ASC | DESC
let string_of_order_dir = function ASC -> "asc" | DESC -> "desc"

type filterop_t = AND | OR
let string_of_filterop = function AND -> "and" | OR -> "or"

type conn_t =
	{
	inet_addr: Unix.inet_addr;
	host: string;
	port: int;
	username: string;
	password: string;
	}

(* Unix.inet_addr_of_string *)

(********************************************************************************)
(**	{1 Public module types}							*)
(********************************************************************************)

module Filter = struct
  type field_t = string
  type value_t = string
  type op_t = EQ | NE | LT | GT | LE | GE
  type t = field_t*op_t*value_t

  let string_of_op = function EQ -> "==" | NE -> "!=" | LT -> "<" | GT -> ">" | LE -> "<=" | GE -> ">="
  let json_of_filter (field,op,value) = `Assoc [("field",`String field);("op",`String (string_of_op op));("value",`String value)];
end

module Credit = struct
  type t = {asset: asset_t; address: address_t; amount: amount_t; block_index: block_index_t; event: tx_id_t option}
end

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


module type CONNECTION =
sig
	val default: conn_t option
end


module type ENGINE =
sig
	type 'a monad_t

	val create_send: ?conn:conn_t -> source:address_t -> destination:address_t -> asset_t -> quantity_t -> tx_hex_t monad_t
	val transmit: ?conn:conn_t -> ?is_signed:bool -> tx_hex_t -> tx_id_t monad_t
    val get_credits: ?conn:conn_t -> ?filters:Filter.t list -> ?order_by:order_by_t -> ?order_dir:order_dir_t -> ?filterop: filterop_t -> unit -> (Credit.t list) monad_t
end


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let amount_of_float x =
	Int64.of_float (x *. 1e8 +. (if x < 0.0 then -0.5 else 0.5))

let float_of_amount x =
	(Int64.to_float x) /. 1e8


(********************************************************************************)
(**	{1 Public functors}							*)
(********************************************************************************)

module Make (Httpclient: HTTPCLIENT) (Connection: CONNECTION) : ENGINE with type 'a monad_t = 'a Httpclient.Monad.t =
struct
	open Httpclient

	(************************************************************************)
	(**	{2 Type definitions}						*)
	(************************************************************************)

	type 'a monad_t = 'a Monad.t


	(************************************************************************)
	(**	{2 Private functions and values}				*)
	(************************************************************************)


	(************************************************************************)
	(**	{3 Common operators}						*)
	(************************************************************************)

	let (|>) x f = f x

	let (|?) x y = match x with
		| Some x -> x
		| None	 -> y

	let (>>=) t f = Monad.bind t f

	let (>|=) m f = Monad.bind m (fun x -> Monad.return (f x))


	(************************************************************************)
	(**	{3 Low-level functions}						*)
	(************************************************************************)

    let sort_assoc l = List.sort (fun a b -> compare (fst a) (fst b)) l

	let invoke ?conn ~params methode =
		begin match (conn, Connection.default) with
			| (Some conn, _)    -> Monad.return conn
			| (None, Some conn) -> Monad.return conn
			| (None, None)	    -> Monad.fail Unspecified_connection
		end >>= fun conn ->
		let headers =
			[
			("content-type", "application/json");
			("authorization", "Basic " ^ (Cryptokit.transform_string (Cryptokit.Base64.encode_compact_pad ()) (conn.username ^ ":" ^ conn.password)));
			] in
		let request = `Assoc
			[
			("method", `String methode);
            ("jsonrpc", `String "2.0");
			("params", params);
			("id", `Int 0);
			] in
		let xrequest = Yojson.Safe.to_string request in
        (*Printf.eprintf "%s\n" xrequest; flush_all ();*)
		Monad.catch
			(fun () ->
				Httpclient.post_string
					~headers
					~inet_addr:conn.inet_addr
                    ~host:conn.host
					~port:conn.port
					~uri:"/api/"
					xrequest)
			(function exc -> Monad.fail (Httpclient_error exc)) >>= fun xresponse ->
		match Yojson.Safe.from_string xresponse with
			| `Assoc assoc ->
               (*Printf.eprintf "*%s*\n" (Yojson.Safe.to_string (`Assoc (sort_assoc assoc))); flush_all ();*)
               begin match (sort_assoc assoc) with
                     | [("error", `Assoc assoc); ("id", _); ("jsonrpc", _); ] -> 
                        begin match (sort_assoc assoc) with
                              | [("code", `Int code); ("message", `String message)] ->
                                 let exc = Counterparty_error (code, "", message)
						         in Monad.fail exc
                              | [("code", `Int code); ("data", `Assoc assoc); ("message", _)] ->
                                 begin match (sort_assoc assoc) with
                                       | [("args", _); ("message", `String message);("type", `String error_type)] ->
						                  let exc = Counterparty_error (code, error_type, message)
						                  in Monad.fail exc
                                       | _ -> assert false
                                 end
                              | _ -> assert false
                        end
                     | [("id", _); ("jsonrpc", _); ("result",x)] ->   
						Monad.return x
                     | _  -> 
                        (*Printf.eprintf "%s\n" xresponse;*)
                        assert false
               end
			| _ -> assert false


	(************************************************************************)
	(**	{3 Conversion between {!sigcomp_t} and [string]			*)
	(************************************************************************)

	let string_of_sigcomp = function
		| `All	  -> "ALL"
		| `None	  -> "NONE"
		| `Single -> "SINGLE"


	(************************************************************************)
	(**	{3 Conversion between {!addnodeop_t} and [string]		*)
	(************************************************************************)

	let string_of_addnodeop = function
		| `Add	  -> "add"
		| `Remove -> "remove"
		| `Onetry -> "onetry"


	(************************************************************************)
	(**	{3 Conversion from JSON values to OCaml values}			*)
	(************************************************************************)

	let to_unit = function
		| `Null -> ()
		| _	-> assert false

	let to_option f = (fun x ->
        match x with
		| `Null -> None
		| _	-> Some (f x)
    )

	let to_int = function
		| `Int x -> x
		| _	 -> assert false

	let to_int64 = function
		| `Int x    -> Int64.of_int x
		| `Intlit x -> Int64.of_string x
		| _	    -> assert false

	let to_float = function
		| `Float x -> x
		| _	   -> assert false

	let to_bool = function
		| `Bool x -> x
		| _	  -> assert false

	let to_string = function
		| `String x -> x
		| _	    -> assert false

	let to_amount x =      
		to_float x |> amount_of_float

	let to_list f = function
		| `List xs -> List.map f xs
		| _	   -> assert false

	let to_assoc = function
		| `Assoc xs -> xs
		| _	    -> assert false

	let to_sorted_assoc = function
		| `Assoc xs -> sort_assoc xs
		| _	    -> assert false

	let (=|=) x f = match x with
		| Some x -> f x
		| None	 -> `Null

	(************************************************************************)
	(**	{3 Conversion from OCaml values to JSON values}			*)
	(************************************************************************)

	let of_int x = `Int x

	let of_float x = `Float x

	let of_bool x = `Bool x

	let of_string x = `String x

	let of_list conv xs = `List (List.map conv xs)

	let of_assoc x = `Assoc x

	let of_amount x = float_of_amount x |> of_float

    let of_quantity x = `Intlit x

	let params_of_1tuple f1 = function
		| Some x -> [f1 x]
		| None   -> []

	let params_of_2tuple fname f1 f2 = function
		| (Some x, Some y) -> [f1 x; f2 y]
		| (Some x, None)   -> [f1 x]
		| (None, None)     -> []
		| _		   -> invalid_arg fname

	let params_of_3tuple fname f1 f2 f3 = function
		| (Some x, Some y, Some z) -> [f1 x; f2 y; f3 z]
		| (Some x, Some y, None)   -> [f1 x; f2 y]
		| (Some x, None, None)	   -> [f1 x]
		| (None, None, None)	   -> []
		| _			   -> invalid_arg fname

	let create_send ?conn ~source ~destination asset quantity =
	  let params = `List [of_string source; of_string destination; of_string (string_of_asset asset); of_quantity (Int64.to_string quantity)] in
	  invoke ?conn ~params "create_send" >|= to_string

    let transmit ?conn ?(is_signed=false) tx_hex =
      let params = `List [of_string tx_hex; of_bool is_signed] in
      invoke ?conn ~params "transmit" >|= to_string

    let get_credits ?conn ?filters ?order_by ?order_dir ?(filterop=AND) () =
      let to_result assocs = 
        to_list
          (fun assoc ->
           (*let yyy = to_sorted_assoc assoc in
           Printf.eprintf "#%s#" (Yojson.Safe.to_string (`Assoc yyy));*)
           match to_sorted_assoc assoc with
           | [("address", `String address);("amount", amount);("asset", `String asset);("block_index", `Int block_index);("calling_function", _);("event", event)] ->
              { Credit.address=address; amount=to_int64 amount; asset=asset_of_string asset; block_index=block_index; event = to_option to_string event }
           | _ -> assert false
          ) assocs
      in
      let params = `Assoc [
                      ("filters", filters =|= (fun l -> `List (List.map (fun filter -> Filter.json_of_filter filter) l)));
                      ("order_by", order_by =|= of_string);
                      ("order_dir", order_dir =|= (fun x -> of_string (string_of_order_dir x)));
                      ("filterop", of_string (string_of_filterop filterop));
                    ] in
      invoke ?conn ~params "get_credits" >|= to_result
end
