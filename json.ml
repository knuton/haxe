type json =
	| Assoc of (string * json) list
	| Bool of bool
	| Float of float
	| Int of int
	| List of json list
	| Null
	| String of string

let rec json_to_string json =
	let insert_sep prefix = if prefix = "" then prefix else prefix ^ ", " in
	match json with
	| Assoc pairs ->
		let join_pair (key, value) = "\"" ^ String.escaped key ^ "\": " ^ json_to_string value in
		"{ " ^ List.fold_left (fun acc pair -> insert_sep acc ^ join_pair pair) "" pairs ^ " }"
	| Bool value ->
		if value then "true" else "false"
	| Float value ->
		string_of_float value
	| Int value ->
		string_of_int value
	| List values ->
		"[" ^ List.fold_left (fun acc value -> insert_sep acc ^ json_to_string value) "" values ^ "]"
	| Null ->
		"null"
	| String value ->
		"\"" ^ String.escaped value ^ "\""