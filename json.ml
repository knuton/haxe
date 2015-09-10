type json =
	| JsonAssoc of (string * json) list
	| JsonBool of bool
	| JsonFloat of float
	| JsonInt of int
	| JsonList of json list
	| JsonNull
	| JsonString of string

let rec json_to_string json =
	let insert_sep prefix = if prefix = "" then prefix else prefix ^ ", " in
	match json with
	| JsonAssoc pairs ->
		let join_pair (key, value) = "\"" ^ String.escaped key ^ "\": " ^ json_to_string value in
		"{ " ^ List.fold_left (fun acc pair -> insert_sep acc ^ join_pair pair) "" pairs ^ " }"
	| JsonBool value ->
		if value then "true" else "false"
	| JsonFloat value ->
		string_of_float value
	| JsonInt value ->
		string_of_int value
	| JsonList values ->
		"[" ^ List.fold_left (fun acc value -> insert_sep acc ^ json_to_string value) "" values ^ "]"
	| JsonNull ->
		"null"
	| JsonString value ->
		"\"" ^ String.escaped value ^ "\""