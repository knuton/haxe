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

let rec print_to_buffer printer json =
	let insert_sep pos = if pos = 0 then () else printer ", " in
	match json with
	| Assoc pairs ->
		printer "{ ";
		List.iteri (fun i (key, value) ->
			insert_sep i;
			printer "\"";
			printer (String.escaped key);
			printer "\": ";
			print_to_buffer printer value
		) pairs;
		printer " }";
	| Bool value ->
		printer (if value then "true" else "false");
	| Float value ->
		printer (string_of_float value);
	| Int value ->
		printer (string_of_int value);
	| List values ->
		printer "[";
		List.iteri (fun i value ->
			insert_sep i;
			print_to_buffer printer value;
		) values;
		printer "]";
	| Null ->
		printer "null";
	| String value ->
		printer "\"";
		printer (String.escaped value);
		printer "\"";