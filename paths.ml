
let die fmt =
  Printf.ksprintf (fun str -> Printf.eprintf "Error: %s\n%!" str; exit 1) fmt

module String_set = struct
  include Set.Make (String)
  let of_list li = List.fold_right add li empty
end

module String_map = struct
  include Map.Make (String)
  let of_list li = List.fold_right (fun (key, value) -> add key value) li empty
end

type paths = string list
type context = paths String_map.t
type contexts = context String_map.t

let has_prefix str prefix =
  let prefix_len = String.length prefix in
  String.length str >= prefix_len && String.sub str 0 prefix_len = prefix

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines

let context_name_re = Str.regexp "^\\[\\([^]]+\\)\\]$"
let paths_re = Str.regexp "^\\([A-Z_0-9]+\\)=\\(.*\\)$"
let colon_re = Str.regexp_string ":"

let parse_paths : string -> paths =
  fun str ->
    Str.split colon_re str

let unparse_paths : paths -> string =
  fun paths ->
    String.concat ":" paths

let parse_config_line line =
  if String.length line = 0 then
    `Ignore
  else
    if Str.string_match context_name_re line 0 then
      `Context_name (Str.matched_group 1 line)
    else if Str.string_match paths_re line 0 then
      let varname = Str.matched_group 1 line in
      let paths = parse_paths (Str.matched_group 2 line) in
      `Paths (varname, paths)
    else
      die "Strange line: %S" line

let parse_config lines =
  let f (contexts, opt_state) = function
    | `Ignore ->
        (contexts, opt_state)
    | `Context_name context_name ->
        let contexts =
          match opt_state with
            | None ->
                contexts
            | Some (old_context_name, context) ->
                String_map.add old_context_name context contexts
        in
        contexts, Some (context_name, String_map.empty)
    | `Paths (varname, paths) ->
        let state =
          match opt_state with
            | None ->
                die "Strange line placement: %s=%s"
                  varname (String.concat ":" paths)
            | Some (context_name, context) ->
                Some (context_name, String_map.add varname paths context)
        in
        contexts, state
  in
  let contexts, opt_state = List.fold_left f (String_map.empty, None) lines in
  match opt_state with
    | None -> contexts
    | Some (context_name, context) -> String_map.add context_name context contexts

let unparse_context context =
  String_map.iter
    (fun varname paths ->
       Printf.printf "%s=%s\n"
         varname
         (String.concat ":" paths))
    context

let unparse_config contexts =
  String_map.iter
    (fun context_name context ->
       Printf.printf "[%s]\n" context_name;
       unparse_context context)
    contexts;
  Printf.printf "%!"


let contexts =
  parse_config (List.map parse_config_line (read_file "paths.config"))

let is_enabled : context -> bool =
  fun paths ->
    List.for_all
      (function (var, paths) ->
         let env_paths = try parse_paths (Sys.getenv var) with Not_found -> [] in
         List.for_all (fun path -> List.mem path env_paths) paths)
      (String_map.bindings paths)

let explore () =
  print_endline "\027[31mAvailable contexts\027[0m";
  unparse_config contexts;
  print_endline "\n\027[31mRelevant env variables\027[0m";
  let varnames =
    String_map.fold
      (fun _ context ->
         String_map.fold
           (fun varname _ -> String_set.add varname)
           context)
      contexts String_set.empty
  in
  String_set.iter
    (fun varname ->
       Printf.printf "%s=%s\n" varname (try Sys.getenv varname with Not_found -> ""))
    varnames;
  let enabled, disabled =
    List.partition
      (fun (_, paths) -> is_enabled paths)
      (String_map.bindings contexts)
  in
  print_endline "\n\027[31mEnabled contexts\027[0m";
  List.iter (fun (name, _) -> Printf.printf "%s " name) enabled;
  print_endline "\n\n\027[31mDisabled contexts\027[0m";
  List.iter (fun (name, _) -> Printf.printf "%s " name) disabled;
  Printf.printf "\n%!"

let explore_context context_name =
  try
    unparse_context (String_map.find context_name contexts)
  with Not_found -> die "Unknown context %s" context_name

let add name =
  try
    let paths = String_map.find name contexts in
    String_map.iter
      (fun varname paths ->
         let env_paths = parse_paths (Sys.getenv varname) in
         let new_env_paths =
           List.filter (fun path -> not (List.mem path env_paths)) paths @ env_paths in
         if env_paths <> new_env_paths then
           Printf.printf "export %s=%s\n%!"
             varname (unparse_paths new_env_paths))
      paths
  with Not_found ->
    die "Context %s is unknown" name

let remove name =
  try
    let context = String_map.find name contexts in
    let enabled_contexts = String_map.filter (fun _ context -> is_enabled context) contexts in
    if not (is_enabled context) then
      die "Context %s is not enabled" name;
    String_map.iter
      (fun varname paths ->
         let env_paths = parse_paths (Sys.getenv varname) in
         let new_env_paths =
           let is_local path = List.mem path paths in
           let is_otherwise_enabled path =
             List.exists
               (function _, context ->
                  try List.mem path (String_map.find varname context)
                  with Not_found -> false)
               (String_map.bindings enabled_contexts)
           in
           List.filter (fun path -> not (is_local path || is_otherwise_enabled path)) env_paths
         in
         if env_paths <> new_env_paths then
           Printf.printf "export %s=%s\n%!"
             varname (unparse_paths new_env_paths))
      context
  with Not_found ->
    die "Context %s is unknown" name

let () =
  Arg.parse [
    "-explore-all", Arg.Unit explore, "Explore the active paths";
    "-explore", Arg.String explore_context, "Explore the active paths";
    "-add", Arg.String add, "Add a context";
    "-remove", Arg.String remove, "Remove a context";
  ] (die "Unknown option %s") "hello"


