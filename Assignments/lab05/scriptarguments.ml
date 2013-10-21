(* command argument flags *)
let common_arguments = [
  ("--trace-failure", Arg.Set Globals.trace_failure,
   "Enable trace all failure (and exception)");
  ("-debug", Arg.String (fun s ->
      Debug.z_debug_file:=s; Debug.z_debug_flag:=true),
   "Read from a debug log file");
  ("-debug-regexp", Arg.String (fun s ->
      Debug.z_debug_file:=("$"^s); Debug.z_debug_flag:=true),
   "Match logged methods from a regular expression");
  ("-dre", Arg.String (fun s ->
      Debug.z_debug_file:=("$"^s); Debug.z_debug_flag:=true),
   "Shorthand for -debug-regexp");
  ("-v", Arg.Set Debug.debug_on,
   "Verbose")
  ]
