(**
This program was originally written by Luis Leopoldo Pérez on March 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by 
students of ITCR in January 2019.
Program entrypoint for Caml-Triangle
Implementation file                                  

@author Luis Leopoldo Pérez
@author Jose Antonio Alpízar Aguilar
@author Pablo Josué Brenes Jimenes
@author Luis José Castillo Valverde
*)

open Printf

(** File output variables for printers *)
let output_file = ref "a.out"
let xml_error_file = ref ""
let xml_tree_file = ref ""
let xml_decorated_tree_file = ref ""
let tokens_file_pipe = ref ""
let tokens_file_xml = ref ""
let tokens_file_html = ref ""
let do_usage = ref false

(** Variables for tree storage *)
let astree = ref Ast.Null_program
let dastree = ref Ast.Null_program

(* Compiles the program! *)


(** Function that the name of a file reads *)
let read_file file = (
  let ic = open_in file in
  let buf = Buffer.create (in_channel_length ic) in
  try
    while true do
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
    done;
    assert false
  with 
    End_of_file -> Buffer.contents buf 
)


(** 
Function generates impression of tokens separated by pipes
@param inpt Source file
@param file_name Destination file 
*)
let print_tokens_funtion_pipe inpt file_name =
  printf "Generate tokens with pipes....\n";    
  let lexbuf = (Lexing.from_string (read_file inpt)) in
  try
    TokenPrinter_Pipe.print_tokens Scanner.scan_token lexbuf file_name;
  with 
    _ -> ()

(** 
Function generates impression of tokens in xml file
@param inpt Source file
@param file_name Destination file 
*)
let print_tokens_funtion_xml inpt file_name =
  printf "Generate tokens in xml file....\n";
  let lexbuf = (Lexing.from_string (read_file inpt)) in
  try
    TokenPrinter_XML.print_tokens Scanner.scan_token lexbuf file_name;
  with
    _ -> ()

(** 
Function generates impression of tokens in HTML file
@param inpt Source file
@param file_name Destination file 
*)
let print_tokens_funtion_html inpt file_name =
  printf "Generate tokens in html file....\n";  
  let lexbuf = (Lexing.from_string (read_file inpt)) in
  try
    TokenPrinter_HTML.print_tokens Scanner.scan_token lexbuf file_name;
  with
    _ -> ()      

(** 
Function that compiles a source file
@param inpt Source file
@param outpt Destination file 
*)
let compile inpt outpt =
  let parse buf = (
  let lexbuf = (Lexing.from_string buf) in
    try
      astree := Parser.parse_program Scanner.scan_token lexbuf
    with
      _ -> ()
  ) in
  let zer = read_file inpt in
  printf "Syntactic Analysis ...\n";
  parse zer;
  (*  Uncomment the following lines to perform the contextual analysis and
      the generation of code *)
  
  if (ErrorReporter.num_errors() == 0 && !astree != Ast.Null_program) then
  begin
     printf "Contextual Analysis ...\n";
     dastree := Checker.check_program !astree;
     (* Uncomment the generation of code *)
     (*
     if (ErrorReporter.num_errors() == 0) then
     begin
        printf "Code Generation ...\n";
        Encoder.encode_program !dastree outpt
     end
     *)           
  end;
  
      
  ErrorReporter.show_errors();
  if (ErrorReporter.num_errors() == 0) then
    printf "Compilation was successful\n"
  else begin
    astree := Ast.Null_program;
    dastree := Ast.Null_program;
    printf "Compilation was unsuccessful\n"
  end

(** Arguments to send by command lines and their different meanings  *)
let specs = [
  ("-o", Arg.String (function s -> output_file:= s),
    "<file> Sets the output file name. Defaults to a.out");
  ("-xe", Arg.String (function s -> xml_error_file:= s),
    "<file> Writes the error list in XML format.");
  ("-xt", Arg.String (function s -> xml_tree_file:= s),
    "<file> Writes the abstract syntax tree in XML format.");
  ("-tpp", Arg.String (function s -> tokens_file_pipe:= s),
    "<file> Writes the tokens in file separated with pipes.");
  ("-tpx", Arg.String (function s -> tokens_file_xml:= s),
    "<file> Writes the tokens in XML file.");
  ("-tph", Arg.String (function s -> tokens_file_html:= s),
    "<file> Writes the tokens in HTML file.");
  ("-xd", Arg.String (function s -> xml_decorated_tree_file:= s),
    "<file> Writes the decorated abstract syntax tree in XML format.")]

(** General message *)
let usage_msg = "\nusage: " ^ Sys.argv.(0) ^ " <source> [options]\n"

(** Main function *)
let main () = 
  printf "********** Triangle Compiler (Caml Version 1.0) **********\n";
  if ((Array.length Sys.argv) < 2) then begin
     printf "%s: not enough parameters." Sys.argv.(0);
     Arg.usage specs usage_msg
  end else
  let input_file = Sys.argv.(1) in
  incr(Arg.current);
  Arg.parse specs (function s -> ()) (usage_msg);
  try
    compile input_file !output_file;
    if ((String.compare !xml_error_file "") != 0) then
      ErrorReporter.write_xml_errors !xml_error_file;
    if ((String.compare !xml_tree_file "") != 0) then
      TreeDrawer.write_xml_tree !astree !xml_tree_file;
    if ((String.compare !tokens_file_pipe "") != 0) then
      print_tokens_funtion_pipe input_file !tokens_file_pipe;
    if ((String.compare !tokens_file_xml "") != 0) then
      print_tokens_funtion_xml input_file !tokens_file_xml;
    if ((String.compare !tokens_file_html "") != 0) then
      print_tokens_funtion_html input_file !tokens_file_html;
    if ((String.compare !xml_decorated_tree_file "") != 0) then
      TreeDrawer.write_xml_tree !dastree !xml_decorated_tree_file
  with 
    Sys_error s -> printf "%s" s

let _ = Printexc.print main ()