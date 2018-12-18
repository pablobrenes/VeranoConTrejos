(* ------------------------------------ *)
(* Program entrypoint for Caml-Triangle *)
(* Implementation file                  *)
(*                                      *)
(* (c) 2006 Luis Leopoldo P�rez.        *)
(* Last modification: March 12, 2006    *)
(* ------------------------------------ *)

open Ast
open Parser
open Scanner
open Checker
open Encoder
open ErrorReporter
open Printf
open Lexing
open TokenPrinter

let outputFile = ref "a.out"
let xmlErrorFile = ref ""
let xmlTreeFile = ref ""
let xmlDTreeFile = ref ""
let xmlTableFile = ref ""
let tokensFile = ref ""
let doUsage = ref false
let astree = ref NullProgram
let dastree = ref NullProgram

(* Compiles the program! *)

let readfile file = (
 (* Esto es nuevo *)
let ic = open_in file in
let buf = Buffer.create (in_channel_length ic) in
try
  while true do
    let line = input_line ic in
    Buffer.add_string buf line;
    Buffer.add_char buf '\n';
  done; assert false
with End_of_file ->
  Buffer.contents buf 
  (* Esto es nuevo *)
  )

let printTokensFuntion inpt fileName =
    printf "Generate tokens....\n";
    
    let lexbuf = (Lexing.from_string (readfile inpt)) in
      try
        TokenPrinter.printTokens Scanner.scanToken lexbuf fileName;
      with _ -> ()


let compile inpt outpt =
    let parse buf = (
        let lexbuf = (Lexing.from_string buf) in
            try
               astree := Parser.parseProgram Scanner.scanToken lexbuf
            with _ -> ()) in
    let zer = readfile inpt in
        printf "Syntactic Analysis ...\n";
        parse zer;

        (*Para que no haga mas que la evaluacion sintactica*)
        
        ErrorReporter.showErrors();
        if (ErrorReporter.numErrors() == 0) then
           printf "Compilation was successful\n"
        else
        begin
           astree := NullProgram;
           dastree := NullProgram;
           printf "Compilation was unsuccessful\n"
        end

let specs = [
    ("-o", Arg.String (function s -> outputFile:= s), "<file> Sets the output file name. Defaults to a.out");
    ("-xe", Arg.String (function s -> xmlErrorFile:= s), "<file> Writes the error list in XML format.");
    ("-xt", Arg.String (function s -> xmlTreeFile:= s), "<file> Writes the abstract syntax tree in XML format.");
    ("-xd", Arg.String (function s -> xmlDTreeFile:= s), "<file> Writes the decorated abstract syntax tree in XML format.");
    ("-xi", Arg.String (function s -> xmlTableFile:= s), "<file> Writes the identification table details in XML format.");
    ("-tp", Arg.String (function s -> tokensFile:= s), "<file> Writes the tokens.")]

let usage_msg = "\nusage: " ^ Sys.argv.(0) ^ " <source> [options]\n"

let main () = 
    printf "********** Triangle Compiler (Caml Version 1.0) **********\n";
    if ((Array.length Sys.argv) < 2) then
    begin
       printf "%s: not enough parameters." Sys.argv.(0);
       Arg.usage specs usage_msg
    end
    else
    let inputFile = Sys.argv.(1) in
        incr(Arg.current);
        Arg.parse specs (function s -> ()) (usage_msg);
        try
           compile inputFile !outputFile;
           if ((String.compare !xmlErrorFile "") != 0) then
              ErrorReporter.writeXMLErrors !xmlErrorFile;

           if ((String.compare !xmlTreeFile "") != 0) then
              TreeDrawer.writeXMLTree !astree !xmlTreeFile;

           if ((String.compare !xmlDTreeFile "") != 0) then
              TreeDrawer.writeXMLTree !dastree !xmlDTreeFile;
              
           if ((String.compare !xmlTableFile "") != 0) then
              Encoder.writeXMLTable !xmlTableFile;
           
           if ((String.compare !tokensFile "") != 0) then
              printTokensFuntion inputFile !tokensFile
              
        with Sys_error s -> printf "%s" s


let _ = Printexc.print main ()