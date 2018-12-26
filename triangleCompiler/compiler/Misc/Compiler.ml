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
open TokenPrinter_Pipe
open TokenPrinter_XML

let outputFile = ref "a.out"
let xmlErrorFile = ref ""
let xmlTreeFile = ref ""
let xmlDTreeFile = ref ""
let xmlTableFile = ref ""
let tokensFile_Pipe = ref ""
let tokensFile_XML = ref ""
let tokensFile_HTML = ref ""
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


let printTokensFuntion_Pipe inpt fileName =
    printf "Generate tokens with pipes....\n";
    
    let lexbuf = (Lexing.from_string (readfile inpt)) in
      try
        TokenPrinter_Pipe.printTokens Scanner.scanToken lexbuf fileName;
      with _ -> ()

let printTokensFuntion_XML inpt fileName =
    printf "Generate tokens in xml file....\n";
    
    let lexbuf = (Lexing.from_string (readfile inpt)) in
      try
        TokenPrinter_XML.printTokens Scanner.scanToken lexbuf fileName;
      with _ -> ()

let printTokensFuntion_HTML inpt fileName =
    printf "Generate tokens in html file....\n";
    
    let lexbuf = (Lexing.from_string (readfile inpt)) in
      try
        TokenPrinter_HTML.printTokens Scanner.scanToken lexbuf fileName;
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
    ("-tpp", Arg.String (function s -> tokensFile_Pipe:= s), "<file> Writes the tokens in file separated with pipes.");
    ("-tpx", Arg.String (function s -> tokensFile_XML:= s), "<file> Writes the tokens in XML file.");
    ("-tph", Arg.String (function s -> tokensFile_HTML:= s), "<file> Writes the tokens in HTML file.")]

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
           
           if ((String.compare !tokensFile_Pipe "") != 0) then
              printTokensFuntion_Pipe inputFile !tokensFile_Pipe;

           if ((String.compare !tokensFile_XML "") != 0) then
              printTokensFuntion_XML inputFile !tokensFile_XML;

           if ((String.compare !tokensFile_HTML "") != 0) then
              printTokensFuntion_HTML inputFile !tokensFile_HTML
              
        with Sys_error s -> printf "%s" s


let _ = Printexc.print main ()