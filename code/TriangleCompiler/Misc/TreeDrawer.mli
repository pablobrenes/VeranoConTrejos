(* ------------------------------------------------------ *)
(* Abstract Syntax Tree Drawing Library for Caml-Triangle *)
(* Interface file                                         *)
(*                                                        *)
(* (c) 2006 Luis Leopoldo P�rez.                          *)
(* Last modification: March 12, 2006                      *)
(* ------------------------------------------------------ *)

open Ast

(* Writes an entire abstract syntax tree into an XML file *)
val writeXMLTree: astProgram -> string -> unit