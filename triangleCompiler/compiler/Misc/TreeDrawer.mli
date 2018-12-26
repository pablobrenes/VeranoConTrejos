(* ------------------------------------------------------ *)
(* Abstract Syntax Tree Drawing Library for Caml-Triangle *)
(* Interface file                                         *)
(*                                                        *)
(* (c) 2006 Luis Leopoldo Pérez.                          *)
(* Last modification by:                                  *)
(* Jose Antonio Alpizar Aguilar - 2016201868              *)
(* Pablo Josué Brenes Jiménez - 2016250460                *)
(* Luis José Castillo Valverde - 2016094804               *)
(* 22/12/2018                                             *)
(* ------------------------------------------------------ *)


open Ast

(* Writes an entire abstract syntax tree into an XML file *)
val writeXMLTree: astProgram -> string -> unit