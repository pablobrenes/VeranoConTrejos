(* ------------------------------------------------------ *)
(* Abstract Syntax Tree Drawing Library for Caml-Triangle *)
(* Interface file                                         *)
(*                                                        *)
(* (c) 2006 Luis Leopoldo P�rez.                          *)
(* Last modification by:                                  *)
(* Jose Antonio Alpizar Aguilar - 2016201868              *)
(* Pablo Josu� Brenes Jim�nez - 2016250460                *)
(* Luis Jos� Castillo Valverde - 2016094804               *)
(* 22/12/2018                                             *)
(* ------------------------------------------------------ *)


open Ast

(* Writes an entire abstract syntax tree into an XML file *)
val writeXMLTree: astProgram -> string -> unit