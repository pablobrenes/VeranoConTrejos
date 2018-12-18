(* ------------------------------------- *)
(* Contextual Analyzer for Caml-Triangle *)
(* Interface file                        *)
(*                                       *)
(* (c) 2006 Luis Leopoldo P�rez.         *)
(* Last modification: March 12, 2006     *)
(* ------------------------------------- *)

open Parser
open Ast

(* Semantically checks the program, returning a "decorated" abstract syntax tree *)
val checkProgram: Ast.astProgram -> Ast.astProgram