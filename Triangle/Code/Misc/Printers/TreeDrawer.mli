(**
This program was originally written by Luis Leopoldo P�rez on April 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Abstract Syntax Tree Drawing Library for Caml-Triangle
Interface file                                  

@author Luis Leopoldo P�rez
@author Jose Antonio Alp�zar Aguilar
@author Pablo Josu� Brenes Jimenes
@author Luis Jos� Castillo Valverde
*)

open Ast

(** Writes an entire abstract syntax tree into an XML file *)
val write_xml_tree: ast_program -> string -> unit