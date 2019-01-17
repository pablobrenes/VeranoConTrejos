(**
This program was originally written by Luis Leopoldo P�rez on March 12, 2006.
This program was reviewed, repaired, completed, verified, and validated by
students of ITCR in January 2019.
Error Reporting Library for Caml-Triangle
Interface file                                  

@author Luis Leopoldo P�rez
@author Jose Antonio Alp�zar Aguilar
@author Pablo Josu� Brenes Jimenes
@author Luis Jos� Castillo Valverde
*)


(** Returns the number of errors found *)
val num_errors: unit -> int

(** Reports an error, adding it to the error list *)
val report_error: string -> Lexing.position -> unit

(** Reports a restriction, *)
val report_restriction: string -> unit

(** Prints the entire error list *)
val show_errors: unit -> unit

(** Writes the error list in the specified XML file *)
val write_xml_errors: string -> unit