(**
This program was originally written by Luis Leopoldo P�rez on May 26, 2006.
This program was reviewed, repaired, completed, verified, and validated by 
students of ITCR in January 2019.
Runtime Entities Definition for Caml-Triangle
Interface file                                 

@author Luis Leopoldo P�rez
@author Jose Antonio Alp�zar Aguilar
@author Pablo Josu� Brenes Jimenes
@author Luis Jos� Castillo Valverde
*)


type object_address = { level: int; displacement: int }

                                           
type runtime_entity = 
  | Null_runtime_entity     
  | Known_value of int * int (* size,value *)
  | Unknown_value of int * object_address (* size *)
  | Known_address of int * object_address (* size *)
  | Unknown_address of int * object_address (* size *)
  | Known_routine of int * object_address (* size *)
  | Unknown_routine of int * object_address (* size *)
  | Primitive_routine of int * int (* size, displacement *)
  | Equality_routine of int * int (* size, displacement *)
  | Field of int * int (* size, field_offset *)
  | Type_representation of int (* size *)