open TokenPrinter
open Scanner
open Printf

let main () =
    let chan = open_in "test.tri" in
    let len = (in_channel_length chan) in
    let buf = String.make len ' ' in
    let _ = input chan buf 0 len in
        close_in_noerr chan;
        buf;

    (* Si el buffer es consumido se debe volver a crear con Lexing.from_string *)
	let lexbuf = (Lexing.from_string buf) in
	TokenPrinter.printTokens Scanner.scanToken lexbuf;

let _ = Printexc.print main ()