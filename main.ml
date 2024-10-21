open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*Leer varias lineas uniendo con un espacio en blanco*)
let rec read_multiline () =
  let line = read_line () in
  if String.contains line ';' then line
  else line ^ " " ^ read_multiline ()

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let input = read_multiline () in  (*Cambiamos a lectura de múltiples líneas*)
      let tm = s token (from_string input) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
