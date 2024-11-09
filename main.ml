open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*Recursive function used for reading multiples lines, joininig these ones with a blank space*)
let rec read_multiline () =
  let line = read_line () in
  if String.contains line ';' then line   (*Return input when double semicolon is found*)
  else line ^ " " ^ read_multiline ()

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let input = read_multiline () in  (*Input from the user*)
      let trimmed_input = String.trim input in  (*Remove blank spaces and newlines*)
      if trimmed_input = ";;" then loop ctx     (*Empty input, only typed ";;" *)
      else(*
        let tm = s token (from_string input) in
        let tyTm = typeof ctx tm in
        (*print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term (eval tm)); *) (* First type and the term*)
        Format.open_hvbox 0;
        print_string ("- : " ^ string_of_ty tyTm ^ " =");
        Format.print_space();
        pretty_printer (eval tm);
        Format.close_box ();
        Format.print_flush(); (*Clean boxes*)
        print_newline();
        loop ctx
        *)
        let c = s token (from_string (input)) in  (* get the command from the input *)
        loop (execute ctx c)
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
