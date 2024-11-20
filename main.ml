open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*Recursive function used for reading multiples lines, joininig these ones with a blank space*)
let rec read_multiline () =
  let line = read_line () in              (*Read line*)
  if String.contains line ';' then line   (*Return input when a colon is found, enough with just one*)
  else line ^ " " ^ read_multiline ()     (*Keep reading*)

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let input = read_multiline () in          (*Input from user*)
      let trimmed_input = String.trim input in  (*Remove blank spaces and newlines*)
      if trimmed_input = ";;" then loop ctx     (*Empty input, only typed ";;"*)
      else
        let c = s token (from_string (input)) in  (*Get command from input *)
        loop (execute ctx c)                      (*Execute new command*)
    with  (*Captured errors*)
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
    loop emptyctx (*Start loop with an empty context*)
  ;;

top_level_loop ()
