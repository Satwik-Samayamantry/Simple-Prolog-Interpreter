
let evaluate () =

    let cin =
      (if Array.length Sys.argv > 1 then
        open_in Sys.argv.(1)
      else
        stdin)
    in
    let _ = print_string "| ?- "
    in
    let _ = flush stdout
    in
    let filename =
      (let lexbuf = Lexing.from_channel cin in
        Parser.filename Lexer.scan lexbuf
    )
    in
    let file_handle = open_in filename in
    print_string "| ?- "
    
let _ = evaluate ()