module Program

open System
open LexDefs
open ParserDefs
open StateMonad

let lex input = 
    try    
        let y = Lexer.tokenize tokenDefinitions input
        printfn "%A" y
    with e -> printf "%s" e.Message
    

let runParse input = 
    try    
        let y = Lexer.tokenize tokenDefinitions input
        let result = Parser.parse y symbolDefs
        match result with
        | Error msg -> printf "%s" msg
        | Ok (exp, _) -> printf "%A" exp
    with e -> printf "%A" e


// Change to switch was runs
let mainProgram = runParse

let input = "5 + 5"

let action = fun _ ->
    Console.Write "\nEnter input: "
    Console.ReadLine()

let readlines = Seq.initInfinite (fun _ -> action())

let run item = if item = ":quit" 
                then Some(item) 
                elif item = String.Empty 
                then None
                else
                    mainProgram item 
                    None

Seq.tryPick run readlines |> ignore
Console.WriteLine "Thanks! Come Again"
