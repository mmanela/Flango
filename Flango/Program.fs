module Program

open System
open FlangoLexDefs
open FlangoParserDefs
open Parser
open FlangoTypeChecker
open Models
open Lexer
open StateMonad

let lex input = 
    try    
        let y = Lexer.tokenize flangoSymbolDefinitions input
        printfn "%A" y
    with e -> printf "%s" e.Message
    

let runParse input = 
    try    
        let y = Lexer.tokenize flangoSymbolDefinitions input
        let result = parse y flangoSymbolDefs
        match result with
        | Error msg -> printf "%s" msg
        | Ok (exp, _) -> printf "%A" exp
    with e -> printf "%A" e


let mutable programState = FlangoTypeChecker.initializeProgramState()
let typeCheck input = 
    try    
        let tokens = Lexer.tokenize flangoSymbolDefinitions input
        let res = parse tokens flangoSymbolDefs
        match res with
        | Error msg -> printf "%s" msg
        | Ok (exps, _) -> 
            exps |> List.fold (fun _ exp ->
                match FlangoTypeChecker.typeCheckStatement programState exp with
                | Good (newState, msg, stmWithType) -> 
                    programState <- newState
                    sprintf "%s" msg
                | Bad (state, msg) -> sprintf "%s" msg) "" |> printf "%s"
    with e -> printf "%A\n\n%A" e.Message e.StackTrace



let interpret input = 
    try    
        let tokens = Lexer.tokenize flangoSymbolDefinitions input
        let res = Parser.parse tokens flangoSymbolDefs
        match res with
        | Error msg -> printf "%s" msg
        | Ok (exps, _) -> 
            exps |> List.fold (fun _ exp ->
                match FlangoTypeChecker.typeCheckStatement programState exp with
                | Bad (state, msg) -> sprintf "%s\n" msg
                | Good (newTypeState, msg, stmWithType) -> 
                    let tp = sprintf "Type:: %s\n" msg
                    let (newState, msg) = Interpreter.executeExpression newTypeState stmWithType
                    programState <- newState
                    sprintf "%s\r%s" tp msg) "" |> printf "%s"
            
    with e -> printf "%A\n\n%A" e.Message e.StackTrace


// Change to switch was runs
let mainProgram = interpret

let input = "
fun id x = x
id 5"

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




