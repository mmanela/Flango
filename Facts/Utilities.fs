module Utilities

open System
open Lexer
open FlangoLexDefs
open FlangoParserDefs
open Parser
open Interpreter
open FlangoTypeChecker
open Models
open StateMonad

let mutable programState = FlangoTypeChecker.initializeProgramState()
let typeCheck input = 
    try    
        programState <- FlangoTypeChecker.initializeProgramState()
        let tokens = Lexer.tokenize flangoSymbolDefinitions input
        let res = parse tokens flangoSymbolDefs
        match res with
        | Error msg -> None
        | Ok (exps, _) -> 
            exps |> List.fold (fun _ exp ->
                match FlangoTypeChecker.typeCheckStatement programState exp with
                | Good (newTypeState, msg, stmWithType) -> 
                    programState <- newTypeState
                    Some (newTypeState, stmWithType.Type)
                | Bad (state, msg) -> None) None
    with e -> None

let interpret input = 
    try    
        programState <- FlangoTypeChecker.initializeProgramState()
        let tokens = Lexer.tokenize flangoSymbolDefinitions input
        let res = Parser.parse tokens flangoSymbolDefs
        match res with
        | Error msg -> None
        | Ok (exps, _) -> 
            exps |> List.fold (fun _ exp ->
                match FlangoTypeChecker.typeCheckStatement programState exp with
                | Bad (state, msg) -> None
                | Good (newTypeState, msg, stmWithType) -> 
                    let (newState, msg) = Interpreter.executeExpression newTypeState stmWithType
                    programState <- newState
                    Some (newState, msg)) None
    with e -> None

let isTypeOperator tp =
    match tp with 
    | TypeOperator (name,args) -> true
    | otherwise -> false
    
let getOperatorName tp =
    match tp with 
    | TypeOperator (name,args) -> name
    | otherwise -> null
    
let getOperatorArgs tp =
    match tp with 
    | TypeOperator (name,args) -> args
    | otherwise -> []
    
let isTypeVariable tp =
    match tp with 
    | TypeVar name -> true
    | otherwise -> false

let getVariableName tp =
    match tp with 
    | TypeVar name -> name
    | otherwise -> -1