module Interpreter
open FlangoTypeChecker
open Thunker
open Models
open StateMonad

type Interpreter<'a> =  StateMonad<ProgramState,'a>
let interpreter = stateMondad

let bindThunkToName name thunk = 
    interpreter {
        let! state = interpreter.Get()
        let env = state.environment.SetThunk name thunk
        do! interpreter.Put { environment = env; }
    }

let addToSymbolTable name thunk = 
    interpreter {
        let! state = interpreter.Get()
        let env = state.environment.SetThunk name thunk
        do! interpreter.Put { environment = env; }
    }
let getIdentifierName exp =
    interpreter {
        match exp.Exp with 
        | Id name -> return name
        | otherwise -> return! error "Expected Identifier"
     }

let interpretLet name exp = 
    interpreter {
        let expThunk = buildExpressionThunk exp
        let! idName = getIdentifierName name
        do! bindThunkToName idName expThunk
    }
    

let interpretFun name argExps bodyExp scope = 
    interpreter {
        let bodyThunk = buildExpressionThunk bodyExp
        let! idName = getIdentifierName name
        let! argNames = map getIdentifierName argExps
        let lambdaThunk = buildFunction argNames bodyThunk scope
        do! addToSymbolTable idName lambdaThunk
        return None
    }

let interpretExpression (expressionRes:ExpressionResult) = 
    interpreter {
        let! tp = applySubstitutions expressionRes.Type
        match expressionRes.Exp with
        | Let (name, exp) -> 
            do! interpretLet name exp
            return None
        | Fun (name, argExps, bodyExp, scope) -> return! interpretFun name argExps bodyExp scope
        | exp -> return Some (buildExpressionThunk expressionRes, tp)

    }
    
let rec runThunk thunk state tp = 
    match thunk state with
    | ResultThunk(ProgramResult.Ok(res, newState)) -> 
        match tp with
        | TypeOperator("->",_) -> ("", newState)
        | otherwise -> (res |> string, newState)
    | ResultThunk(ProgramResult.Error msg) -> (msg, state)
    | Thunk th -> runThunk th state tp
    | LambdaThunk lt -> ("",state)
    
   
let executeExpression programState statement = 
    match interpretExpression statement programState with
    | Error msg -> (programState, msg)
    | Ok(None, newState) -> (newState, "")
    | Ok (Some (Thunk thunk, tp), newState) -> 
        let (res, resultState) = runThunk thunk newState tp
        (newState, res)
        