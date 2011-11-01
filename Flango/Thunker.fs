module Thunker
open FlangoTypeChecker

open Models

type ThunkerBuilder() = 
    member b.Return(x) = Thunk (fun state -> ResultThunk( Ok (x, state)))
    member b.ReturnFrom(x) = x
    member b.Zero() = Thunk (fun state -> ResultThunk (Ok ((), state)))
    member b.Error msg = Thunk (fun st -> ResultThunk(Error msg))
    member b.Bind(ex, rest) =                 
                 match ex with
                 | NoThunk -> ResultThunk (Error "No thunk")
                 | ResultThunk res -> 
                    match res with
                    | Ok (value,state2) -> 
                            match rest value with
                            | Thunk th -> Thunk( fun _ -> th state2)
                            | LambdaThunk lt -> LambdaThunk (fun _ arg -> lt state2 arg)
                     | Error msg -> ResultThunk (Error msg)
                 | Thunk th ->
                    Thunk( fun state ->
                          let res = th state in 
                          b.Bind( res, rest))
                 | LambdaThunk lt ->
                    LambdaThunk( fun state arg ->
                             let res = lt state arg in 
                             b.Bind( res, rest))
    
    member b.GetArg () = LambdaThunk (fun state arg -> ResultThunk(Ok (arg, state)))
    member b.Get () = Thunk (fun state -> ResultThunk(Ok (state, state)))
    member b.Put s = Thunk (fun state -> ResultThunk(Ok ((), s)))
    
let thunker = ThunkerBuilder()

let pushScope scope = 
    thunker {
        let! state = thunker.Get()
        let env = state.environment.PushScope scope
        do! thunker.Put { environment = env; }
    }
let popScope () = 
    thunker {
        let! state = thunker.Get()
        let (env,scope)= state.environment.PopScope ()
        do! thunker.Put { environment = env; }
    }
    
let addThunkToScope name thunk = 
    thunker {
        let! state = thunker.Get()
        let env = state.environment.SetThunk name thunk
        do! thunker.Put { environment = env; }
    }

let freshenType typeExp = 
    thunker {
        let! state = thunker.Get()
        let (env, tp) = state.environment.ApplySubs typeExp
        do! thunker.Put { environment = env }
        return tp
    }

let getThunkFromScope name =
    thunker {
        let! state = thunker.Get()
        match state.environment.Get name with
        | (env, Some idVal) -> 
            do! thunker.Put { environment = env}
            return! (snd idVal)
        | otherwise ->return! thunker.Error("Identifier not found")
    }
let buildPrimalThunk value typeExp = 
    thunker {
        match value with 
        | Int x -> return box x
        | Float x -> return box x
        | Bool x -> return box x
        | Id x -> 
            let thunk = getThunkFromScope x
            return! thunk
    }
   
let builtInOperators = ["+"; "-"; "*"; "/"; "^"; "||"; "&&"; "="; "!="; "<"; "<="; ">"; ">=";]

let rec buildExpressionThunk (exp:ExpressionResult) = 
    thunker {
        let! expType = freshenType exp.Type
        match exp.Exp with
        | Cond (test, ifThen, ifElse) -> return! buildCondThunk test ifThen ifElse 
        | Call (name, args) -> return! buildCallThunk name args expType
        |  x -> return! buildPrimalThunk x expType
        
    }
and buildCondThunk test ifThen ifElse =
    thunker {
       let! state = thunker.Get()
       let! conditionValue = buildExpressionThunk test
       if unbox<bool>(conditionValue) = true
       then 
           let! ifT = buildExpressionThunk ifThen
           return ifT
       else
           let! ifE = buildExpressionThunk ifElse
           return ifE
    }
and buildBuiltInBinaryOperatorThunk op exp1 exp2  = 
    thunker { 
        let! thunk1 = buildExpressionThunk exp1
        let! thunk2 = buildExpressionThunk exp2
        let! resultType = freshenType exp1.Type
        let typeName = match resultType with
                        | TypeOperator(name, _) -> name
                        | otherwise -> ""
        match (op, typeName) with 
        | "+", "Integer"  -> return unbox<int>(thunk1) + unbox<int>(thunk2) |> box
        | "+", "Float"    -> return unbox<float>(thunk1) + unbox<float>(thunk2) |> box
        | "-", "Integer"  -> return unbox<int>(thunk1) - unbox<int>(thunk2) |> box
        | "-", "Float"    -> return unbox<float>(thunk1) - unbox<float>(thunk2) |> box
        | "*", "Integer"  -> return unbox<int>(thunk1) * unbox<int>(thunk2) |> box
        | "*", "Float"    -> return unbox<float>(thunk1) * unbox<float>(thunk2) |> box
        | "/", "Integer"  -> return unbox<int>(thunk1) / unbox<int>(thunk2) |> box
        | "/", "Float"    -> return unbox<float>(thunk1) / unbox<float>(thunk2) |> box
        | "^", "Integer"  -> return pown (unbox<int>(thunk1))  (unbox<int>(thunk2)) |> box
        | "^", "Float"    -> return unbox<float>(thunk1) ** unbox<float>(thunk2) |> box
        | "=", "Integer"  -> return unbox<int>(thunk1) = unbox<int>(thunk2) |> box
        | "=", "Float"    -> return unbox<float>(thunk1) = unbox<float>(thunk2) |> box
        | "=", "Boolean"  -> return unbox<bool>(thunk1) = unbox<bool>(thunk2) |> box
        | "!=", "Integer"  -> return unbox<int>(thunk1) <> unbox<int>(thunk2) |> box
        | "!=", "Float"    -> return unbox<float>(thunk1) <> unbox<float>(thunk2) |> box
        | "!=", "Boolean"  -> return unbox<bool>(thunk1) <> unbox<bool>(thunk2) |> box
        | ">=", "Integer"  -> return unbox<int>(thunk1) >= unbox<int>(thunk2) |> box
        | ">=", "Float"    -> return unbox<float>(thunk1) >= unbox<float>(thunk2) |> box
        | ">=", "Boolean"  -> return unbox<bool>(thunk1) >= unbox<bool>(thunk2) |> box
        | ">", "Integer"  -> return unbox<int>(thunk1) > unbox<int>(thunk2) |> box
        | ">", "Float"    -> return unbox<float>(thunk1) > unbox<float>(thunk2) |> box
        | ">", "Boolean"  -> return unbox<bool>(thunk1) > unbox<bool>(thunk2) |> box
        | "<=", "Integer"  -> return unbox<int>(thunk1) <= unbox<int>(thunk2) |> box
        | "<=", "Float"    -> return unbox<float>(thunk1) <= unbox<float>(thunk2) |> box
        | "<=", "Boolean"  -> return unbox<bool>(thunk1) <= unbox<bool>(thunk2) |> box
        | "<", "Integer"  -> return unbox<int>(thunk1) < unbox<int>(thunk2) |> box
        | "<", "Float"    -> return unbox<float>(thunk1) < unbox<float>(thunk2) |> box
        | "<", "Boolean"  -> return unbox<bool>(thunk1) < unbox<bool>(thunk2) |> box
        | _,_ -> return! thunker.Error "Unknown type"

    }
and getCallArgumentValueThunk outerState expression =
    thunker {
        let! innerState = thunker.Get()
        do! thunker.Put outerState
        match expression.Exp with
        | Id name -> 
            let! thunk = getThunkFromScope name
            do! thunker.Put innerState
            return thunk
        | otherwise -> 
            let! thunk =  buildExpressionThunk expression
            do! thunker.Put innerState
            return thunk
    }
and applyArgsToThunk thunk args = 
    thunker {
        if List.isEmpty args
        then return! thunk
        else 
            match thunk with
            | LambdaThunk lt ->
                let! state = thunker.Get()
                let argThunk = getCallArgumentValueThunk state args.[0]
                let thunk2= lt state argThunk
                return! applyArgsToThunk thunk2 (List.tail args)
            | Thunk th ->
                let! state = thunker.Get()
                let thunk2 = th state
                return! applyArgsToThunk thunk2 args
            | ResultThunk rs -> 
                return! thunker.Error "Here again, sadly"
    
    }
and buildCallThunk callee args expType = 
    thunker {
        match callee.Exp with
        | Id name ->
            if List.exists (fun x -> x = name) builtInOperators
            then return! buildBuiltInBinaryOperatorThunk name args.[0] args.[1]
            else
                let thunk = getThunkFromScope name
                return! applyArgsToThunk thunk args
        | otherwise -> return! thunker.Error "NOT YET" 
    }
    

let buildLambdaThunk argName bodyThunk = 
    thunker {
        let! state = thunker.Get()
        let! argValue = thunker.GetArg()
        do! addThunkToScope argName argValue
        return! bodyThunk
    }
    
let rec buildFunctionArgs argNames restThunk =
    thunker {
        if List.isEmpty argNames
        then 
            return! restThunk
        else
            let! state = thunker.Get()
            let thunk = buildLambdaThunk argNames.[0] restThunk
            return! buildFunctionArgs (List.tail argNames) thunk
    }
    
let rec buildFunction argNames restThunk scope =
    thunker {
       do! pushScope scope
       let! state = thunker.Get()
       let! thunk =  buildFunctionArgs (List.rev argNames) restThunk
       do! popScope ()
       return thunk
    }