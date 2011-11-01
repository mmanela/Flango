//module FlangoInterpreter
//open FlangoTypeChecker
//open FlangoParser
//open AST
//
//
//type Interpreter<'a> = ProgramState -> ProgramResult<'a>  
//
//let getTypeName typeExp = 
//    match typeExp with
//    | TypeOperator(name,_) -> name
//    | TypeVar name -> string name
//    | Unknown -> "?"
//
//let runThunk thunk execState = 
//    match thunk with
//    | ValueThunk t -> t execState
//    | otherwise -> box ""
//    
//type InterpreterBuilder() = 
//    member b.Return(x) = fun s -> Ok (x, s)
//    member b.Zero() = fun s -> Ok ((),s)
//    member b.Error msg = fun _ -> Error msg
//    member b.Bind(p, rest) = 
//        fun execState -> 
//                 let parserState = p execState in 
//                 match parserState with
//                 | Ok (value,execState2) -> (rest value) execState2
//                 | Error msg -> Error msg
//    
//    member b.Get () = fun execState -> Ok (execState, execState)
//    member b.Put s = fun execState -> Ok ((), s)
//    
//let interpreter = InterpreterBuilder()
//
//let rec map action items  = 
//    interpreter {
//        if List.isEmpty items
//        then return []
//        else 
//            let! res = action items.Head
//            let! rest = map action items.Tail
//            return  res :: rest
//    }
//    
//let error message = 
//    interpreter {
//        let errorMsg = sprintf "Interpreter Error:\n %s" message
//        return! interpreter.Error errorMsg
//    }
//    
//let lookup name = 
//    interpreter {
//        let! state = interpreter.Get()
//        match state.environment.Get name with
//        | (env, Some idVal) -> return idVal.thunk
//        | (env, None) -> return! error (sprintf "Symbol %s was not found" name)
//    
//    }
//    
//let get name state = 
//    (snd(state.environment.Get(name)) |> Option.get).thunk
//    
//let put name thunk state = 
//    { environment = state.environment.SetThunk name thunk}
//    
//let getName binding = 
//    interpreter {
//        match binding.Exp with
//        | Primal(Id name) -> return name
//        | otherwise -> return! error (sprintf "This should never happen")
//    
//    }
//    
//let builtInOperators = 
//    ["+"; "-"; "*"; "/"; "^"; "||"; "&&"; "="; "!="; "<"; "<="; ">"; ">=";]
//
//let resolveType typeExp = 
//    interpreter {
//        let! state = interpreter.Get()
//        match applySubstitutions typeExp state with
//        | Ok (resolved, _) -> return resolved
//        | _ -> return! error "Unable to resolve type"        
//    }
//let resolve typeExp state =
//    match resolveType typeExp state with
//    | Ok (tp, s) -> tp
//    | otherwise -> typeExp
//
//let getIdName exp =
//    interpreter {
//        match exp with 
//        | Primal(Id name) -> return name
//        | otherwise -> return! error "Expected Identifier"
//     }
//
//let getLambda (LambdaThunk lambda) = lambda
//let getValue (ValueThunk value) = value
//let rec buildPrimalThunk value typeExp = 
//    interpreter {
//        match value with 
//        | Int x -> return ValueThunk (fun s -> box x)
//        | Float x -> return ValueThunk (fun s -> box x)
//        | Id x -> 
//            match typeExp with
//            | TypeOperator ("->",_) -> return LambdaThunk ( fun s t -> getLambda (get x s) s t)
//            | otherwise -> return ValueThunk ( fun s -> getValue (get x s) s)
//        | Bool x -> return ValueThunk (fun s -> box x)
//    }
//and buildExpressionThunk exp = 
//    interpreter {
//        let! expType = resolveType exp.Type
//        match exp.Exp with
//        | Primal x -> return! buildPrimalThunk x expType
//        | Call (name, args) -> return! buildCallThunk name args expType
//        | Cond (test, ifThen, ifElse) -> return! buildCondThunk test ifThen ifElse 
//    }
//and buildCondThunk test ifThen ifElse =
//    interpreter {
//       let! state = interpreter.Get()
//       let! testThunk = buildExpressionThunk test
//       if unbox<bool>((getValue testThunk) state) = true
//       then 
//           let! ifT = buildExpressionThunk ifThen
//           return ifT
//       else
//           let! ifE = buildExpressionThunk ifElse
//           return ifE
//    
//    }
//and buildCallThunk exp args typeExp = 
//    interpreter {
//        match exp.Exp with
//        | Primal( Id name) ->
//            if List.exists (fun x -> x = name) builtInOperators
//            then return! buildBuiltInBinaryOperatorThunk name args.[0] args.[1] 
//            else 
//                let! functionThunk = lookup name
//                return! buildApplicationThunk functionThunk args typeExp
//        | otherwise -> 
//            let! functionThunk = buildExpressionThunk exp
//            return! buildApplicationThunk functionThunk args typeExp
//    }
//and buildApplicationThunk functionThunk args typeExp = 
//    interpreter {
//        let! argThunks = map buildExpressionThunk args
//        match typeExp with
//            | TypeOperator ("->",_) -> 
//                return LambdaThunk 
//                    ( fun s t -> 
//                        let (thunk, newState) = applyArgsToThunk functionThunk argThunks s
//                        match thunk with
//                        | ValueThunk vt -> (getLambda (unbox<Thunk>(vt newState))) newState t
//                        | LambdaThunk lt -> (lt newState t))
//            | otherwise -> 
//                return ValueThunk 
//                    ( fun s -> 
//                        let (thunk, newState) = applyArgsToThunk functionThunk argThunks s
//                        (getValue thunk) newState)
//    }
//and applyArgsToThunk funThunk args state =
//        match funThunk with 
//        | ValueThunk value ->
//           if List.isEmpty args then
//               (funThunk, state)
//           else
//               let arg::rest = args
//               let t = ((getLambda (unbox<Thunk>(value state))) state arg)
//               (snd t  , fst t)
//        | LambdaThunk lambda -> 
//            if List.isEmpty args then
//                (funThunk, state)
//            else
//                let arg::rest = args
//                let (newState, nextThunk) = lambda state arg
//                applyArgsToThunk nextThunk rest newState 
//    
//and buildBuiltInBinaryOperatorThunk op exp1 exp2  = 
//    interpreter { 
//        let! thunk1 = buildExpressionThunk exp1
//        let! thunk2 = buildExpressionThunk exp2
//        let! resultType = resolveType exp1.Type
//        
//        match op with 
//        | "+" -> 
//            if resultType = intType 
//            then return ValueThunk (fun s -> box (unbox<int>(runThunk thunk1 s) + unbox<int>(runThunk thunk2 s)))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) + unbox<float>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | "-" -> 
//            if resultType = intType 
//            then return ValueThunk (fun s -> box (unbox<int>(runThunk thunk1 s) - unbox<int>(runThunk thunk2 s)))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) - unbox<float>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | "*" -> 
//            if resultType = intType 
//            then return ValueThunk (fun s -> box (unbox<int>(runThunk thunk1 s) * unbox<int>(runThunk thunk2 s)))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) * unbox<float>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | "/" -> 
//            if resultType = intType 
//            then return ValueThunk (fun s -> box (unbox<int>(runThunk thunk1 s) / unbox<int>(runThunk thunk2 s)))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) / unbox<float>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | "^" -> 
//            if resultType = intType 
//            then return ValueThunk (fun s -> box  (pown (unbox<int>(runThunk thunk1 s))  (unbox<int>(runThunk thunk2 s))))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) ** unbox<float>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | "=" ->
//            if resultType = intType 
//            then return ValueThunk (fun s -> box  ((unbox<int>(runThunk thunk1 s)) =  unbox<int>(runThunk thunk2 s)))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) = unbox<float>(runThunk thunk2 s)))
//            elif resultType = boolType 
//            then return ValueThunk (fun s -> box (unbox<bool>(runThunk thunk1 s) = unbox<bool>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | "!=" ->
//            if resultType = intType 
//            then return ValueThunk (fun s -> box  ((unbox<int>(runThunk thunk1 s)) <>  unbox<int>(runThunk thunk2 s)))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) <> unbox<float>(runThunk thunk2 s)))
//            elif resultType = boolType 
//            then return ValueThunk (fun s -> box (unbox<bool>(runThunk thunk1 s) <> unbox<bool>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | ">" ->
//            if resultType = intType 
//            then return ValueThunk (fun s -> box  ((unbox<int>(runThunk thunk1 s)) >  unbox<int>(runThunk thunk2 s)))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) > unbox<float>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | ">=" ->
//            if resultType = intType 
//            then return ValueThunk (fun s -> box  ((unbox<int>(runThunk thunk1 s)) >=  unbox<int>(runThunk thunk2 s)))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) >= unbox<float>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | "<" ->
//            if resultType = intType 
//            then return ValueThunk (fun s -> box  ((unbox<int>(runThunk thunk1 s)) <  unbox<int>(runThunk thunk2 s)))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) < unbox<float>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | "<=" ->
//            if resultType = intType 
//            then return ValueThunk (fun s -> box  ((unbox<int>(runThunk thunk1 s)) <=  (unbox<int>(runThunk thunk2 s))))
//            elif resultType = floatType 
//            then return ValueThunk (fun s -> box (unbox<float>(runThunk thunk1 s) <= unbox<float>(runThunk thunk2 s)))
//            else return! error "Unknown type"
//        | "||" ->
//            return ValueThunk (fun s -> box  ((unbox<bool>(runThunk thunk1 s)) || unbox<bool>(runThunk thunk2 s)))
//        | "&&" ->
//            return ValueThunk (fun s -> box  ((unbox<bool>(runThunk thunk1 s)) && unbox<bool>(runThunk thunk2 s)))
//    }
//    
//
//     
//let addToSymbolTable name thunk = 
//    interpreter {
//        let! state = interpreter.Get()
//        let env = state.environment.SetThunk name thunk
//        do! interpreter.Put { environment = env; }
//    }
//
//let interpretLet name exp = 
//    interpreter {
//        let! expThunk = buildExpressionThunk exp
//        let! idName = getIdName name.Exp
//        do! addToSymbolTable idName expThunk
//        return None
//    }
//
//let lambdaForThunk argName returnThunk state argThunk =
//    ((put argName argThunk state), returnThunk) 
//
//let interpretFun name argExps bodyExp = 
//    interpreter {
//        let bodyThunk = 
//            ValueThunk (fun s -> 
//                match buildExpressionThunk bodyExp s with
//                | Ok (t, s) -> 
//                    match resolve bodyExp.Type s with
//                    | TypeOperator("->",_) -> box t
//                    | otherwise -> getValue t s
//                )
//        let! idName = getIdName name.Exp
//        let! argNames = map getName argExps
//        let lambdaThunk = List.foldBack (fun argName currentThunk -> LambdaThunk (lambdaForThunk argName currentThunk)) argNames bodyThunk
//        do! addToSymbolTable idName lambdaThunk
//        return None
//    }
//
//let interpretStatement statement = 
//    interpreter {
//        match statement with
//        | Expression exp -> let! thunk = buildExpressionThunk exp
//                            return Some thunk
//        | Let (name, exp) -> return! interpretLet name exp
//        | Fun (name, argExps, bodyExp, scope) -> return! interpretFun name argExps bodyExp
//    }
//    
//
//let thunkToString thunk programState = 
//    runThunk thunk programState |> string
//    
//let executeStatement programState statement = 
//    match interpretStatement statement.Stm programState with
//    | Error msg -> (programState, msg)
//    | Ok ((Some thunk), newState) -> (newState, thunkToString thunk newState )
//    | Ok (None, newState) -> (newState, "")