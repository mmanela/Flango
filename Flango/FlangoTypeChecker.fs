module FlangoTypeChecker

open Models
open System
open StateMonad


type TypeComparison = OccursIn | Same | Different

type TypeChecker<'a> =  StateMonad<ProgramState,'a>
let typeChecker = stateMondad

let error message = 
    typeChecker {
        let errorMsg = sprintf "Type Checking Error:\n %s" message
        return! typeChecker.Error errorMsg
    }
    

let expressionResultWithType exp typeExp =
    { Exp = exp.Exp; Pos = exp.Pos; Type = typeExp}  

let beginScope () = 
    typeChecker {
        let! state = typeChecker.Get()
        let env = state.environment.NewScope()
        do! typeChecker.Put { environment = env; }
    }
    
let endScope () = 
    typeChecker {
        let! state = typeChecker.Get()
        let (env, scope) = state.environment.PopScope()
        do! typeChecker.Put { environment = env; }
        return scope
    }
    
let newTypeVar () = 
    typeChecker {
        let! state = typeChecker.Get()
        let (env, newTypeNum) = state.environment.NewTypeVar()
        do! typeChecker.Put { environment = env; }
        return TypeVar newTypeNum
    }


let getName binding msg = 
    typeChecker {
        match binding.Exp with
        | Id name -> return name
        | otherwise -> return! error (sprintf "%s\nAt Line:%d Column:%d" msg binding.Pos.Line binding.Pos.Column)
    
    }

   
let updateSubstitution name typeExp = 
    typeChecker {
        let! state = typeChecker.Get()
        let env = state.environment.AddTypeSub name typeExp
        do! typeChecker.Put { environment = env }
    }
    
let rec applySubstitutions typeExp = 
    typeChecker {
        let! state = typeChecker.Get()
        let (env, tp) = state.environment.ApplySubs typeExp
        do! typeChecker.Put { environment = env }
        return tp
    }


let updateEnvironment name typeExp =
    typeChecker {
        let! state = typeChecker.Get()
        do! typeChecker.Put {environment =  state.environment.SetType name typeExp; }
    }
    
let addToEnvironment name typeExp =
    typeChecker {
        let! state = typeChecker.Get()
        if state.environment.Contains name
        then return! error (sprintf "Identifier %s is already declared" name)
        else 
            do! updateEnvironment name typeExp
    }
    
let getFromEnvironment name =
    typeChecker {
        let! state = typeChecker.Get()
        match state.environment.Get name with
        | (env, Some idVal) -> 
            do! typeChecker.Put { environment = env}
            return fst idVal
        | otherwise ->return! error (sprintf "Identifier %s is not defined" name)
    }
    

let rec compareTypes (TypeVar typeName) typeExp = 
    typeChecker {
        let! exp = applySubstitutions typeExp
        match exp with
        | TypeVar tn -> return (if tn = typeName then Same else Different)
        | TypeOperator (n, args) -> 
            let! occurs = map (compareTypes (TypeVar typeName)) args
            let res = if List.exists (fun x -> x <> Different) occurs then OccursIn else Different
            return res
    }

let addNewTypeToEnvironment expResult =
    typeChecker { 
        let! name = getName expResult "Expected identifier"
        let! newType = newTypeVar ()
        do!  addToEnvironment name newType
        return expressionResultWithType expResult newType
     }

let rec unifyArgs args1 args2  = 
    typeChecker {
        if List.isEmpty args1 && List.isEmpty args2
        then return ()
        else
            if List.isEmpty args1 || List.isEmpty args2
            then return! error "Number of type arguments do not match"
            else 
                do! unifyType args1.Head args2.Head
                do! unifyArgs args1.Tail args2.Tail
    }
and unifyType typeExp1 typeExp2 =
    typeChecker {
        let! exp1 = applySubstitutions typeExp1
        let! exp2 = applySubstitutions typeExp2
        
        match exp1 with
        | TypeVar name -> 
            let! compare = compareTypes (TypeVar name) exp2
            match compare with 
            | OccursIn -> 
                return! error "Resolution of type would result in infinite type"
            | Different ->
                do! updateSubstitution name exp2
            | Same -> return ()
                
        | TypeOperator (name, args) ->
            match exp2 with
            | TypeVar name -> return! unifyType exp2 exp1
            | TypeOperator (name2, args2) ->
                if name <> name2 
                then 
                    return! error (sprintf "Cannot resolve type operator %s with type operator %s" name name2)
                else
                    do! unifyArgs args args2
    }

   

let rec analyzeExp exp =
    typeChecker {
        match exp.Exp with
        | Id name -> 
            let! typeExp = getFromEnvironment name
            return expressionResultWithType exp typeExp
        | Bool _ -> return expressionResultWithType exp boolType
        | Int _ -> return expressionResultWithType exp intType
        | Float _ -> return expressionResultWithType exp floatType
        | Cond (test, ifTrue, ifFalse) -> 
            let! res =  analyzeCondExp test ifTrue ifFalse
            return {Exp = fst res; Pos = exp.Pos; Type = snd res}
        | Call (nameExp, argsList) -> 
            let! res = analyzeCallExp nameExp argsList
            return {Exp = fst res; Pos = exp.Pos; Type = snd res}
        | Let (binding, exp) -> 
            let! res =  analyzeLet binding exp
            return {Exp = fst res; Pos = exp.Pos; Type = snd res}
        | Fun (binding, args, body, scope) -> 
            let! res = analyzeFunction binding args body
            return {Exp = fst res; Pos = exp.Pos; Type = snd res}
    }
and applyArgsToFunction funType (argTypes: TypeExp list) = 
    typeChecker {
        if argTypes.IsEmpty then return funType
        else
            match funType with         
            | TypeOperator ("->",funTypeArgs) -> 
                  // unify the known function type with the call site arguments
                  do! unifyType funTypeArgs.[0] argTypes.Head
                  return! applyArgsToFunction funTypeArgs.[1] argTypes.Tail    
            | TypeVar typeName ->
                // The function type is unknown so we assign it an inferred type from its call site arguments
                let! returnType = newTypeVar()
                let inferredFunType = List.foldBack functionType argTypes returnType
                do! unifyType (TypeVar typeName) inferredFunType
                return! applyArgsToFunction inferredFunType argTypes
            | otherwise -> 
                return! error "Can only apply a value to a function"
    }
and analyzeCallExp callee callArgList =    
    typeChecker {
        let! funNode = analyzeExp callee
        let! callArgsWithType = map analyzeExp callArgList        
        let! resultType = applyArgsToFunction funNode.Type (List.map (fun (x:ExpressionResult) -> x.Type) callArgsWithType)
        return (Call (funNode, callArgsWithType), resultType)
    }
and analyzeCondExp test ifTrue ifFalse = 
    typeChecker {
        let! testExp = analyzeExp test
        do! unifyType testExp.Type boolType
        let! thenExp = analyzeExp ifTrue
        let! elseExp = analyzeExp ifFalse
        do! unifyType thenExp.Type elseExp.Type
        return (Cond(testExp, thenExp, elseExp), thenExp.Type)
    }

and analyzeLet binding exp = 
    typeChecker {
        let! name = getName binding "Invalid let binding"
        let! bodyExp = analyzeExp exp
        do! addToEnvironment name bodyExp.Type
        let nameWithType = expressionResultWithType binding bodyExp.Type
        return (Let(nameWithType, bodyExp), bodyExp.Type)
    }   
and analyzeFunction binding args body = 
    typeChecker {
        let! name = getName binding "Invalid function binding"
        
        do! beginScope()
        let! argsWithType = map addNewTypeToEnvironment args 
        let! bodyWithType = analyzeExp body
        let! funType = applySubstitutions (List.foldBack functionType (List.map (fun (x:ExpressionResult) -> x.Type) argsWithType)  bodyWithType.Type)
        let! scope = endScope()
        
        do! addToEnvironment name funType
        let nameWithType = expressionResultWithType binding funType
        return (Fun(nameWithType, argsWithType, bodyWithType, scope), funType)
    }


let analyze exp = 
    typeChecker {
        let! res = analyzeExp exp
        let! updatedType = applySubstitutions res.Type
        return {Exp= res.Exp; Pos=res.Pos; Type=updatedType}
    }

let initializeBuiltInFunctions () =
    typeChecker {
        let typeVar = intType
        do! addToEnvironment "+" (functionType typeVar (functionType typeVar typeVar))
        do! addToEnvironment "-" (functionType typeVar (functionType typeVar typeVar))
        do! addToEnvironment "*" (functionType typeVar (functionType typeVar typeVar))
        do! addToEnvironment "/" (functionType typeVar (functionType typeVar typeVar))
        do! addToEnvironment "^" (functionType typeVar (functionType typeVar typeVar))
        
        do! addToEnvironment "<" (functionType typeVar (functionType typeVar boolType))
        do! addToEnvironment "<=" (functionType typeVar (functionType typeVar boolType))
        do! addToEnvironment ">" (functionType typeVar (functionType typeVar boolType))
        do! addToEnvironment ">=" (functionType typeVar (functionType typeVar boolType))
        
        let! typeVar = newTypeVar()
        do! addToEnvironment "&&" (functionType typeVar (functionType typeVar boolType))
        let! typeVar = newTypeVar()
        do! addToEnvironment "||" (functionType typeVar (functionType typeVar boolType))
        let! typeVar = newTypeVar()
        do! addToEnvironment "!=" (functionType typeVar (functionType typeVar boolType))
        let! typeVar = newTypeVar()
        do! addToEnvironment "=" (functionType typeVar (functionType typeVar boolType))
    }




    
let initialProgramState = { environment  = Environment.Empty.NewScope() }

let initializeProgramState () = 
    match initializeBuiltInFunctions () initialProgramState with
    | Error msg -> raise (new Exception("Fatal error in initialization of program state") )
    | Ok (typeExp, programState) -> programState


type TypeCheckerResult = 
    | Good of ProgramState * string * ExpressionResult
    | Bad of ProgramState * string
    
let typeCheckStatement programState expression = 
    match analyze expression programState with
    | Error msg -> Bad (programState, msg)
    | Ok (expWithType, currentState) -> Good (currentState, expWithType.Type.ToString(), expWithType)