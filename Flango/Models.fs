module Models

open System

let numToLetter num = 
    let letters = ['a'..'z']
    let unfoldStep num  = 
        if num > 26 
        then Some (letters.[25], num - 26)
        elif num > 0
        then Some(letters.[num - 1], 0)
        else None
    "'" + Seq.fold (fun st v -> string v + st) "" (Seq.unfold unfoldStep num)
                             
   
    
type TypeName = int
type TypeExp = 
    | Unknown
    | TypeVar of TypeName
    | TypeOperator of string * TypeExp list
    
    override typeExp.ToString() =
        let rec typeToString typeExp vars =  
            match typeExp with
            | TypeVar num -> 
                match Map.tryFind num vars with
                | Some name ->
                    (numToLetter name, vars)
                | None -> 
                    let name = vars.Count + 1
                    (numToLetter name, Map.add num name vars)
            | TypeOperator (name, []) -> (name, vars) 
            | TypeOperator ("->", args) -> 
                let (str1, vars1) = printFunctionType args.[0] vars
                let (str2, vars2) = printFunctionType args.[1] vars1
                (str1 + "->" + str2, vars2)
            | otherwise -> ("?", vars)  
            
        and printFunctionType typeExp vars = 
            match typeExp with
            | TypeOperator ("->", args) -> 
                let (str1, vars1) = typeToString typeExp vars
                ("(" + str1 +  ")", vars1)
            | otherwise -> typeToString typeExp vars
         
        typeToString typeExp Map.empty |> fst

let boolType = TypeOperator ("Boolean",[])
let intType = TypeOperator  ("Integer",[])
let floatType = TypeOperator ("Float",[])
let functionType typeOfArg typeOfRest = TypeOperator ("->", [typeOfArg; typeOfRest])

type ErrorState = string 


type ProgramResult<'a,'st> = 
    | Ok of  'a * 'st
    | Error of ErrorState
 
type ProgramState = 
    {
        environment : Environment;
    }
and Thunk<'a> = 
    | Thunk of (ProgramState->  Thunk<'a>)
    | LambdaThunk of (ProgramState -> Thunk<obj> ->  Thunk<'a>)
    | ResultThunk of ProgramResult<'a, ProgramState>
    | NoThunk

and Scope =
    { 
        env : Map<string,TypeExp * Thunk<obj>>;
    }
    static member Empty with get() = { env = Map.empty; }
    member x.Get name = x.env.[name]
    member x.Contains name = x.env.ContainsKey name
    member x.TryGet name = Map.tryFind name x.env
    member x.Set name value = { env = x.env.Add(name, value); }
    
    member x.SetType name typeExp = 
        if x.Contains name 
        then
            let idVal = x.Get name
            x.Set name (typeExp, snd idVal)
        else
            x.Set name (typeExp, NoThunk)
    // Set the thunk value of a scope name    
    member x.SetThunk name thunk = 
        let value = x.Get name
        x.Set name (fst value, thunk)

and Environment= 
    { 
        scopes : Scope list;
        subs : Map<TypeName, TypeExp>;
        typeNum : TypeName;
    }
    static member Empty with get() = { scopes = []; subs = Map.empty; typeNum = 0; }
    member x.NewTypeVar () = ({ scopes = x.scopes; subs = x.subs; typeNum = x.typeNum +  1;}, x.typeNum + 1)
    member x.AddTypeSub name typeExp = {scopes = x.scopes; subs = x.subs.Add (name, typeExp); typeNum = x.typeNum; }
    member private x.reduceSubs typeExp = 
        match typeExp with
        | TypeVar typeName -> 
            match x.subs.TryFind typeName with
            | None -> (x, TypeVar typeName)
            | Some tExp -> 
                let (e, resExp) = x.reduceSubs tExp
                let env = e.AddTypeSub typeName resExp
                (env, resExp)
        | TypeOperator (op, args) -> 
            let (env, newArgTypes) = x.reduceSubsList args []
            (env, TypeOperator (op, newArgTypes))
            
    member private x.reduceSubsList argTypes newTypes =
        if List.isEmpty argTypes then 
            (x, newTypes)
        else
            let (env, typ) = x.reduceSubs argTypes.Head
            env.reduceSubsList argTypes.Tail (List.append newTypes [typ])
               
    member x.Get name = x.getFromScopes name x.scopes
    member private x.getFromScopes name scopes = 
        match scopes with
        | sc :: rest -> 
            let idValOpt = sc.TryGet name
            if idValOpt.IsSome then
                let idVal = idValOpt.Value
                let (env, updateType) = x.reduceSubs (fst idVal)
                ({ scopes = env.scopes; subs = env.subs; typeNum = env.typeNum;}, Some (updateType, snd idVal))
            else
                x.getFromScopes name rest
        | [] -> (x,None)
                
            
    member x.Set name value = { scopes = x.scopes.Head.Set name value :: x.scopes.Tail; subs = x.subs; typeNum = x.typeNum; }
    member x.SetThunk name thunk = { scopes = x.scopes.Head.SetThunk name thunk :: x.scopes.Tail; subs = x.subs; typeNum = x.typeNum;}
    member x.SetType name typeExp = { scopes = x.scopes.Head.SetType name typeExp :: x.scopes.Tail; subs = x.subs; typeNum = x.typeNum;}
    member x.PopScope () = { scopes = x.scopes.Tail; subs = x.subs; typeNum = x.typeNum;} , x.scopes.Head
    member x.PushScope scope = { scopes = scope :: x.scopes; subs = x.subs; typeNum = x.typeNum; }
    member x.NewScope () = x.PushScope Scope.Empty
    member x.Contains name = x.scopes.Head.Contains name
    member x.ApplySubs typeExp = 
        let (env, tp) = x.reduceSubs typeExp
        ({ scopes = env.scopes; subs = env.subs; typeNum = env.typeNum; }, tp)
           



type Pos = 
    { 
        Line : int;
        Column : int
    }

type ExpressionResult = { Exp : Expression; Pos: Pos; Type: TypeExp}
and Expression = 
    | Call of ExpressionResult * ExpressionResult list
    | Cond of ExpressionResult * ExpressionResult * ExpressionResult
    | Let of ExpressionResult * ExpressionResult
    | Fun of ExpressionResult * ExpressionResult list * ExpressionResult * Scope
    | Id of String
    | Int of int
    | Float of float
    | Bool of bool