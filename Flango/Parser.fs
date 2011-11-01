module Parser

open Lexer
open System
open System.Collections.Generic
open Models
open StateMonad

// Parser states consists of 'a which is the return type of any given parser combination
// and 'ex which is the user define type of the AST they are building
type Parser<'a, 'ex> = StateMonad<InputState<'ex>,'a>
and Symbol<'ex> = 
    {
        name : string;
        led : Token -> 'ex -> Parser<'ex,'ex>;
        nud : Token -> int -> Parser<'ex,'ex>;
        lbp : int;
    }
and InputState<'ex> = 
    { 
        tokens : seq<Token>;
        token : Token option;
        symbol : Symbol<'ex> option;
        symbolMap : Map<string, Symbol<'ex>>
    }


let parser = stateMondad

let pp inp = match inp.token with
             | Some t -> Ok (5,inp)
             | None -> Error "HEY"


let error token message = 
    parser {
        let errorMsg = sprintf "Parsing Error:\n %s\n At line:%d and character:%d at text:%s" message token.line token.column token.text
        return! parser.Error errorMsg
    }

let errorLed token left =
    parser {
        return! error token "Symbol does not take a left argument"
    }

let errorNud token rbp =
    parser {
        return! error token "Symbol does not take zero left arguments"
    }

let errorStd token =
    parser {
        return! error token "Unexpected symbol"
    }

let currentSymbol() =
    parser {
        let! state = parser.Get()
        return Option.get state.symbol
    }
let currentToken() =
    parser {
        let! state = parser.Get()
        return Option.get state.token
    }
let current() =
    parser {
        let! state = parser.Get()
        return Option.get state.token, Option.get state.symbol
    }


    
let upateTokenAndSymbol (token:Token) rest =
    parser {
        let! state = parser.Get()
        let symbol = Map.tryFind token.name state.symbolMap
        match symbol with
        | None -> 
            let symbol = Map.tryFind token.text state.symbolMap
            match symbol with
            | None -> 
                do! parser.Put {
                    tokens = rest; 
                    token = Some token; 
                    symbol = Some {name = token.text; led = errorLed; nud = errorNud; lbp = 0}
                    symbolMap = state.symbolMap;
                } 
            | Some sym ->
                do! parser.Put {
                    tokens = rest; 
                    token = Some token; 
                    symbol = Some sym; 
                    symbolMap = state.symbolMap;
                } 
        | Some sym ->
            do! parser.Put {
                tokens = rest; 
                token = Some token; 
                symbol = Some sym; 
                symbolMap = state.symbolMap;
                }
    }


let nextToken() =
    parser {
        let! state = parser.Get()
        if Seq.isEmpty state.tokens then 
            return! error (Option.get state.token) "Unexpected end of input"
        else
            let token = Seq.head state.tokens
            let rest = Seq.skip 1 state.tokens
            do! upateTokenAndSymbol token rest
    }

let advance() = 
    parser {
        do! nextToken()
    }
    

let verifyName tokenName =
    parser {
        let! token = currentToken()
        if token.name <> tokenName then
            return! error token (sprintf "Expected a token with name %s " tokenName)
    }

let verifyText tokenText =
    parser {
        let! token = currentToken()
        if token.text <> tokenText then
            return! error token (sprintf "Expected a token with text %s" tokenText)
    }
    
    
// advances if the current token matches token name
let advanceIfName tokenName = 
    parser {
        do! verifyName tokenName
        return! advance()
    }
let advanceIfText tokenText = 
    parser {
        do! verifyText tokenText
        return! advance()
    }
 
let rec leftBoundExpression rbp token symbol left = 
    parser {
        if (rbp < symbol.lbp) then
            do! advance()
            let! newLeft = symbol.led token left
            let! newToken, newSymbol = current()
            return! leftBoundExpression rbp newToken newSymbol newLeft
        else
            return left
    }
    
let expression rbp = 
    parser {
        let! token, symbol = current()
        do!  advance()
        let! left = symbol.nud token rbp
        let! nextToken, nextSymbol = current()
        return! leftBoundExpression rbp nextToken nextSymbol left
    }

let rec many rbp combinator = 
    parser {
        let! exp = combinator rbp
        let! rest =  (many rbp combinator)
        return exp :: rest
    } <|> parser.Return []

let rec manyTokens cond = 
    parser {
        let! token, symbol = current()
        if cond token 
        then 
            do! advance()
            let! rest =  manyTokens cond
            return token :: rest
        else return []
    }

let rec manyExpressions rbp = 
    parser {
        let! exp = expression rbp
        let! rest =  (manyExpressions rbp)
        return exp :: rest
    } <|> parser.Return []

let rec manyExpressionsWhile rbp cond = 
    parser {
        let! token, symbol = current()
        if cond token 
        then
            let! exp = expression rbp
            let! rest =  (manyExpressionsWhile rbp cond)
            return exp :: rest
        else return []
    } <|> parser.Return []

let rec manyExpressionsInFile rbp = 
    parser {
        let! token, symbol = current()
        if token.name <> "(END)"
        then
            let! exp = expression rbp
            let! rest =  (manyExpressionsInFile rbp)
            return exp :: rest
        else return []
    }

// Symbol definition functions
let addSymbol symbol = Map.add symbol.name symbol
            

let binaryLed builder rbp token left =
   parser {
        let! right = expression rbp
        return builder token [left; right]
    }

let addBinaryOperator name lbp rbp builder =
    addSymbol    { name = name;
                   lbp = lbp;
                   nud = errorNud;
                   led = binaryLed builder rbp;
                 }

let addInfixL name lbp builder = addBinaryOperator name lbp lbp builder
let addInfixR name lbp builder = addBinaryOperator name lbp (lbp - 1) builder


let initParse state =
        let result = advance () state
        match result with
         | Ok (value,inputState2) ->
            manyExpressionsInFile 0 inputState2
         | Error msg -> Error msg
        
        
let parse toks symbolDefs = 
    let tokens = seq { yield! toks; yield { name="(END)"; text=""; pos=0; column=0; line=0;} }
    if(Seq.isEmpty tokens) then 
        Error "No input"
    else 
        initParse {tokens = tokens; symbolMap = symbolDefs; token = None; symbol = None;}