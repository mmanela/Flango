module FlangoParserDefs
open Parser
open Models
open Lexer
open System

let expResult exp token = {Exp = exp; Pos = {Line = token.line; Column = token.column}; Type = Unknown}
let idExp token = { Exp = Id token.text; Pos = {Line = token.line; Column = token.column}; Type = Unknown}

let callBuilder token args = expResult (Call (idExp token, args)) token


let onside (startToken:Token) (currentToken:Token) = 
    startToken.line <= currentToken.line 
    && startToken.column < currentToken.column
   

let letNud token rbp = 
    parser {
        let! idToken, idSymbol = current()
        do! advanceIfName "ID"
        
        do! advanceIfText "="
        let! exp = expression 0
        
        return expResult (Let (idExp idToken, exp)) token
    };

let ifNud token rbp = 
    parser {
        let! cond = expression 0
        do! advanceIfName "THEN"
        let! exp1 = expression 0
        do! advanceIfName "ELSE"
        let! exp2 = expression 0
        
        return expResult (Cond (cond, exp1, exp2)) token
    };
    

let funNud token rbp = 
    parser {
        let! nameToken, nameSymbol = current()
        if nameToken.name <> "ID"
        then return! error nameToken "Invalid function name"
        else
            do! advance()
            let! args = manyTokens (fun x -> x.name = "ID")
            if List.isEmpty args 
            then return! error token "Functions need arguments!"
            else
                do! advanceIfText "="
                let! body = expression 0
                let argExpList = List.map (fun t -> expResult (Id t.text) t) args
                return expResult (Fun (idExp nameToken, argExpList, body, Scope.Empty )) nameToken
    };


let leftParenNud token rbp = 
    parser {
        let! exp = expression 0
        let! _ = advanceIfText ")"

        if rbp < 90 then
            let! args = manyExpressions 90
            if args.Length > 0
            then return expResult (Call (exp, args)) token
            else return expResult exp.Exp token
        else 
            return expResult exp.Exp token
    }

let idNud token rbp = 
    parser {
        if rbp < 90 then
            let! args = manyExpressionsWhile 90 (onside token)
            if args.Length > 0
            then return callBuilder token  args
            else return expResult (Id token.text) token 
        else 
            return expResult (Id token.text) token 
    };


let valueNud dataConstructor converter token rbp = 
    parser {
        return expResult (dataConstructor (converter token.text)) token
    }

// A symbol which represents a single literal value. ex: a number, a bool
let addLiteralSymbol name dataConstructor converter = 
    addSymbol     { name = name; 
                       lbp = 0; 
                       led = errorLed; 
                       nud = valueNud dataConstructor converter 
                     }

let leftParenSymbol = 
    { name = "(";
      lbp = 0;
      nud = leftParenNud;
      led = errorLed;
    }

let letSymbol =
    { name = "LET";
      lbp = 0;
      nud = letNud;
      led = errorLed;
    }
    
let ifSymbol =
    { name = "IF";
      lbp = 0;
      nud = ifNud;
      led = errorLed;
    }
    
let funSymbol =
    { name = "FUNCTION";
      lbp = 0;
      nud = funNud;
      led = errorLed;
    }
    
    
let idSymbol = 
    {
        name = "ID";
        lbp = 0;
        nud = idNud;
        led = errorLed;
    }

let eqSymbol =
    { name = "=";
      lbp = 10;
      nud = errorNud;
      led = binaryLed callBuilder 9; 
    }

// The end symbol determines the end of input
let endSymbol = 
    { name="(END)"; 
      led = errorLed; 
      nud = errorNud;
      lbp = -Int32.MaxValue
    }
    
let addEmptySymbol name = 
    addSymbol  {   name = name; 
                   led = errorLed; 
                   nud = errorNud; 
                   lbp =0; 
                  }
let bool str = 
    if str = "True" then true
    else false
    
let flangoSymbolDefs =  Map.empty |>  
                        addEmptySymbol ")" |>
                        addEmptySymbol "THEN" |>
                        addEmptySymbol "ELSE" |>
                        
                        addLiteralSymbol "INT" Int int |>
                        addLiteralSymbol "FLOAT" Float double |>
                        addLiteralSymbol "BOOL" Bool bool  |>
                        
                        
                        addSymbol eqSymbol |>
                        addInfixR "!=" 10 callBuilder |>
                        addInfixR "&&" 30 callBuilder |>
                        addInfixR "||" 30 callBuilder |>
                        
                        addInfixL ">" 40 callBuilder |>
                        addInfixL ">=" 40 callBuilder |>
                        addInfixL "<" 40 callBuilder |>
                        addInfixL "<=" 40 callBuilder |>
                        
                        addInfixL "+"  50 callBuilder |>
                        addInfixL "-" 50 callBuilder |>
                        addInfixL "*" 60 callBuilder |>
                        addInfixL "/" 60 callBuilder |>
                        addInfixL "^" 70 callBuilder|>
                        
                        addSymbol idSymbol |>
                        addSymbol leftParenSymbol |>
                        addSymbol letSymbol |>
                        addSymbol funSymbol |>
                        addSymbol ifSymbol |>
                        addSymbol endSymbol