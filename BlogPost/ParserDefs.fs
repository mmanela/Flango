module ParserDefs

open Parser
open Lexer
open System


type Expression = 
    | Assign of Expression * Expression
    | Call of Expression * Expression list
    | Cond of Expression * Expression * Expression
    | Let of Expression * Expression
    | Fun of Expression * Expression list * Expression
    | Id of string
    | Int of int
    | Float of float
    | Bool of bool

let callBuilder token args =(Call (Id token.text, args))


let letNud token rbp = 
    parser {
        let! idToken, idSymbol = current()
        do! advanceIfName "ID"
        
        do! advanceIfText "="
        let! exp = expression 0
        
        return (Let (Id idToken.text, exp))
    };

let ifNud token rbp = 
    parser {
        let! cond = expression 0
        do! advanceIfName "THEN"
        let! exp1 = expression 0
        do! advanceIfName "ELSE"
        let! exp2 = expression 0
        
        return (Cond (cond, exp1, exp2))
    };
    
let functionArgCombinator rbp =
    parser {
        do! advanceIfText ","
        return! expression rbp
    }

let funNud token rbp = 
    parser {
        let! nameToken, nameSymbol = current()
        if nameToken.name <> "ID"
        then return! error nameToken "Invalid function name"
        else
            do! advance()
            do! advanceIfText("(")
            let! currentToken = currentToken()
            if currentToken.text = ")"
            then
                return Fun(Id nameToken.text,[],Id nameToken.text)
            else
                let! first = expression 90
                let! rest = many 90 functionArgCombinator
                do! advanceIfText ")"
                return Fun (left, List.Cons(first,rest))

//            let! args = manyTokens (fun x -> x.name = "ID")
//            if List.isEmpty args 
//            then return! error token "Functions need arguments!"
//            else
//                do! advanceIfText "="
//                let! body = expression 0
//                let argExpList = List.map (fun t -> Id t.text) args
//                return Fun (Id nameToken.text, argExpList, body)
    };



let leftParenLed token left = 
    parser {
        let! currentToken = currentToken()
        if currentToken.text = ")"
        then
            return Call (left, [])
        else
            let! first = expression 90
            let! rest = many 90 functionArgCombinator
            do! advanceIfText ")"
            return Call (left, List.Cons(first,rest))
        
    }

let leftParenNud token rbp = 
    parser {
        let! exp = expression 0
        do! advanceIfText ")"
        return exp  
    }

let idNud token rbp = 
    parser {
        return (Id token.text)  
    };


let valueNud dataConstructor converter token rbp = 
    parser {
        return  (dataConstructor (converter token.text))
    }


let ternaryLed token left =
   parser {
        let first = left
        let! second = expression 0
        do! advanceIfText ":"
        let! third = expression 0
        return Call (Id token.text, [first; second; third])
    }

let indexerLed token left =
   parser {
        let! index = expression 0
        do! advanceIfText "]"
        return Call (Id token.text, [left; index])
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
      lbp = 80;
      nud = leftParenNud;
      led = leftParenLed;
    }
    
let ifSymbol =
    { name = "IF";
      lbp = 0;
      nud = ifNud;
      led = errorLed;
    }
    
let funSymbol =
    { name = "function";
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

let ternarySymbol = 
    { name = "?";
      lbp = 20;
      nud = errorNud;
      led = ternaryLed;
    }

let indexerSymbol = 
    { name = "[";
      lbp = 80;
      nud = errorNud;
      led = indexerLed;
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
    if str = "true" then true
    else false
    
let symbolDefs =    Map.empty |>  
                    addEmptySymbol ":" |>
                    //addEmptySymbol ")" |>
                    //addEmptySymbol "THEN" |>
                    //addEmptySymbol "ELSE" |>
                        
                    addLiteralSymbol "INT" Int int |>
                    addLiteralSymbol "FLOAT" Float double |>
                    addLiteralSymbol "BOOL" Bool bool  |>
                        
                        
                    addSymbol eqSymbol |>
                    addInfixR "!=" 10 callBuilder |>
                    addSymbol leftParenSymbol |>
                    addSymbol indexerSymbol |>
                    addSymbol ternarySymbol |>
                    addInfixR "&&" 30 callBuilder |>
                    addInfixR "||" 30 callBuilder |>
                        
                    addInfixL "===" 40 callBuilder |>
                    addInfixL "!==" 40 callBuilder |>
                    addInfixL "==" 40 callBuilder |>
                    addInfixL "!=" 40 callBuilder |>
                    addInfixL ">" 40 callBuilder |>
                    addInfixL ">=" 40 callBuilder |>
                    addInfixL "<" 40 callBuilder |>
                    addInfixL "<=" 40 callBuilder |>
                        
                    addInfixL "+"  50 callBuilder |>
                    addInfixL "-" 50 callBuilder |>
                    addInfixL "*" 60 callBuilder |>
                    addInfixL "/" 60 callBuilder |>
                    addInfixL "^" 70 callBuilder|>
                    addInfixL "." 80 callBuilder|>
                        
                    addSymbol idSymbol |>
                    addSymbol leftParenSymbol |>
                    //addSymbol varSymbol |>
                    addSymbol funSymbol |>
                    //addSymbol ifSymbol |>
                    addSymbol endSymbol