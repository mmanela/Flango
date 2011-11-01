module FlangoLexDefs

open Lexer

let flangoSymbolDefinitions = 
    state {
        do! addNextlineDefinition "NEWLINE" @"(\n\r)|\n|\r"
        do! addIgnoreDefinition "WS"        @"\s"
        
        // keywords
        do! addDefinition "LET"             "let"
        do! addDefinition "FUNCTION"        "fun"
        do! addDefinition "BOOL"            "True|False"
        do! addDefinition "IF"              "if"
        do! addDefinition "THEN"            "then"
        do! addDefinition "ELSE"            "else"
           
        // literals
        do! addDefinition "ID"              "(?i)[a-z][a-z0-9]*"
        do! addDefinition "FLOAT"           @"[0-9]+\.[0-9]+"
        do! addDefinition "INT"             "[0-9]+"
        
        // symbols
        do! addDefinition "LPAREN"      @"\("
        do! addDefinition "RPAREN"      @"\)"

        // operators
        do! addDefinition "OPERATOR"      @"[+*=!/&|<>\^\-]+"
        
        
    }