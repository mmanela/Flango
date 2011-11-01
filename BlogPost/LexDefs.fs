module LexDefs

open Lexer

let tokenDefinitions = 
    state {
        do! addNextlineDefinition "NEWLINE" @"(\n\r)|\n|\r"
        do! addIgnoreDefinition "WS"        @"\s"
        
        // keywords
        do! addDefinition "KEYWORDS"        "var|function|if|for"
        do! addDefinition "BOOL"            "true|false"
           
        // literals
        do! addDefinition "ID"              "(?i)[a-z][a-z0-9]*"
        do! addDefinition "FLOAT"           @"[0-9]+\.[0-9]+"
        do! addDefinition "INT"             "[0-9]+"
        
        // symbols
//        do! addDefinition "LPAREN"      @"\("
//        do! addDefinition "RPAREN"      @"\)"
//        do! addDefinition "LCURL"       @"\{"
//        do! addDefinition "RCURL"       @"\}"
//        do! addDefinition "LBRACK"      @"\["
//        do! addDefinition "RBRACK"      @"\]"

        // operators
        do! addDefinition "SYNTAX"      @"[\[\]\{\}\(\),]+"
        do! addDefinition "OPERATOR"      @"[?:+*=!/&|<>\^\-.]+"
        
        
    }