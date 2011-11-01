module Lexer

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open StateMonad

type Token = 
    { name : string;
      text: string; 
      pos :int;
      column: int;
      line: int }
      
type LexDefinitions = 
  {regexes : string list;
   names : string list;
   nextlines : bool list;
   ignores : bool list; }


val state : StateMonadBuilder

val tokenize : StateMonad<LexDefinitions,'a> -> string -> seq<Token>

val addDefinition : string -> string -> StateMonad<LexDefinitions,unit>
val addIgnoreDefinition : string -> string -> StateMonad<LexDefinitions,unit>
val addNextlineDefinition : string -> string -> StateMonad<LexDefinitions,unit>