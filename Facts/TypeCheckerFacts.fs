module TypeCheckerFacts

open Xunit
open FlangoTypeChecker
open Models
open Utilities

[<Fact>]
let Will_type_integer_correctly() =
  
    let (state,typ) = typeCheck "5" |> Option.get
    
    Assert.Equal("Integer", typ.ToString())
    
    
[<Fact>]
let Will_type_true_bool_correctly() =
  
    let (state,typ) = typeCheck "True" |> Option.get
    
    Assert.Equal("Boolean", typ.ToString())
    
          
[<Fact>]
let Will_type_false_bool_correctly() =
  
    let (state,typ) = typeCheck "False" |> Option.get
    
    Assert.Equal("Boolean", typ.ToString())
  
[<Fact>]
let Will_type_integer_to_integer_function() =
  
    let (state,typ) = typeCheck "fun inc x = x + 1" |> Option.get
    
    Assert.Equal("Integer->Integer",typ.ToString())
 
 
[<Fact>]
let Will_type_boolean_to_boolean_function() =
  
    let (state,typ) = typeCheck "fun is x = x = True" |> Option.get
    
    Assert.Equal("Boolean->Boolean",typ.ToString())
 
 
[<Fact>]
let Will_type_a_unary_unbounded_function() =
  
    let (state,typ) = typeCheck "fun id x = x" |> Option.get
    
    Assert.Equal("'a->'a",typ.ToString())   
    
[<Fact>]
let Will_type_a_binary_unbounded_function() =
  
    let (state,typ) = typeCheck "fun id x y = y" |> Option.get
    
    Assert.Equal("'a->('b->'b)",typ.ToString())     
   
[<Fact>]
let Will_type_conditional_statement_to_result_type() =
  
    let (state,typ) = typeCheck "if True then 1 else 2" |> Option.get
    
    Assert.Equal("Integer", typ.ToString())
    

[<Fact>]
let Will_type_a_binary_function_with_unbound_variables_function() =
  
    let (state,typ) = typeCheck "fun id x y = y" |> Option.get
    
    Assert.Equal("'a->('b->'b)",typ.ToString())
    
    

[<Fact>]
let Will_type_a_argument_as_a_function_if_used_as_callee_in_function_body() =
  
    let (state,typ) = typeCheck "fun id x y = x y" |> Option.get
    
    Assert.Equal("('a->'b)->('a->'b)",typ.ToString()) 

[<Fact>]
let Will_type_composition_of_add_function_with_composition_function() =
  
    let code = "
    fun id x = x
    fun add a b = a b
    add id"

    let (state,typ) = typeCheck code |> Option.get
    
    Assert.Equal("'a->'a",typ.ToString()) 

    
[<Fact>]
let Will_type_resolving_generic_function_type() =

    let code = "
    fun id x = x
    let x = 5
    id x"

    let (state,typ) = typeCheck code |> Option.get
    
    Assert.Equal("Integer",typ.ToString()) 


[<Fact>]
let Will_create_fresh_vars_for_bound_type_vars() =

    let code = "
    fun id x = x
    let x = 5
    id x
    id"

    let (state,typ) = typeCheck code |> Option.get
    
    Assert.Equal("'a->'a",typ.ToString()) 
