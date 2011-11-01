module InterpreterFacts

open Xunit
open Models
open Utilities
open Interpreter


[<Fact>]
let Will_interpret_integer() =
  
    let (state,res) = interpret "5" |> Option.get
    
    Assert.Equal("5", res)
    
    
[<Fact>]
let Will_interpret_true_bool() =
  
    let (state,res) = interpret "True" |> Option.get
    
    Assert.Equal("True", res)
    
          
[<Fact>]
let Will_interpret_false_bool() =
  
    let (state,res) = interpret "False" |> Option.get
    
    Assert.Equal("False", res)
  

[<Fact>]
let Will_return_value_from_let_bound_variable() =
  
    let code ="
    let x = 5
    x"

    let (_,res) = interpret code |> Option.get
    
    Assert.Equal("5",res)

[<Fact>]
let Will_execute_function_with_passed_in_arg() =
  
    let code ="
    fun id a = a + 5
    id 5"

    let (_,res) = interpret code |> Option.get
    
    Assert.Equal("10",res)

[<Fact(Timeout=2000)>]
let Will_execute_function_with_passed_in_arg_that_is_same_name_as_let_binding() =
  
    let code ="
    let x = 5
    fun id x = x
    id x"

    let (_,res) = interpret code |> Option.get

    Assert.Equal("5",res)
