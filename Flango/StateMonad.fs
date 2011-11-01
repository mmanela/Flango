module StateMonad

type StateMonad<'st,'a> = 'st -> State<'st,'a>  
and State<'st,'a> = 
    | Ok of  'a * 'st
    | Error of ErrorState
and ErrorState = string
and StateMonadBuilder() = 
    member b.Return(x) = fun s -> Ok (x, s)
    member b.ReturnFrom(x) = x
    member b.Zero() = fun s -> Ok ((),s)
    member b.Error msg = fun _ -> Error msg
    member b.Bind(p, rest) = 
        fun state -> 
                 let result = p state in 
                 match result with
                 | Ok (value,state2) -> (rest value) state2
                 | Error msg -> Error msg  

    member b.Get () = fun state -> Ok (state, state)
    member b.Put s = fun state -> Ok ((), s)

    member b.Combine(p1, p2) = 
        fun inputState -> 
            match p1 inputState with
            | Error msg -> match p2 inputState with
                           | Error msg2 -> Error msg
                           | otherwise -> otherwise
            | otherwise -> otherwise

let stateMondad = StateMonadBuilder()

let (<|>) p1 p2 = stateMondad.Combine(p1, p2)
let (>>=) a f = stateMondad.Bind(a,f)
let (>>) a f = stateMondad.Bind(a,fun _ -> f)

let rec map action items  = 
    stateMondad {
        if List.isEmpty items
        then return []
        else 
            let! res = action items.Head
            let! rest = map action items.Tail
            return  res :: rest
    }