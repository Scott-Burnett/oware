module Oware

type StartingPosition =
    | South
    | North

type board = {
    a : int
    b : int
    c : int
    d : int
    e : int
    f : int
    a' : int 
    b' : int
    c' : int
    d' : int
    e' : int
    f' : int
    npoints : int
    spoints : int
    turn : StartingPosition 
} 
    
let getSeeds n board = 
    match n with 
        |1 -> board.a
        |2 -> board.b
        |3 -> board.c
        |4 -> board.d
        |5 -> board.e
        |6 -> board.f
        |7 -> board.a'
        |8 -> board.b'
        |9 -> board.c'
        |10 -> board.d'
        |11 -> board.e'
        |12 -> board.f'
        |_ -> failwith "Out of range"

let useHouse n board = failwith "Not implemented"

let start position = {a = 4; b = 4; c = 4; d = 4; e = 4; f = 4; a' = 4; b' = 4; c' = 4; d' = 4; e' = 4; f' = 4; npoints = 0; spoints = 0; turn = position}

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
