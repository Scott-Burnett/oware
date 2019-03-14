module Oware

open System.IO

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
    turn : StartingPosition } 
    
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

let useHouse n board = 
    let rec go a b c d e f a' b' c' d' e' f' temp nummoves = 
        match temp = 0 with 
            |true -> 1
            |false -> go (b + 1) c d e f a' b' c' d' e' f' a (temp - 1) nummoves
    match n with 
        |1 -> go 0 board.b 
     

let start position = {a = 4; b = 4; c = 4; d = 4; e = 4; f = 4; a' = 4; b' = 4; c' = 4; d' = 4; e' = 4; f' = 4; npoints = 0; spoints = 0; turn = position}

let score board = (board.spoints, board.npoints)

let gameState board = match board.npoints > 25, board.spoints > 25 with 
    |true, true -> "Game ended in a draw"
    |true, false -> "North won"
    |false, true -> "South won"
    |false, false -> match board.turn with 
        |South -> "South's turn"
        |North -> "North's turn"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
