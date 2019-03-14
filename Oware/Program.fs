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
    let rec reset a b c d e f a' b' c' d' e' f' n =
        match n = 0 with 
            |true -> { a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = board.spoints; npoints = board.npoints; turn = board.turn }
            |false -> reset f' a b c d e f a' b' c' d' e' (n - 1)
    let rec go a b c d e f a' b' c' d' e' f' temp nummoves = 
        match temp = 0 with 
            |true -> reset a b c d e f a' b' c' d' e' f' nummoves
            |false -> go (b + 1) c d e f a' b' c' d' e' f' a (temp - 1) nummoves
    match n with 
        |1 -> go 0 board.b board.c board.d board.e board.f board.a' board.b' board.c' board.d' board.e' board.f' board.a board.a
        |2 -> go 0 board.c board.d board.e board.f board.a' board.b' board.c' board.d' board.e' board.f' board.a board.b (board.b + n - 1)   
        |3 -> go 0 board.d board.e board.f board.a' board.b' board.c' board.d' board.e' board.f' board.a board.b board.c (board.c + n - 1)
        |4 -> go 0 board.e board.f board.a' board.b' board.c' board.d' board.e' board.f' board.a board.b board.c board.d (board.d + n - 1)
        |5 -> go 0 board.f board.a' board.b' board.c' board.d' board.e' board.f' board.a board.b board.c board.d board.f (board.f + n - 1)
        |6 -> go 0 board.a' board.b' board.c' board.d' board.e' board.f' board.a board.b board.c board.d board.e board.f (board.f + n - 1)
        |7 -> go 0 board.b' board.c' board.d' board.e' board.f' board.a board.b board.c board.d board.e board.f board.a' (board.a' + n - 1)
        |8 -> go 0 board.c' board.d' board.e' board.f' board.a board.b board.c board.d board.e board.f board.a' board.b' (board.b' + n - 1)  
        |9 -> go 0 board.d' board.e' board.f' board.a board.b board.c board.d board.e board.f board.a' board.b' board.c' (board.c' + n - 1)
        |10 -> go 0 board.e' board.f' board.a board.b board.c board.d board.e board.f board.a' board.b' board.c' board.d' (board.d' + n - 1)
        |11 -> go 0 board.f' board.a board.b board.c board.d board.e board.f board.a' board.b' board.c' board.d' board.f' (board.f' + n - 1)
        |12 -> go 0 board.a board.b board.c board.d board.e board.f board.a' board.b' board.c' board.d' board.e' board.f' (board.f' + n - 1)

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
