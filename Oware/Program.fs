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
    let getboard a b c d e f a' b' c' d' e' f' spoints npoints turn = 
        match turn = South with 
            |true -> { a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = spoints; npoints = npoints; turn = North }
            |false -> { a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = spoints; npoints = npoints; turn = South }
    let rec reset a b c d e f a' b' c' d' e' f' sp np turn offset =
        match offset = 0 with 
            |true -> getboard a b c d e f a' b' c' d' e' f' sp np turn
            |false -> reset f' a b c d e f a' b' c' d' e' sp np turn ((offset + 11) % 12)
    let rec getpoints a b c d e f a' b' c' d' e' f' spoints npoints turn offset = 
        match (a = 2 || a = 3), (offset >= 0 && offset <= 5 && turn = North), (offset >= 6 && offset <= 11 && turn = South) with
            |true, true, false -> getpoints f' 0 b c d e f a' b' c' d' e' (spoints) (npoints + a) turn ((offset + 11) % 12)
            |true, false, true -> getpoints f' 0 b c d e f a' b' c' d' e' (spoints + a) (npoints) turn ((offset + 11) % 12)
            |_, _, _ -> reset a b c d e f a' b' c' d' e' f' spoints npoints turn offset
    let rec go a b c d e f a' b' c' d' e' f' numseeds offset cycles = 
        match numseeds = 1, (cycles = 0) with
            |_, true -> go b c d e f a' b' c' d' e' f' a numseeds ((offset + 1) % 12) ((cycles + 1) % 12)
            |true, false -> getpoints (a + 1) b c d e f a' b' c' d' e' f' board.spoints board.npoints board.turn (offset) 
            |false, false -> go b c d e f a' b' c' d' e' f' (a + 1) (numseeds - 1) ((offset + 1) % 12) ((cycles + 1) % 12)
    match (((n > 0 && n < 7) && board.turn = South) || ((n > 5 && n < 13) && board.turn = North)) && ((getSeeds n board) > 0) with
        |true -> match n with 
            |1 -> go board.b board.c board.d board.e board.f board.a' board.b' board.c' board.d' board.e' board.f' 0 board.a 1 1
            |2 -> go board.c board.d board.e board.f board.a' board.b' board.c' board.d' board.e' board.f' board.a 0 board.b 2 1 
            |3 -> go board.d board.e board.f board.a' board.b' board.c' board.d' board.e' board.f' board.a board.b 0 board.c 3 1
            |4 -> go board.e board.f board.a' board.b' board.c' board.d' board.e' board.f' board.a board.b board.c 0 board.d 4 1
            |5 -> go board.f board.a' board.b' board.c' board.d' board.e' board.f' board.a board.b board.c board.d 0 board.e 5 1
            |6 -> go board.a' board.b' board.c' board.d' board.e' board.f' board.a board.b board.c board.d board.e 0 board.f 6 1
            |7 -> go board.b' board.c' board.d' board.e' board.f' board.a board.b board.c board.d board.e board.f 0 board.a' 7 1
            |8 -> go board.c' board.d' board.e' board.f' board.a board.b board.c board.d board.e board.f board.a' 0 board.b' 8 1
            |9 -> go board.d' board.e' board.f' board.a board.b board.c board.d board.e board.f board.a' board.b' 0 board.c' 9 1
            |10 -> go board.e' board.f' board.a board.b board.c board.d board.e board.f board.a' board.b' board.c' 0 board.d' 10 1
            |11 -> go board.f' board.a board.b board.c board.d board.e board.f board.a' board.b' board.c' board.d' 0 board.e' 11 1
            |12 -> go board.a board.b board.c board.d board.e board.f board.a' board.b' board.c' board.d' board.e' 0 board.f' 0 1
        |false -> board

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
