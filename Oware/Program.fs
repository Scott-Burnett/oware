module Oware

open System.IO
open System

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




let houseBelongsToPlayer n board =
    ((n > 0 && n < 7 && board.turn = South) || (n > 5 && n < 13 && board.turn = North))

let houseIsNotEmpty n board = 
    (getSeeds n board) > 0

let pHasNoSeeds ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; turn = turn } as board) =
    (a = 0 && b = 0 && c = 0 && d = 0 && e = 0 && f = 0 && turn = South) || (a' = 0 && b' = 0 && c' = 0 && d' = 0 && e' = 0 && f' = 0 && turn = North)

let nextTurn ({turn = turn} as board) =
    match turn with
        |South -> {board with turn = North}
        |North -> {board with turn = South}

let getboard (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset) =            
    let rec reset a b c d e f a' b' c' d' e' f' spoints npoints turn offset =
        match offset = 0 with 
            |true -> { a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = spoints; npoints = npoints; turn = turn }
            |false -> reset f' a b c d e f a' b' c' d' e' spoints npoints turn ((offset + 11) % 12)
    reset a b c d e f a' b' c' d' e' f' spoints npoints turn offset

let getpoints (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset) = 
    let justInCaseBoard = nextTurn (getboard (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset))
    let rec inner a b c d e f a' b' c' d' e' f' spoints npoints turn offset =
        match (a = 2 || a = 3), (offset >= 0 && offset <= 5 && turn = North), (offset >= 6 && offset <= 11 && turn = South) with
            |true, true, false -> inner f' 0 b c d e f a' b' c' d' e' spoints (npoints + a) turn ((offset + 11) % 12)
            |true, false, true -> inner f' 0 b c d e f a' b' c' d' e' (spoints + a) npoints turn ((offset + 11) % 12)
            |_, _, _ -> let nextboard = nextTurn (getboard (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset))
                        match pHasNoSeeds nextboard with
                            |true -> justInCaseBoard
                            |false -> nextboard
    inner a b c d e f a' b' c' d' e' f' spoints npoints turn offset

let rec sowseeds (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset) numseeds cycles = 
    let rec inner a b c d e f a' b' c' d' e' f' spoints npoints turn offset numseeds cycles =
        match numseeds = 1, (cycles = 0) with
            |_, true -> inner b c d e f a' b' c' d' e' f' a spoints npoints turn ((offset + 1) % 12) numseeds ((cycles + 1) % 12)
            |false, false -> inner b c d e f a' b' c' d' e' f' (a + 1) spoints npoints turn ((offset + 1) % 12) (numseeds - 1) ((cycles + 1) % 12)
            |true, false -> getpoints ((a + 1), b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset)        
    inner a b c d e f a' b' c' d' e' f' spoints npoints turn offset numseeds cycles

let noValidTurns ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'} as board) =
    (a < 6 && b < 5 && c < 4 && d < 3 && e < 2 && f < 1 && a' < 6 && b' < 5 && c' < 4 && d' < 3 && e' < 2 && f' < 1)

let cleanUp ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = spoints; npoints = npoints; turn = turn} as board) =
    {a = 0; b = 0; c = 0; d = 0; e = 0; f = 0; a' = 0; b' = 0; c' = 0; d' = 0; e' = 0; f' = 0; spoints = spoints + a + b + c + d + e + f; npoints = npoints + a' + b' + c' + d' + e' + f'; turn = turn }

let useHouse n ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = spoints; npoints = npoints; turn = turn } as board) = 
    let newboard = 
        let newboard =
            match (houseBelongsToPlayer n board) && (houseIsNotEmpty n board) with
                |true -> match n with 
                    |1 -> sowseeds (b, c, d, e, f, a', b', c', d', e', f', 0, spoints, npoints, turn, 1) a 1
                    |2 -> sowseeds (c, d, e, f, a', b', c', d', e', f', a, 0, spoints, npoints, turn, 2) b 1 
                    |3 -> sowseeds (d, e, f, a', b', c', d', e', f', a, b, 0, spoints, npoints, turn, 3) c 1
                    |4 -> sowseeds (e, f, a', b', c', d', e', f', a, b, c, 0, spoints, npoints, turn, 4) d 1
                    |5 -> sowseeds (f, a', b', c', d', e', f', a, b, c, d, 0, spoints, npoints, turn, 5) e 1
                    |6 -> sowseeds (a', b', c', d', e', f', a, b, c, d, e, 0, spoints, npoints, turn, 6) f 1
                    |7 -> sowseeds (b', c', d', e', f', a, b, c, d, e, f, 0, spoints, npoints, turn, 7) a' 1
                    |8 -> sowseeds (c', d', e', f', a, b, c, d, e, f, a', 0, spoints, npoints, turn, 8) b' 1
                    |9 -> sowseeds (d', e', f', a, b, c, d, e, f, a', b', 0, spoints, npoints, turn, 9) c' 1
                    |10 -> sowseeds (e', f', a, b, c, d, e, f, a', b', c', 0, spoints, npoints, turn, 10) d' 1
                    |11 -> sowseeds (f', a, b, c, d, e, f, a', b', c', d', 0, spoints, npoints, turn, 11) e' 1
                    |12 -> sowseeds (a, b, c, d, e, f, a', b', c', d', e', 0, spoints, npoints, turn, 0) f' 1
                    |_ -> board
                |false -> board
        match pHasNoSeeds newboard with
            |true -> board
            |false -> newboard
    newboard
//UseHouse


let start position = {a = 4; b = 4; c = 4; d = 4; e = 4; f = 4; a' = 4; b' = 4; c' = 4; d' = 4; e' = 4; f' = 4; npoints = 0; spoints = 0; turn = position}

let score board = (board.spoints, board.npoints)

let gameState board = 
    match board.npoints >= 25, board.spoints >= 25, (board.npoints = 24 && board.spoints = 24) with 
        |_, _, true -> "Game ended in a draw"
        |true, true, _ -> "Game ended in a draw"
        |true, false, _ -> "North won"
        |false, true, _ -> "South won"
        |false, false, _ -> match board.turn with 
            |South -> "South's turn"
            |North -> "North's turn"

let print (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset) = 
  printf "\n\t\t\t%i\n\t%i\t%i\t%i\t%i\t%i\t%i\n\t\t      %A\n\t%i\t%i\t%i\t%i\t%i\t%i\n\t\t\t%i\n\n" npoints a b c d e f turn a' b' c' d' e' f' spoints


let rec play uinput game =
        match uinput with
        |1 -> useHouse uinput game
        |2 -> useHouse uinput game
        |3 -> useHouse uinput game
        |4 -> useHouse uinput game
        |5 -> useHouse uinput game
        |6 -> useHouse uinput game
        |7 -> useHouse uinput game
        |8 -> useHouse uinput game
        |9 -> useHouse uinput game
        |10 -> useHouse uinput game
        |11 -> useHouse uinput game
        |12 -> useHouse uinput game
print (4,4,4,4,4,4,4,4,4,4,4,4,0,0,South,0)

[<EntryPoint>]
let main _ =
    play 1 (start South)
    0 // return an integer exit code//print (4,4,4,4,4,4,4,4,4,4,4,4,0,0,South,0) //4,4,4,4,4,4,4,4,4,4,4,4,0,0,South,0

