module Oware

open System.IO
open System.Net

type StartingPosition =
    | South
    | North
//StartingPosition

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
//board

let start position = {a = 4; b = 4; c = 4; d = 4; e = 4; f = 4; a' = 4; b' = 4; c' = 4; d' = 4; e' = 4; f' = 4; npoints = 0; spoints = 0; turn = position}
//start

let score board = (board.spoints, board.npoints)
//score

let gameState board = 
    match board.npoints >= 25, board.spoints >= 25, (board.npoints = 24 && board.spoints = 24) with 
        |_, _, true -> "Game ended in a draw"
        |true, true, _ -> "Game ended in a draw"
        |true, false, _ -> "North won"
        |false, true, _ -> "South won"
        |false, false, _ -> match board.turn with 
            |South -> "South's turn"
            |North -> "North's turn"
//gameState
    
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
//getSeeds

let updateConsole ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = spoints; npoints = npoints; turn = turn} as board) message =
    System.Console.Clear ()
    System.Console.WriteLine ("\n\t\t---------------------------------------------------------------\n\t\t\t\t\t " + gameState board + "\n\t\t===============================================================")
    System.Console.WriteLine ("\t\t                            NORTH")
    System.Console.WriteLine ("\t\t--------------------------POINTS: {0}----------------------------", npoints)
    System.Console.WriteLine ("\t\t ||  ||                                                 ||  ||")
    System.Console.WriteLine ("\t\t ||  ||  12 [{0}]\t11 [{1}]\t10 [{2}]\t9  [{3}]\t8  [{4}]\t7  [{5}]  ||  ||", f', e', d', c', b', a'  )
    System.Console.WriteLine ("\t\t=====||=================================================||=====")
    System.Console.WriteLine ("\t\t ||  ||  1  [{0}]\t2  [{1}]\t3  [{2}]\t4  [{3}]\t5  [{4}]\t6  [{5}]  ||  ||", a, b, c, d, e, f)
    System.Console.WriteLine ("\t\t ||  ||                                                 ||  ||")
    System.Console.WriteLine ("\t\t--------------------------POINTS: {0}----------------------------", spoints)
    System.Console.WriteLine ("\t\t                           SOUTH")
    System.Console.WriteLine ("\t\t===============================================================\n\t\t\t\t" + message + "\n\t\t---------------------------------------------------------------")
    ()
//updateConsole

let readInput () =
    int32 (System.Console.ReadLine())


let houseBelongsToPlayer n board =
    ((n > 0 && n < 7 && board.turn = South) || (n > 5 && n < 13 && board.turn = North))
//houseBelongsToPlayer

let houseIsNotEmpty n board = 
    (getSeeds n board) > 0
//houseIsNotEmpty

let turnWasAllowed ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; turn = turn } as board) =
    match turn with 
        |South -> match (a = 0 && b = 0 && c = 0 && d = 0 && e = 0 && f = 0) with
                    |true -> (a' = 0 && b' = 0 && c' = 0 && d' = 0 && e' = 0 && f' = 0)
                    |false -> true
        |North -> match (a' = 0 && b' = 0 && c' = 0 && d' = 0 && e' = 0 && f' = 0) with 
                    |true -> (a = 0 && b = 0 && c = 0 && d = 0 && e = 0 && f = 0)
                    |false -> true
//turnWasAllowed

let pHasNoSeeds ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; turn = turn} as board) =
    (a = 0 && b = 0 && c = 0 && d = 0 && e = 0 && f = 0 && turn = South) || (a' = 0 && b' = 0 && c' = 0 && d' = 0 && e' = 0 && f' = 0 && turn = North)
//pHasNoSeeds

let isEmpty ({spoints = spoints; npoints = npoints} as board) = 
    (spoints + npoints) = 48
//isEmpty

let nextTurn ({turn = turn} as board) =
    match turn with
        |South -> {board with turn = North}
        |North -> {board with turn = South}
//nextTurn

let getboard (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset) =            
    let rec reset a b c d e f a' b' c' d' e' f' spoints npoints turn offset =
        match offset = 0 with 
            |true -> { a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = spoints; npoints = npoints; turn = turn }
            |false -> reset f' a b c d e f a' b' c' d' e' spoints npoints turn ((offset + 11) % 12)
    reset a b c d e f a' b' c' d' e' f' spoints npoints turn offset
//getboard

let getpoints (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset) = 
    let justInCaseBoard = nextTurn (getboard (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset))
    let rec inner a b c d e f a' b' c' d' e' f' spoints npoints turn offset =
        match (a = 2 || a = 3), (offset >= 0 && offset <= 5 && turn = North), (offset >= 6 && offset <= 11 && turn = South) with
            |true, true, false -> inner f' 0 b c d e f a' b' c' d' e' spoints (npoints + a) turn ((offset + 11) % 12)
            |true, false, true -> inner f' 0 b c d e f a' b' c' d' e' (spoints + a) npoints turn ((offset + 11) % 12)
            |_, _, _ -> let nextboard = nextTurn (getboard (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset))
                        match turnWasAllowed nextboard with
                            |true -> (nextboard, "Please choose a house to sow from")
                            |false -> (justInCaseBoard, "Cannot capture all of your opponent's seeds")
    inner a b c d e f a' b' c' d' e' f' spoints npoints turn offset
//getpoints

let rec sowseeds (a, b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset) numseeds cycles = 
    let rec inner a b c d e f a' b' c' d' e' f' spoints npoints turn offset numseeds cycles =
        match numseeds = 1, (cycles = 0) with
            |_, true -> inner b c d e f a' b' c' d' e' f' a spoints npoints turn ((offset + 1) % 12) numseeds ((cycles + 1) % 12)
            |false, false -> inner b c d e f a' b' c' d' e' f' (a + 1) spoints npoints turn ((offset + 1) % 12) (numseeds - 1) ((cycles + 1) % 12)
            |true, false -> getpoints ((a + 1), b, c, d, e, f, a', b', c', d', e', f', spoints, npoints, turn, offset)        
    inner a b c d e f a' b' c' d' e' f' spoints npoints turn offset numseeds cycles
//sowseeds

let noValidTurns ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; turn = turn} as board) =
    match turn = South with
        |true -> (a < 6 && b < 5 && c < 4 && d < 3 && e < 2 && f < 1) && ( a' = 0 && b' = 0 && c' = 0 && d' = 0 && e' = 0 && f' = 0)
        |false -> (a' < 6 && b' < 5 && c' < 4 && d' < 3 && e' < 2 && f' < 1) && ( a = 0 && b = 0 && c = 0 && d = 0 && e = 0 && f = 0)
//noValidTurns

let cleanUp ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = spoints; npoints = npoints; turn = turn} as board) =
    {a = 0; b = 0; c = 0; d = 0; e = 0; f = 0; a' = 0; b' = 0; c' = 0; d' = 0; e' = 0; f' = 0; spoints = (spoints + a + b + c + d + e + f); npoints = (npoints + a' + b' + c' + d' + e' + f'); turn = turn }
//cleanUp

let useHouse n ({a = a; b = b; c = c; d = d; e = e; f = f; a' = a'; b' = b'; c' = c'; d' = d'; e' = e'; f' = f'; spoints = spoints; npoints = npoints; turn = turn } as board) = 
    let (newboard, message) = 
        match noValidTurns board with
            |true -> (cleanUp board, "")
            |false -> let (newboard, message) =
                          match (houseBelongsToPlayer n board), (houseIsNotEmpty n board) with
                              |true, true -> match n with 
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
                                      |_ -> (board, "Out of range")  
                              |false, true -> (board, "You cannot choose your opponants house")        
                              |_, false -> (board, "You cannot sow from an empty house") 
                      match not (turnWasAllowed newboard) with
                          |true -> (board, "You must play to give your opponent pieces if they have none") 
                          |false -> newboard, message  
    updateConsole newboard message                      
    newboard
//useHouse

let play board =
    let rec inner board =
        match gameState board with
        |"North's turn"|"South's turn" -> inner (useHouse (readInput ()) board)                             
        |_ -> board
    inner board


[<EntryPoint>]
let main _ =
    let board = start South
    let nothing = updateConsole board ""
    let anothernothing = play board
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
