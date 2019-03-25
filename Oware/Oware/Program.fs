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
}

type points = 
    | Spoints of int 
    | Npoints of int


let getSeeds n board = 
    match n with 
    | 1 -> board.a
    | 2 -> board.b
    | 3 -> board.c
    | 4 -> board.d
    | 5 -> board.e
    | 6 -> board.f
    | 7 -> board.a'
    | 8 -> board.b'
    | 9 -> board.c'
    | 10 -> board.d'
    | 11 -> board.e'
    | 12 -> board.f'
    | _ -> failwith "Getseeds, Out of range exception: n>12"


let useHouse n board = 
    let SeedsToBeUsed = getSeeds n board
    let bob num =
        match (num > 0) with
        |true -> 1
        |_ -> 0
    match (n>0 && n<13) with
    |true -> match (n) with 
       |1 -> {a = 0;b = (getSeeds (n+1) board) + bob SeedsToBeUsed; c = (getSeeds (n+2) board)+bob (SeedsToBeUsed-1); d = (getSeeds (n+3) board)+bob (SeedsToBeUsed-2); e = (getSeeds (n+4) board)+bob (SeedsToBeUsed-3); f = (getSeeds (n+5) board)+bob (SeedsToBeUsed-4); a' = (getSeeds (n+6) board)+bob (SeedsToBeUsed-5); b' = (getSeeds (n+7) board)+bob (SeedsToBeUsed-6); c' = (getSeeds (n+8) board)+bob (SeedsToBeUsed-7); d' = (getSeeds (n+9) board)+bob (SeedsToBeUsed-8); e' = (getSeeds (n+10) board)+bob (SeedsToBeUsed-9); f' = (getSeeds (n+11) board)+bob (SeedsToBeUsed-10)} 
              
       | _ -> failwith "UseHouse, recursive part: move=<0"
    |_ -> failwith "UseHouse, n<0 or n>12"  
    
   
    
        


let start position = {a = 4;b = 4; c = 4; d = 4; e = 4; f = 4; a' = 4; b' = 4; c' = 4; d' = 4; e' = 4; f' = 4}

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
