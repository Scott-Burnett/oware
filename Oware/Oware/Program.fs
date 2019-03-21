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

//let houseToNum n =
// match n with
// | 1 -> 'a'
// | 2 -> 'b'
// | 3 -> 'c'
// | 4 -> 'd'
// | 5 -> 'e'
// | 6 -> 'f'
// | 7 -> 'a'
// | 8 -> 'b'
// | 9 -> 'c'
// | 10 -> 'd'
// | 11 -> 'e'
// | 12 -> 'f'

let useHouse n board = 
    let SeedsToBeUsed = getSeeds n board
    let rec bob move = 
       match (move>0) with 
       |true -> 
        {a = 0; b = 5; c = 5; d = 5; e = 5; f = 4; a' = 4; b' = 4; c' = 4; d' = 4; e' = 4; f' = 4}//set selected house to 0, get the current house, set to 0, copy and update (add 1 to) (n+1)
       | _ -> failwith "UseHouse, recursive part: move=<0"
    match (n>0 && n<13) with
    |true -> bob SeedsToBeUsed
    |_ -> failwith "UseHouse, n<0 or n>12"
    
        


let start position = {a = 4;b = 4; c = 4; d = 4; e = 4; f = 4; a' = 4; b' = 4; c' = 4; d' = 4; e' = 4; f' = 4}

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
