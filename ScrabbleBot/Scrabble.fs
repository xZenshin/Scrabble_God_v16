namespace ScrabbleGod

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        boardState    : Map<coord, (uint32 * (char * int))>
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playerList    : List<uint32>
        currentPlayer : uint32
    }


    let updateHand (ms: (uint32 * uint32) list) (newPieces: (uint32 * uint32) list) (currentHand: MultiSet.MultiSet<uint32>) = 
        (ms |> List.fold(fun cH b -> MultiSet.remove (fst b) (snd b) cH) currentHand
        |> List.fold(fun cH a -> MultiSet.add (fst a) (snd a) cH)) newPieces

    let updateBoardState prevState newMoves =
        List.fold (fun acc next -> Map.add (fst next) (snd next) acc) prevState newMoves

    let nextPlayer (pl: List<uint32>) (cp: uint32) = 
        if cp+1u = uint32 pl.Length then 0u
        else cp+1u



    let mkState b bs d pn h pl cp = {board = b; boardState = bs; dict = d;  playerNumber = pn; hand = h; playerList = pl; currentPlayer = cp}
    let boardState st    = st.boardState
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerList st    = st.playerList
    let currentPlayer st = st.currentPlayer

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                //Extracting piece ID and count 
                let mss = ms |> List.map(fun (_, (pid, (char, pv) )) -> (pid,1u))

                let newHand = State.updateHand mss newPieces st.hand
                let newBoardState = State.updateBoardState st.boardState ms
                let nextPlayer = State.nextPlayer st.playerList st.currentPlayer


                let st' = State.mkState st.board newBoardState st.dict st.playerNumber newHand st.playerList nextPlayer
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let newBoardState = State.updateBoardState st.boardState ms
                let nextPlayer = State.nextPlayer st.playerList st.currentPlayer


                let st' = State.mkState st.board newBoardState st.dict pid st.hand st.playerList nextPlayer
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg)
            (dictf : bool -> Dictionary.Dict)  
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let pl = [uint32 1 ..  numPlayers]

        fun () -> playGame cstream tiles (State.mkState board Map.empty<coord, (uint32 * (char * int))> dict playerNumber handSet pl 0u)
        