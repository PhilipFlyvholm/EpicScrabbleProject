namespace TheCheaterBot

open ScrabbleBot
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern =
            @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map
            (fun t ->
                match t.Value with
                | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state =
        { board: Parser.board
          dict: ScrabbleUtil.Dictionary.Dict
          playerNumber: uint32
          hand: MultiSet.MultiSet<uint32>
          wordMap: Map<coord, (uint32 * char)>}

    let mkState b d pn h wm =
        { board = b
          dict = d
          playerNumber = pn
          hand = h
          wordMap = wm}

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand
    let wordMap st = st.wordMap

module Scrabble =
    open System.Threading

    //Returns ((charId * (char * pointValue)) list * wordValue) list
    let findMove (st: State.state) (pieces: Map<uint32, 'a>) =

        let rec aux (dict: Dictionary.Dict) (chrList: MultiSet.MultiSet<uint32>) (currentWord : ((uint32 * (char * int)) list * int)) =
             MultiSet.fold
                    (fun acc id amount -> //TODO How should amount be handled?
                        
                        let set = (Map.find id pieces) |> Seq.head
                        
                        let chr = set |> fst
                        let pointValue = set |> snd
                        
                        let innerDict = Dictionary.step chr dict
                        let newChrList = MultiSet.removeSingle id chrList
                        match innerDict with
                        | Some (b, newDict) ->
                            let newWord = (fst(currentWord)@[id, (chr, pointValue)], snd(currentWord) + pointValue)
                            if b then
                                newWord::acc@(aux newDict newChrList newWord)
                            else
                                acc@(aux newDict newChrList newWord)
                        | None -> acc
                    )
                    List.empty
                    chrList
            
        aux st.dict st.hand (List.empty, 0)
        
    let findBoardMoves (st: State.state) (pieces: Map<uint32, 'a>) =
       Map.fold (fun acc (x,y) (id, chr) -> (
                            //let seqChr = seq {(chr, 0)}
                            //let cPieces = Map.add id seqChr pieces
                            let cPieces = Map.add id (Set.add (chr, 0) Set.empty) pieces
                            
                            match Map.tryFind (x,y) acc with
                            | Some v -> Map.add (x,y) (v @ findMove st cPieces) acc
                            | None -> Map.add (x,y) (findMove st cPieces) acc
                            
                )) Map.empty st.wordMap
        
    let playGame cstream pieces (st: State.state) =

        let rec aux (st: State.state) =
            Print.printHand pieces (State.hand st)
            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint
                "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

            let moves = if st.wordMap.Count = 0 then
                            let result = findMove st pieces
                            List.map (fun word ->
                                List.mapi (fun i word -> (0,i),word) (fst(word))
                            ) result
                        else
                            let result = findBoardMoves st pieces
                            Map.fold (fun acc (x,y) words ->
                                (List.fold (fun innerAcc word ->
                                    innerAcc @ List.mapi (fun i word2 -> (x,y + i), word2) (fst(word))
                                ) List.empty words) :: acc
                            ) List.empty result
            
            //List.fold (fun acc str ->
              //              debugPrint (sprintf "%A \n" str)
                //        ) () result[0..10]

            //let input = System.Console.ReadLine()
            //let move = RegEx.parseMove input
            let move = moves.Head
            
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess (ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                // This state needs to be updated
                let st' =
                    (List.fold
                        (fun (acc: State.state) (coords, (id, (chr, value))) ->
                            (
                             //Fold through pieces placed
                             let hand' = MultiSet.removeSingle id acc.hand
                             let wordMap' = Map.add coords (id, chr) acc.wordMap
                             
                             //TODO UPDATE BOARD
                             //TODO ADD SCORE TO STATE
                             State.mkState acc.board acc.dict acc.playerNumber hand' wordMap'))
                        st
                        ms)
                    |> List.fold
                        (fun acc (id, amount) ->
                            (
                             //fold through pieces given
                             let hand' = MultiSet.add id amount acc.hand
                             State.mkState acc.board acc.dict acc.playerNumber hand' acc.wordMap))
                    <| newPieces

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //TODO UPDATE STATE BOARD
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st


        aux st

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        Dictionary2.empty

        let handSet =
            List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
