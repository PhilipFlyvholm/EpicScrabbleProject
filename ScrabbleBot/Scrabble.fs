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
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state =
        { board: Parser.board
          dict: Dictionary.Dict
          numPlayers : uint32
          playerNumber: uint32
          playerTurn : uint32
          hand: MultiSet.MultiSet<uint32>
          wordMap: Map<coord, uint32 * char>
          drawableTiles: uint32}

    let mkState b d np pn pt h wm dt =
        { board = b
          dict = d
          numPlayers = np
          playerNumber = pn
          playerTurn = pt
          hand = h
          wordMap = wm
          drawableTiles = dt}

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let playerTurn st = st.playerTurn
    let hand st = st.hand
    let wordMap st = st.wordMap

module Scrabble =
    //type with direction
    type direction =
        | Up | Down | Left | Right
    let isOtherWordsInTheWay (x,y) (st:State.state) (chr:char) =
        let rec goBackward dir acc (x', y') =
            let newCoord =
                match dir with
                | Up | Down -> (x'-1, y')
                | Left | Right -> (x', y'-1)
            match st.wordMap.TryFind newCoord with
            | Some (_, c) -> goBackward dir (string(c) + acc) newCoord
            | None -> acc
        let rec goForward dir acc (x', y') =
            let newCoord =
                match dir with
                | Up | Down -> (x'+1, y')
                | Left | Right -> (x', y'+1)
            match st.wordMap.TryFind newCoord with
            | Some (_, c) -> goForward dir (acc + string(c)) newCoord
            | None -> acc
        fun dir ->
            let backWord = goBackward dir "" (x,y)
            let forwardWord = goForward dir "" (x,y)
            let currentWord = backWord + string(chr) + forwardWord
            if currentWord.Length > 1 && not (currentWord.Equals(string(chr))) then
                not (Dictionary.lookup currentWord st.dict) //if the word is not in the dictionary, then it is in the way
            else
                false //There is nothing in the way
                
    let findCurrentWordInDirection coord (st:State.state) (dir:direction) : Dictionary.Dict=
        let rec aux (f:Dictionary.Dict -> Dictionary.Dict) ((x,y):coord) (dict:Dictionary.Dict) = 
            let newCoord =
                match dir with
                | Up -> (x,y-1)
                | Down -> (x,y+1)
                | Left -> (x-1,y)
                | Right -> (x+1,y)
            
            let newLetter = Map.tryFind newCoord st.wordMap
            
            match newLetter with
            | Some (_,c) ->
                aux (fun x ->
                            let newDict = match (Dictionary.step c x) with
                                          | Some (_, dict) -> dict
                                          | None -> failwith "Failed (should never happen)"
                            f newDict
                        ) newCoord dict
            | None -> f dict
        aux id coord st.dict
        
    //Returns ((charId * (char * pointValue)) list * wordValue) list
    let findMove (st : State.state) (pieces : Map<uint32, 'a>) (dir : direction) (startCoord : coord) (startDict : Dictionary.Dict) =

        let rec aux (dict: Dictionary.Dict) (chrList: MultiSet.MultiSet<uint32>) (currentItem : ((coord * uint32 * (char * int)) list) * int) ((x,y):coord) =
             MultiSet.fold
                    (fun acc id _ ->
                                   
                        let preSeq = (Map.find id pieces)
                        
                        Seq.fold (fun acc2 (chr, pointValue) ->
                            
                            let nextCoord : coord =
                                match dir with
                                | Up -> (x,y-1) //TODO: This will not work since the word would be in reverse (Gaddag would maybe fix)
                                | Left -> (x-1,y) //TODO: This will not work since the word would be in reverse (Gaddag would maybe fix)
                                | Right -> (x+1,y)
                                | Down -> (x,y+1)
                               
                            match Map.tryFind nextCoord st.wordMap with
                                | Some _ -> acc //Can't go further this direction
                                | None -> 
                                    let innerDict = Dictionary.step chr dict
                                    let newChrList = MultiSet.removeSingle id chrList
                                    
                                    match innerDict with
                                    | Some (isFullWord, newDict) ->
                                        let currentWord = fst(currentItem)
                                        let currentScore = snd(currentItem)
                                        let newWord = (currentWord@[(x,y), id, (chr, pointValue)], currentScore + pointValue)
                                        
                                        if isFullWord then
                                            newWord::acc@(aux newDict newChrList newWord nextCoord)
                                        else
                                            acc@(aux newDict newChrList newWord nextCoord)
                                    | None -> acc
                                
                        ) List.empty preSeq
                    )
                    List.empty
                    chrList
            
        aux startDict st.hand (List.empty, 0) startCoord
        
    let findBoardMoves (st: State.state) (pieces: Map<uint32, 'a>) =
       Map.fold (fun acc (x,y) (id, chr) -> (                         
                            //If we want to look right then we need to find the letters on left first
                            let rightDict = findCurrentWordInDirection (x,y) st Left 
                           //If we want to look down then we need to find the letters on up first
                            let downDict = findCurrentWordInDirection (x,y) st Up 
                            
                            let rightAcc = match (Dictionary.step chr rightDict) with
                                            | Some (_, dict) -> findMove st pieces Right (x+1,y) dict 
                                            | None -> []
                            let downAcc =  match (Dictionary.step chr downDict) with
                                            | Some (_, dict) -> findMove st pieces Down (x,y+1) dict 
                                            | None -> []
                            rightAcc @ downAcc @ acc               
                            
                )) List.empty st.wordMap
       
    let updatePlayerTurn (playerTurn : uint32) (numPlayers : uint32) =
        match playerTurn with
        | x when x >= numPlayers   -> 1u
        | _                         -> playerTurn + 1u
        
    let playGame cstream pieces (st: State.state) =
        let rec aux (st: State.state) =
        
            debugPrint (sprintf "Current turn: %d - Says player nr. %d\n" st.playerTurn st.playerNumber)
            let removedTiles = MultiSet.empty
            if st.playerTurn = st.playerNumber then
                forcePrint "Current hand: \n"
                Print.printHand pieces st.hand
                // remove the force print when you move on from manual input (or when you have learnt the format)
                //forcePrint
                //    "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                
                let moves = if st.wordMap.Count = 0 then
                                let result = findMove st pieces Right (0,0) st.dict
                                List.map (fun (item, score) ->
                                    List.map (fun (coord, id, letters) -> coord, (id, letters)) item
                                ) result
                            else
                                let result = findBoardMoves st pieces
                                
                                debugPrint("Result is: " + result.ToString())
                                
                                List.map (fun (item, score) ->
                                    List.map (fun (coord, id, letters) -> coord, (id, letters)) item
                                ) result

                let rec auxFindMove i =
                        let wordsInTheWay =
                            List.fold (
                                fun acc ((x,y),(id, (chr, _))) ->
                                    if acc then
                                        acc
                                    else
                                        match Map.tryFind (x,y) st.wordMap with
                                        | Some _ -> true
                                        | None ->
                                            let right = isOtherWordsInTheWay (x,y) st chr Right
                                            let down = isOtherWordsInTheWay (x,y) st chr Down
                                            right || down
                            ) false moves[moves.Length-i]
                        if wordsInTheWay && moves.Length > i then
                            auxFindMove (i+1)
                        else if moves.Length <= i then
                            []
                        else
                            moves[moves.Length-i]
                        
                
                let move =
                    if moves.Length > 0 then
                        auxFindMove 1
                    else
                        []
                        
                let printableWord =
                    (List.fold
                        (fun acc (_, (_, (chr, point))) ->  acc + string(chr))
                         "" move
                    )
                //ENTER MODE = YOU NEED TO PRESS ENTER BETWEEN MOVES TO TEST
                let enterMode = false
                
                if(enterMode) then
                    debugPrint (sprintf "Press enter to play %A \n" printableWord)
                    let input = System.Console.ReadLine()
                    ()
                
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                let mutable removedTiles = st.hand
                if move.Length > 0 then
                    send cstream (SMPlay move)
                else
                     debugPrint "No legal moves!"
                     if (MultiSet.size st.hand < st.drawableTiles) then
                        send cstream (SMChange (MultiSet.toList st.hand))
                     else
                        removedTiles <- MultiSet.ofList((MultiSet.toList st.hand)[0..(int st.drawableTiles - 1)])
                        send cstream (SMChange (MultiSet.toList(removedTiles)) )

                debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream
            
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
                             let drawableTiles' = acc.drawableTiles - 1u
                             
                             //TODO ADD SCORE TO STATE
                             State.mkState acc.board acc.dict acc.numPlayers acc.playerNumber acc.playerTurn hand' wordMap' drawableTiles'))
                        st
                        ms)
                    |> List.fold
                        (fun acc (id, amount) ->
                            (
                             //fold through pieces given
                             let hand' = MultiSet.add id amount acc.hand
                             State.mkState acc.board acc.dict acc.numPlayers acc.playerNumber acc.playerTurn hand' acc.wordMap acc.drawableTiles))
                    <| newPieces
                
                let st' = State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber (updatePlayerTurn st'.playerTurn st'.numPlayers) st'.hand st'.wordMap st'.drawableTiles
                aux st'
            | RCM (CMChangeSuccess (newTiles)) ->
                let handMinusTiles = MultiSet.subtract removedTiles st.hand
                debugPrint (sprintf "New tiles: %A\n" newTiles)
                let newHand = List.fold (fun acc (tile,amount) -> MultiSet.add tile amount acc) handMinusTiles newTiles
                
                let st' = State.mkState st.board st.dict st.numPlayers st.playerNumber (updatePlayerTurn st.playerTurn st.numPlayers) newHand st.wordMap st.drawableTiles
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //TODO UPDATE STATE BOARD
                let st' = List.fold
                            (fun (acc: State.state) (coords, (id, (chr, value))) ->
                                (
                                    //Fold through pieces placed
                                    let wordMap' = Map.add coords (id, chr) acc.wordMap
                                    
                                    State.mkState acc.board acc.dict acc.numPlayers acc.playerNumber acc.playerTurn acc.hand wordMap' acc.drawableTiles
                                )
                            ) st ms
                            
                let st' = State.mkState st'.board st'.dict st'.numPlayers st'.playerNumber (updatePlayerTurn st'.playerTurn st'.numPlayers) st'.hand st'.wordMap st'.drawableTiles
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = State.mkState st.board st.dict st.numPlayers st.playerNumber (updatePlayerTurn st.playerTurn st.numPlayers) st.hand st.wordMap st.drawableTiles
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMChange(playerId, numberOfTiles)) ->
                let st' = State.mkState st.board st.dict st.numPlayers st.playerNumber (updatePlayerTurn st.playerTurn st.numPlayers) st.hand st.wordMap st.drawableTiles
                aux st'
            | RCM (CMForfeit(playerId)) ->
                let st' = State.mkState st.board st.dict st.numPlayers (st.playerNumber-1u) (updatePlayerTurn st.playerTurn st.numPlayers) st.hand st.wordMap st.drawableTiles
                aux st'
            | RCM (CMPassed(playerId)) ->
                let st' = State.mkState st.board st.dict st.numPlayers st.playerNumber (updatePlayerTurn st.playerTurn st.numPlayers) st.hand st.wordMap st.drawableTiles
                aux st'
            | RCM (CMTimeout(playerId)) ->
                let st' = State.mkState st.board st.dict st.numPlayers st.playerNumber (updatePlayerTurn st.playerTurn st.numPlayers) st.hand st.wordMap st.drawableTiles
                aux st'
            | RGPE [(GPENotEnoughPieces (changeTiles, availableTiles))] ->
                let st' = State.mkState st.board st.dict st.numPlayers st.playerNumber (updatePlayerTurn st.playerTurn st.numPlayers) st.hand st.wordMap availableTiles
                aux st'
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                let st' = State.mkState st.board st.dict st.numPlayers st.playerNumber (updatePlayerTurn st.playerTurn st.numPlayers) st.hand st.wordMap st.drawableTiles
                aux st'

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

        let handSet =
            List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn handSet Map.empty 100u)