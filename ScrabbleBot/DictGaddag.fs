module GaddagDictionary

type GdagDict =
        | Leaf of bool
        | Node of bool * System.Collections.Generic.Dictionary<char, GdagDict>

type innerGaddagDictionary = System.Collections.Generic.Dictionary<char, GdagDict>

let empty (_ : unit) = Leaf false

let rec ert (str : string) =
    (str.ToCharArray())

let reverseString (str : string) =
    System.String (Array.rev <| str.ToCharArray())
    
let rec trieInsert (str : string) (gdagDict : GdagDict) =
    match gdagDict with
    | Leaf _            when str.Length = 0 -> Leaf true
    | Node (_, dict)    when str.Length = 0 -> Node(true, dict)
    | Leaf b ->
        let tempDict = innerGaddagDictionary()
        tempDict.[str.[0]] <- trieInsert str.[1..] (empty())
        Node(b, tempDict)
    | Node (b, innerGaddagDictionary) ->
        let c = str.[0]
        match innerGaddagDictionary.TryGetValue c with
        | (false, _) ->
            innerGaddagDictionary.[c] <- trieInsert str.[1..] (empty())
            Node(b, innerGaddagDictionary)
        | (true, innerTrieDict) ->
            innerGaddagDictionary.[c] <- trieInsert str.[1..] innerTrieDict
            Node(b, innerGaddagDictionary)
            
let rec insert (str : string) (gdagDict : GdagDict) =
    let revStr = reverseString str
    Array.fold (fun acc s -> trieInsert s acc) gdagDict [| for i in 0 .. str.Length-1 -> revStr.[revStr.Length - i - 1..].ToString() + "#" + str.[i+1..] |]

let step (c: char) (gdagDict : GdagDict) =
    match gdagDict with
    | Node (_, innerGaddagDictionary) ->
        match innerGaddagDictionary.TryGetValue c with
            | (true, innerInnerGaddagDictionary) ->
                match innerInnerGaddagDictionary with
                | Leaf b     -> Some (b, innerInnerGaddagDictionary)
                | Node (b,_) -> Some (b, innerInnerGaddagDictionary)
            | (false, _) -> None
    | Leaf _ -> None

let reverse (gdagDict : GdagDict) =
    step '#' gdagDict


let flatmap f = Option.map f >> Option.flatten

let lookup (str : string) (gdagDict : GdagDict) =
    let rec lookupHelp =
        function
        | [] -> fun _ -> failwith "This can never happen"
        | [x] -> step x
        | x :: xs ->
            step x >>
            flatmap (snd >> lookupHelp xs)
    let rec lookupsHelp acc back =
        function
        | [] -> fun _ -> Some false
        | [x] ->
            lookupHelp (x :: back) >>
            flatmap (snd >> reverse) >>
            Option.map (fun (b, _) -> acc && b)
        | x :: xs ->
            lookupHelp (x :: back) >>
            flatmap (snd >> reverse) >>
            flatmap (snd >> lookupHelp xs) >>
            flatmap (fun (b, _) -> lookupsHelp (acc && b) (x :: back) xs gdagDict)
    gdagDict |>
    lookupsHelp true [] [for c in str -> c] |>
    (=) (Some true)

