module ScrabbleBot.DictionaryTrie

    type DictionaryTrie =
        | Leaf of bool
        | Node of bool * System.Collections.Generic.Dictionary<char, DictionaryTrie>

    type innerDict = System.Collections.Generic.Dictionary<char, DictionaryTrie>
    
    let empty (_ : unit) = Leaf false

    let rec insert (s: string) =
        function
        | Leaf _ when s.Length = 0 -> Leaf true
        | Node (_, innerDict) when s.Length = 0 -> Node(true, innerDict)
        
        | Leaf b ->
            let newDict = innerDict()
            newDict[s[0]] <- insert s[1..] (empty())
            Node(b, newDict)
            
        | Node(b, dict) ->
            let c = s[0]
            match dict.TryGetValue c with
            | (true, valu) ->
                dict[c] <- insert s[1..] valu
                Node(b, dict)
                
            | (false, _) ->
                dict[c] <- insert s[1..] (empty())
                Node(b, dict)
            
    let rec lookup (s: string) =
        function
        | Leaf b when s.Length = 0 -> b
        | Leaf _ -> false
        | Node (b, _) when s.Length = 0 -> b
        | Node(_, dict) ->
            match dict.TryGetValue s[0] with
            | (true, valu) -> lookup s[1..] valu
            | (false, _) -> false
    
    let step (c: char) =
        function
        | Node (_, dict) ->
            match dict.TryGetValue c with
            | (true, valu) ->
                match valu with
                | Leaf b -> Some (b, valu)
                | Node (b, _) -> Some (b, valu)
            | (false, _) -> None
        | Leaf _ -> None
