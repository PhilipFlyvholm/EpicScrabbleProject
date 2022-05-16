module ScrabbleBot.DictionaryTrie

    type DictionaryTrie

    val empty : unit -> DictionaryTrie

    val insert : string -> DictionaryTrie -> DictionaryTrie

    val lookup : string -> DictionaryTrie -> bool
    
    val step : char -> DictionaryTrie -> (bool * DictionaryTrie) option
    