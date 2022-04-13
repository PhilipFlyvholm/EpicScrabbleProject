module ScrabbleBot.Dictionary2

    type Dictionary2

    val empty : unit -> Dictionary2

    val insert : string -> Dictionary2 -> Dictionary2

    val lookup : string -> Dictionary2 -> bool
    
    val step : char -> Dictionary2 -> (bool * Dictionary2) option
    