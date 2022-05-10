module GaddagDictionary

type GdagDict

val empty : unit -> GdagDict

val insert : string -> GdagDict -> GdagDict

val step : char -> GdagDict -> (bool * GdagDict) option

val reverse : GdagDict -> (bool * GdagDict) option

val lookup : string -> GdagDict -> bool