// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison>

    val internal empty : MultiSet<'a>
    val internal isEmpty : MultiSet<'a> -> bool
    val internal size : MultiSet<'a> -> uint32
    val internal contains : 'a -> MultiSet<'a> -> bool
    val internal numItems : 'a -> MultiSet<'a> -> uint32
    val internal add : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val internal addSingle : 'a -> MultiSet<'a> -> MultiSet<'a>
    val internal remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val internal removeSingle : 'a -> MultiSet<'a> -> MultiSet<'a>
    val internal fold : ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a
    val internal foldBack : ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b
    val internal ofList : 'a list -> MultiSet<'a>
    val internal toList : MultiSet<'a> -> 'a list
    val internal map : ('a -> 'b) -> MultiSet<'a> -> MultiSet<'b>
    val internal union : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val internal sum : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val internal subtract : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val internal intersection : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>