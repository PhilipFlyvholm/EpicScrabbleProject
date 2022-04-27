// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many (whitespaceChar) <?> "space"
    let spaces1        = many1 (whitespaceChar) <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p = (pchar '(') >*>. p .>*> (pchar ')')
    let curlyBrackets p = (pchar '{') >*>. p .>*> (pchar '}')

    let convertCharListToString (a: char list) = System.String.Concat(a)
    
    let pid = (pchar '_') <|> pletter .>>. many (palphanumeric <|> pchar '_') |>>
              (fun (first, rest) -> convertCharListToString (first::rest))

    let unop op = fun a -> op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2
 
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cAref = createParserForwardedToRef<cExp>()
    
    let BoolTermParse, bTref = createParserForwardedToRef<bExp>()
    let BoolProdParse, bPref = createParserForwardedToRef<bExp>()
    let BoolAtomParse, bAref = createParserForwardedToRef<bExp>()
    let StmntParse1, stmntRef1 = createParserForwardedToRef<stm>()
    let StmntParse2, stmntRef2 = createParserForwardedToRef<stm>()
    
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    let CharToIntParse = unop pCharToInt (parenthesise CharParse) |>> CharToInt  <?> "CharToInt"
    do pref := choice [MulParse;DivParse;ModParse; CharToIntParse; AtomParse]
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let PointValueParse = unop (pPointValue) ParParse |>> PV <?> "Pointvalue"
    let VariablesParse = pid |>> V <?> "Variable"
    let NegParse = unop (pchar '-') TermParse |>> (fun a -> (N -1, a)) |>> Mul <?> "Neg"
    
    do aref := choice [NegParse;NParse;PointValueParse;VariablesParse;ParParse]

    let AexpParse = TermParse
    
    let ToLowerParse = unop pToLower (parenthesise CharParse) |>> ToLower <?> "ToLower"
    let ToUpperParse = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let CharValueParse = pCharValue >*>. parenthesise AtomParse |>> CV  <?> "CV"
    let IntToCharParse = unop pIntToChar ParParse |>> IntToChar  <?> "IntToChar"
    let CParse = (pchar '\'') >>. anyChar .>> (pchar '\'') |>> C <?> "C"
    
    do cAref := choice [ToLowerParse;ToUpperParse;CharValueParse;IntToCharParse;CParse]
    
    let CexpParse = CharParse
    let ConjParse = binop (pstring "/\\") BoolProdParse BoolTermParse |>> Conj <?> "Conj"
    let DisjParse = binop (pstring "\\/") BoolProdParse BoolTermParse |>> (fun (a,b) -> a .||. b) <?> "Disj"
    do bTref := choice [ConjParse; DisjParse; BoolProdParse]
    
    let EqParse = binop (pchar '=') ProdParse TermParse |>> AEq <?> "Equals"
    let NotEqParse = binop (pstring "<>") ProdParse TermParse |>> (fun (a,b) -> a .<>. b) <?> "Not equals"
    let LessThanParse = binop (pchar '<') ProdParse TermParse |>> ALt <?> "Less than"
    let LessThanOrEqualParse = binop (pstring "<=") ProdParse TermParse |>> (fun (a,b) -> a .<=. b) <?> "Less than or equal to"
    let GreaterThanParse = binop (pchar '>') ProdParse TermParse |>> (fun (a,b) -> a .>. b) <?> "Greater than"
    let GreaterThanOrEqualParse = binop (pstring ">=") ProdParse TermParse |>> (fun (a,b) -> a .>=. b) <?> "Greater than or equal to"
    
    do bPref := choice [EqParse; NotEqParse; LessThanParse; LessThanOrEqualParse; GreaterThanParse; GreaterThanOrEqualParse; BoolAtomParse]
    
    let trueParse = pTrue |>> (fun _ -> TT) <?> "True"
    let falseParse = pFalse |>> (fun _ -> FF) <?> "False"
    let notParse = unop (pchar '~') BoolTermParse |>> Not <?> "Not"
    let isLetterParse = unop (pIsLetter) (parenthesise CParse) |>> IsLetter <?> "IsLetter"
    let isDigitParse = unop (pIsDigit) (parenthesise CParse) |>> IsDigit <?> "IsDigit"
    let isVowelParse = unop (pIsVowel) (parenthesise CParse) |>> IsVowel <?> "IsVowel"
    let BoolParParse = parenthesise BoolTermParse <?> "BoolParParse"
    
    do bAref := choice [trueParse;falseParse;notParse;BoolParParse;isLetterParse;isVowelParse;isDigitParse]
    let BexpParse = BoolTermParse
    
    let assignParse = binop (pstring ":=") pid TermParse |>> Ass <?> "Assign"
    let declareParse = pdeclare >>. whitespaceChar >>. pid |>> Declare <?> "Declare"
    let seqParse = binop (pchar ';') StmntParse2 StmntParse1 |>> Seq <?> "Seq"
    let curlyBracketParse = curlyBrackets StmntParse1 <?> "CurlyBracketParse"
    let ifThenElseParse = unop pif (parenthesise BoolTermParse) .>*>. unop pthen curlyBracketParse .>*>. unop pelse StmntParse1 |>> (fun ((bool, ifTrue), ifFalse) -> ITE (bool,ifTrue,ifFalse)) <?> "If-then-else"
    let ifThenParse = unop pif (parenthesise BoolTermParse) .>*>. unop pthen curlyBracketParse |>> (fun (bool, ifTrue) -> ITE (bool, ifTrue, Skip)) <?> "If-then"
    let whileDoParse = unop pwhile (parenthesise BoolTermParse) .>*>. unop pdo curlyBracketParse |>> While <?> "While-do"
    
    do stmntRef1 := choice [seqParse; StmntParse2]
    do stmntRef2 := choice [assignParse; declareParse;ifThenElseParse;ifThenParse;curlyBracketParse;whileDoParse]

    (* The rest of your parser goes here *)
    //TODO do we need to add more parser stuff here?
    let stmntParse = StmntParse1

    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>

    type boardFun2 = coord -> Result<square option, Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    let parseSquareProg (sqp: squareProg) : square =
        Map.map (fun priority str -> stmntToSquareFun (getSuccess(run stmntParse str))) sqp
        
    let stmntToBoardFun stm (m : Map<int, 'a>) : boardFun2 =
        fun (x,y) ->
            stmntEval stm >>>= lookup "_result_" |> evalSM (
                mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"]) |>
                function
                | Success numbr -> if Map.containsKey numbr m then Success(Some m.[numbr]) else Success None
                | Failure erro -> Failure erro
                
    let parseBoardProg = run stmntParse >> getSuccess >> stmntToBoardFun

    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board =
        
        (fun (bp : boardProg) ->
            let m = bp.squares
            let m2 = Map.map (fun _ squareProg -> parseSquareProg squareProg) m
            let defaultSqr = Map.find bp.usedSquare m
            
            {
                center = bp.center
                defaultSquare = parseSquareProg defaultSqr
                squares = parseBoardProg bp.prog m2
            }            
        )