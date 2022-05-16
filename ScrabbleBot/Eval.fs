// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let add a b =  a >>= fun x -> b >>= fun y -> ret (x+y)
    let sub a b =  a >>= fun x -> b >>= fun y -> ret (x-y)
    let div a b = a >>= fun x ->
        b >>= fun y ->
            if y <> 0 then ret (x / y) else fail DivisionByZero    
    let mul a b =  a >>= fun x -> b >>= fun y -> ret (x*y)
    
    let modulo a b = a >>= fun x ->
        b >>= fun y ->
            if y <> 0 then ret (x % y) else fail DivisionByZero     

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp (* check for Consonant *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV exp -> arithEval exp >>= pointValue
        | Add (a, b) -> add (arithEval a) (arithEval b)
        | Sub (a, b) -> sub (arithEval a) (arithEval b)
        | Div (a, b) -> div (arithEval a) (arithEval b)
        | Mul (a, b) -> mul (arithEval a) (arithEval b)
        | CharToInt cExp -> (charEval cExp) >>= (fun s -> ret (int s))
        | Mod (a, b) -> modulo (arithEval a) (arithEval b)
    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV aExp -> (arithEval aExp) >>= characterValue
        | ToUpper cExp -> (charEval cExp) >>= (fun c -> ret (System.Char.ToUpper c))
        | ToLower cExp -> (charEval cExp) >>= (fun c -> ret (System.Char.ToLower c))
        | IntToChar aExp -> (arithEval aExp) >>= (fun i -> ret (char (int '0' + i)))
    and boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (aExp1, aExp2) -> arithEval aExp1 >>= (fun a -> arithEval aExp2 >>= (fun b -> ret (a = b)))
        | ALt (aExp1, aExp2) -> arithEval aExp1 >>= (fun a -> arithEval aExp2 >>= (fun b -> ret (a < b)))
        | Not exp -> boolEval exp >>= (fun b -> ret (not b))
        | Conj (bExp1, bExp2) -> boolEval bExp1 >>= (fun a -> boolEval bExp2 >>= (fun b -> ret (a && b)))
        | IsDigit cExp -> charEval cExp >>= (fun c -> ret (System.Char.IsDigit c))
        | IsLetter cExp -> charEval cExp >>= (fun c -> ret (System.Char.IsLetter c))
        | IsVowel cExp -> charEval cExp >>= (fun c -> ret (not ("BCDFGHJKLMNPQRSTVWXYZ".Contains(System.Char.ToUpper(c)))))
        | IsConsonant cExp -> charEval cExp >>= (fun c -> ret ("BCDFGHJKLMNPQRSTVWXYZ".Contains(System.Char.ToUpper(c))))


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare str -> declare(str)
        | Ass (str, axp) -> arithEval axp >>= update str
        | Skip -> ret()
        | Seq (stm1, stm2) -> stmntEval stm1 >>>= stmntEval stm2 >>= (fun result -> ret(result))
        | ITE (bxp, stm1, stm2) -> push >>>= boolEval bxp >>= fun booler ->
            if booler then
                stmntEval stm1
            else
                stmntEval stm2 >>>= pop
        | While (bxp, stm) -> push >>>= boolEval bxp >>= (fun booler -> if booler then stmntEval stm >>>= stmntEval (While (bxp, stm)) else ret()) >>>= pop

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()
    
    let binop2 func a b =
        prog { let! x = a
               let! y = b
               return func x y }

    let arithEval2 a : SM<int> =
        match a with
        | N n -> prog {return n}
        | V x -> prog {return! lookup x}
        | WL -> prog {return! wordLength}
        | PV ax -> prog {
            let! a = arithEval ax
            return! pointValue a
            }    
        | Add (a1, a2) -> binop2 ( + ) (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> binop2 ( - ) (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> binop2 ( * ) (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> prog {
            let! a = arithEval a1
            let! b = arithEval a2
            if b <> 0 then
                return (a / b)
            else
                return! fail DivisionByZero
            }
        | Mod (a1, a2) ->
            prog {
                let! a = arithEval a1
                let! b = arithEval a2
                if b <> 0 then
                    return (a % b)
                else
                    return! fail DivisionByZero
            }
             
        | CharToInt cxp -> prog {
                let! r = charEval cxp
                return int r
            }
        
    let charEval2 c = charEval c
    let rec boolEval2 b = boolEval b

    let stmntEval2 stm = stmntEval stm

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>

    let stmntToSquareFun (stm : stm) =
        fun (wrd: word) (pos: int) (acc: int) ->
            stmntEval stm >>>= lookup "_result_" |> evalSM (
                mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] wrd ["_pos_"; "_acc_"; "_result_"]
            )

    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm (m:Map<int, squareFun>) : boardFun =
        fun (x,y) ->
            stmntEval stm >>>= lookup "_result_" |> evalSM (
                mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"]) |>
                function
                | Success n -> if Map.containsKey n m then Success(Some m[n]) else Success None
                | Failure e -> Failure e

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard (c:coord) (defaultSq:stm) (boardStmnt:stm) (ids:(int*stm) list) : board =
        {
            center = c
            defaultSquare = stmntToSquareFun defaultSq
            squares = stmntToBoardFun boardStmnt (List.map (fun (int, stm) -> (int, stmntToSquareFun stm)) ids |> Map.ofList)
        }