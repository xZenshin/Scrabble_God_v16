// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H', 4);('E',1);('L',1);('L',1);('O',1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b = a >>= fun x ->
                  b >>= fun y ->
                  ret(x+y)   
    let subtract a b = a >>= fun x ->
                  b >>= fun y ->
                  ret(x-y)

    let multiply a b = a >>= fun x ->
                  b >>= fun y ->
                  ret(x*y)
    let modulo a b = a >>= fun x ->
                  b >>= fun y ->
                  if y <> 0 then ret(x%y) else fail DivisionByZero  

    let div a b = a >>= fun x ->
                  b >>= fun y ->
                  if y <> 0 then ret(x/y) else fail DivisionByZero  
    let greater a b = a >>= fun x ->
                  b >>= fun y ->
                  if x > y then ret true else ret false
    let conjunction a b = a >>= fun x ->
                  (b >>= fun y ->
                  ret(x && y))
    let equal a b = a >>= fun x ->
                  (b >>= fun y ->
                  ret(x = y))

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

    let binop f a b s =
        a s >>= fun x ->
        b s >>= fun y ->
        ret (f x y)

    let find x = fun s ->
        match Map.tryFind x s with
        | Some v -> Success (v, s)
        | None -> Failure (VarNotFound x)

    let rec arithEval a : SM<int> = 
            match a with
            | N n -> ret n
            | V x -> lookup x
            | WL -> wordLength
            | PV x -> arithEval x >>= pointValue
            | Add (a1, a2) -> add (arithEval a1) (arithEval a2)
            | Sub (a1, a2) -> subtract (arithEval a1) (arithEval a2)
            | Mul (a1, a2) -> multiply (arithEval a1) (arithEval a2)
            | Div (a1, a2) -> div (arithEval a1) (arithEval a2) 
            | Mod (a1, a2) -> modulo (arithEval a1) (arithEval a2)
            | CharToInt c ->  charEval c >>= fun x -> ret(int x)
    and charEval c : SM<char> = 
            match c with
            | C c -> ret c
            | CV a -> arithEval a >>= characterValue
            | ToUpper c -> charEval c >>= (System.Char.ToUpper >> ret)
            | ToLower c -> charEval c >>= (System.Char.ToLower >> ret)
            | IntToChar a -> arithEval a >>= fun x -> ret(char x)
          

    let rec boolEval b : SM<bool> = 
            match b with
            | TT -> ret true                
            | FF  -> ret false          
            | AEq (a1, a2) -> equal (arithEval a1) (arithEval a2)
            | ALt (a1, a2) ->  greater (arithEval a1) (arithEval a2)
            | Not b -> boolEval b >>= fun x -> ret(not(x))
            | Conj (b1, b2) -> conjunction (boolEval b1) (boolEval b2)
            | IsVowel c -> charEval c >>=
                function
                | 'A' -> ret true
                | 'E' -> ret true
                | 'I' -> ret true
                | 'O' -> ret true
                | 'U' -> ret true
                | 'Y' -> ret true
                | _ -> ret false
            | IsLetter c ->  charEval c >>= (System.Char.IsLetter >> ret)
            | IsDigit c ->  charEval c >>= (System.Char.IsDigit >> ret)


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
        
    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"