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

    let whitespaceChar = satisfy(fun x -> System.Char.IsWhiteSpace(x)) <?> "whitespace"
    let pletter        = satisfy (fun x -> System.Char.IsLetter(x)) <?> "letter"
    let palphanumeric  = satisfy (fun x -> System.Char.IsLetterOrDigit(x)) <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        =  many1 whitespaceChar <?> "space1"

    let (.>*>.) (p1: Parser<'a>) (p2: Parser<'b>) =  p1 .>> spaces .>>. p2
             

    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>.  p .>*> pchar ')'
    let pid = pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>> fun (a,b) ->  a.ToString() + System.String.Concat(Array.ofList(b))

    let unop op = fun a -> op .>*>. a |>> snd   
    let binop op p1 p2 = p1 .>*>. op .>*>. p2 |>> fun ((a,b), c) -> (a,c)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let SubParse =  binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice  [SubParse; AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; ModParse; DivParse; AtomParse]
        
    let SomeParse, cref = createParserForwardedToRef<cExp>()
 
    
    let cParParse = parenthesise SomeParse
    let NParse   = pint32 |>> N <?> "Int"
    let VParse = pid |>> V <?> "V"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> fun(N x) -> Mul(N -1, N x)
    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PV"
    let cCharToInt = pCharToInt >*>. cParParse |>> CharToInt
    do aref := choice [cCharToInt; NegParse; PVParse; NParse; VParse; ParParse]
    let AexpParse = TermParse 

    let cIntToChar = pIntToChar >*>. ParParse |>> IntToChar 
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"
    let cToUpper = pToUpper >*>. cParParse |>> ToUpper
    let cToLower = pToLower >*>. cParParse |>> ToLower
    let CVParse = pCharValue >*>. parenthesise AexpParse |>> CV <?> "CV"
    
    do cref := choice [cIntToChar; CVParse; cToUpper; cToLower; CParse]
    let CexpParse = SomeParse


    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
