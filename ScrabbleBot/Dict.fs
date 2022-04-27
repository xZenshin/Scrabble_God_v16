module Dictionary

    type Dict = 
        | Leaf of bool
        | Node of Map<char, bool * Dict>
    
    let empty () = Leaf false
    
    let insert (s : string) (node : Dict) = 
        let rec inner (s:string) (nod: Dict) =
            match nod with 
            |Leaf x when s.Length = 1                                    -> Node(Map.add (s.[0]) (true, Leaf false) Map.empty)
            |Leaf x when x = false                                       -> Node(Map.add (s.[0]) (false, inner s.[1..] (Leaf false)) Map.empty)
            |Node x when s.Length = 1 && x.ContainsKey s.[0] = false     -> Node(Map.add (s.[0]) (true, Leaf false) x)
            |Node x when s.Length = 1                                    -> Node(Map.add (s.[0]) (true, snd(Map.find s.[0] x)) x)
            |Node x when x.ContainsKey s.[0]                             -> Node(Map.add (s.[0]) (fst(Map.find s.[0] x), inner (s.[1..]) (snd(Map.find s.[0] x))) x)
            |Node x when x.ContainsKey s.[0] = false                     -> Node(Map.add (s.[0]) (false, inner s.[1..] (Leaf false)) x)
        inner s node


    let lookup (s: string) (node: Dict) = 
        let rec inner (s : string) (nod : Dict) = 
            match nod with
            | Leaf _ -> false
            | Node x when s.Length = 1 -> Map.tryFind s.[0] x |>
                function
                | Some x -> true
                | None   -> false 
            | Node x when x.ContainsKey s.[0] -> inner (s.[1..]) (snd(Map.find s.[0] x))
            | Node x -> false
        inner s node

    let step (c : char) (node : Dict) = 
        let rec inner (c : char) (nod : Dict) = 
            match nod with
            | Node x when x.ContainsKey c  = false -> None  
            | Node x -> Map.tryFind c x |>
                function
                | Some (b, dict) -> 
                    match dict with
                    | Node d when b -> Some(b, dict)
                    | _ -> Some(b, dict)
                | None -> None
            | Leaf _ -> None
        inner c node


