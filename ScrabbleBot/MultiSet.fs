// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison>= Map<'a, uint32>

    let empty : MultiSet<'a> = Map.empty
    
    let isEmpty (m: MultiSet<'a>) = Map.isEmpty(m)

    let size (m: MultiSet<'a>) = m |> Map.fold(fun state key value -> state + value) 0u

    let contains a (s: MultiSet<'a>) = s |> Map.exists (fun n s -> n = a )

    let numItems a (s: MultiSet<'a>) = s |> Map.filter(fun n s -> n = a) |> size

    let add a (n: uint32) (s: MultiSet<'a>) = Map.add a n s
    
    let addSingle a (s: MultiSet<'a>) = Map.add a 1u s
                            
    let remove a (n: uint32) (s: MultiSet<'a>) = if (contains a s && n >= Map.find a s) then Map.remove a s else add a ((Map.find a s) - n) s
                        
    let removeSingle a (s: MultiSet<'a>) = if (contains a s) then add a ((Map.find a s) - 1u) s else s

    let fold f acc (s: MultiSet<'a>) = s |> Map.fold(f) acc

    let foldBack f (s: MultiSet<'a>) acc = acc |> Map.foldBack(f) s        

    let ofList  ls = ls |> List.countBy id |> List.fold(fun x y -> add (fst y) (System.UInt32.Parse((snd y).ToString())) x ) empty

    let rec doStuff times key list = 
            match times with
            | 0u -> list
            | _ -> doStuff (times-1u) key (key::list)


    let toList (s: MultiSet<'a>) = (List.rev(s |> Map.fold(fun state key value -> doStuff value key state) []))

    let map f (s: MultiSet<'a>) = s |> Seq.map(fun x -> f x.Key, x.Value) |> Map.ofSeq
