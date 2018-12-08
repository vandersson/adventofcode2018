
type Dups =
    {
        twos : int
        threes : int
    }

let rec countOccurance c inputS =
    match inputS with
        | hd :: td when hd = c -> 1 + countOccurance c td
        | _ :: td -> countOccurance c td
        | [] -> 0

let countAllOccurance input =
    let rec countAllOccurace' tInput =
        match tInput with
            | hd :: td -> countOccurance hd input :: countAllOccurace' td
            | [] -> []
    countAllOccurace' input

let boxDups l =
    {
        twos = if List.contains 2 l then 1 else 0
        threes = if List.contains 3 l then 1 else 0
    }

let addDups d1 d2 =
    {
        twos = d1.twos + d2.twos
        threes = d1.threes + d2.threes
    }

let sumBoxDups =
    Seq.reduce addDups

let checksum { twos = tw; threes = th } =
    tw * th;

let readlines filepath: seq<string> = System.IO.File.ReadLines(filepath)

let one =
    checksum << sumBoxDups << Seq.map boxDups << Seq.map countAllOccurance << Seq.map Seq.toList << readlines


let rec matchingChars a b =
    match (a, b) with
        | (ha :: ta, hb :: tb) when ha = hb -> Some ha :: matchingChars ta tb
        | (_ :: ta, _ :: tb) -> None :: matchingChars ta tb
        | (_, _) -> []

let matchingForAll (l: List<List<char>>) =
    let rec matchingForAll' =
        function
            | hd :: td -> l |> List.except (Seq.singleton hd) |> List.map (matchingChars hd) |> (fun x -> x :: matchingForAll' td)
            | [] -> []
    matchingForAll' l
    
let onlyOneNone l =
    let rec onlyOneNone' err = function
        | Some _ :: td -> onlyOneNone' err td
        | None :: td -> if err then false else onlyOneNone' true td
        | [] -> true
    onlyOneNone' false l

let implode (xs: List<Option<char>>) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (function
                         | Some c -> c |> sb.Append |> ignore
                         | None -> ())
        sb.ToString()
       
let two input =
    readlines input
    |> Seq.map Seq.toList
    |> Seq.toList
    |> matchingForAll
    |> List.map (List.filter onlyOneNone)
    |> List.filter (not << List.isEmpty)
    |> List.head
    |> List.head
    |> implode
    

