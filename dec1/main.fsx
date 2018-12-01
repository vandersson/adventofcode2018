

let readlines filepath: seq<string> = System.IO.File.ReadLines(filepath)

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let toOperation = function
    | Prefix "+" num -> num |> int
    | Prefix "-" num -> num |> int |> (*) -1
    | _ -> 0


let one =
    Seq.sum << Seq.map toOperation << readlines
    

let listOfOperations = Seq.toList << Seq.map toOperation << readlines

let findRecurringFrequency freqChanges =
    let rec findReccurringFrequency' currentFrequency reachedFreqs freqChanges' circBuf =
        match freqChanges' with            
            | ch :: rest ->                
                let newFrequency = currentFrequency + ch
                if List.contains newFrequency reachedFreqs then
                    newFrequency
                else
                    findReccurringFrequency' newFrequency (newFrequency :: reachedFreqs) rest (ch :: circBuf)
            | [] -> findReccurringFrequency' currentFrequency reachedFreqs (List.rev circBuf) []
    findReccurringFrequency' 0 [] freqChanges []

let two = findRecurringFrequency << listOfOperations
