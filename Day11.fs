module day11

open System

let input = "125 17"

let actualInput = "4329 385 0 1444386 600463 19 1 56615"

type Stone = bigint list
type Arrangement = Stone list

let parseInput (input: string) : Arrangement =
    input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun num -> 
        num.ToCharArray() 
        |> Array.map (fun c -> bigint.Parse(string c))
        |> Array.toList)
    |> Array.toList

let arrangement = parseInput actualInput

//is the stone 0 ?
let isZero (stone: Stone) = 
    stone |> List.forall (fun num -> num = 0I)

//does stone have even number of digits?
let isEven (stone: Stone) = 
    stone |> List.length |> fun len -> len % 2 = 0

let rec removeLeadingZeros (stone: Stone) =
    match stone with
    | [] -> [bigint.Zero] 
    | h::rest when h = bigint.Zero -> removeLeadingZeros rest  
    | _ -> stone

let splitInHalfRemoveLeadingZeros (stone: Stone): Stone list =
    let half = List.length stone / 2
    [List.take half stone; 
     stone |> List.skip half |> removeLeadingZeros] |> List.rev

let multiplyStone (stone: Stone) (multiplier: bigint): Stone =
    let numFromDigits = 
        stone 
        |> List.fold (fun acc digit -> acc * 10I + digit) 0I
        
    let result = numFromDigits * multiplier
    
    let rec toDigits n = 
        if n = 0I then []
        else n % 10I :: toDigits (n / 10I)
        
    result
    |> toDigits
    |> List.rev

let rec rearrangeStone (arrangement: Arrangement) (rearranged: Arrangement) =
    match arrangement with
    | [] -> rearranged |> List.rev
    | stone::rest when isZero stone -> (*it becomes 1*) rearrangeStone rest <| [1I]::rearranged
    | stone::rest when isEven stone -> (*split in half according to rules*) rearrangeStone rest <| (splitInHalfRemoveLeadingZeros stone)@rearranged
    | stone::rest -> (*multiply 2024*) rearrangeStone rest <| (multiplyStone stone 2024I):: rearranged

let rearrangeCount = 25

let countStones (arrangement: Arrangement) : int =
    List.length arrangement

let rec iterateTimes count (arrangement: Arrangement) =
    match count with
    | 0 -> arrangement
    | n -> iterateTimes (n-1) (rearrangeStone arrangement [])



let finalArrangement = iterateTimes rearrangeCount arrangement
let stoneCount = List.length finalArrangement
printfn "Number of stones after %d iterations: %d" rearrangeCount stoneCount

//pt2 after realising we've been deceived
let predictStoneGrowth (stone: Stone) (iterations: bigint) : bigint =
    let memoize = new System.Collections.Generic.Dictionary<Stone * bigint, bigint>()
    
    //recursively calculate the number of stones after n iterations
    let rec predict (stone: Stone) (iterations: bigint) : bigint =
        let key = (stone, iterations)
        match memoize.TryGetValue(key) with
        | true, value -> value //returns memoized value
        | false, _ ->
            let result = 
                match iterations with
                | n when n = bigint.Zero -> bigint.One
                | n -> 
                    match stone with
                    | stone when isZero stone -> predict [bigint.One] (n - bigint.One)
                    | stone when isEven stone -> 
                        let [left; right] = splitInHalfRemoveLeadingZeros stone
                        predict left (n - bigint.One) + predict right (n - bigint.One)
                    | stone -> predict (multiplyStone stone 2024I) (n - bigint.One)
            memoize.Add(key, result)
            result
            
    predict stone iterations


let rec predictArrangementGrowth (arrangement: Arrangement) (iterations: bigint) : bigint =
   match arrangement with
   | [] -> bigint.Zero
   | stone::rest ->
       predictStoneGrowth stone iterations + predictArrangementGrowth rest iterations


let totalStones = predictArrangementGrowth arrangement 75I
printfn "Total stones after 75 iterations: %A" totalStones