module day8

open System

let input = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

let actualInput = "
..................................................
.......................x.................N........
..................x...............................
............x................w.........D.....a....
.........6...........i.........D..............u...
........6.......................q................u
........................i....u.w...........a.2....
....................................u.12..........
..................................f.....D....0a...
.............................Q..D......c..N..f....
..............w..................................f
.........Y.............i...............q..a.......
............................2..........O...q......
.....6..G.....................R...................
..............................N...................
.U.......G................i...J.............0.....
Y..U................F......N......................
.T......Y.............H.................2.P.......
...............T.........F.8........H.............
..T...............F...............l..............0
................G.....e........18...Q.............
.......................F....................O.....
.....Y....U...................l....g..............
U........9........................................
.....................e..q..Q......................
.......X........e......................1Q..O......
............T.................gx......0..........t
...................l......9........P..............
.y...........9...............r.5.......j.P........
..z.........d.........g......................H....
................6.......r...........P.........O...
.A........................8...r...................
.4....W...Z...........9..................s..j.....
.z..W.........y...........og......................
..3.z.....R.....L....o.........................H..
.......yZ.c..W.......p..............s.............
............1..3.........L.........S..............
.......Z..4............o.....S...........5.......s
............c........l......7.....................
.....4....p.........I.......t...........5........j
.......c....h...........C..d......................
......n..........C......L............E....j.......
.X.W..........n....R......d.I...............5.....
3.........Cn.........L...r.............e..........
...A...........Z.p.....I..S.............s.......J.
....................7.............S...X....J......
........X.............o...........................
........A....h.R.....7.t...I......................
..A.4z......y.p..h.7...........Et.................
................h........3..E..d.8................"

type Node = {
    symbol: char
    xCoord: int
    yCoord: int
}

let parseInput (input: string) =
    let lines = input.Trim().Split([|"\r\n"; "\n"|], System.StringSplitOptions.None)
    let height = lines.Length
    let width = lines[0].Length
    
    let grid = Array2D.init height width (fun y x ->
        let symbol = lines[y][x]
        { symbol = symbol; xCoord = x; yCoord = y }
    )
    grid

let grid = parseInput actualInput

let uniqueChars = 
    grid 
    |> Seq.cast<Node>
    |> Seq.map (fun n -> n.symbol)
    |> Seq.filter (fun c -> c <> '.')
    |> Seq.distinct
    |> Seq.toArray

let nodeGroups: Node list list = 
    uniqueChars
    |> Array.map (fun c -> 
        grid 
        |> Seq.cast<Node>
        |> Seq.filter (fun n -> n.symbol = c)
        |> Seq.toList
    )
    |> Array.toList

let isValidGridPoint = fun (x, y) -> 
    x >= 0 && x < grid.GetLength(1) && y >= 0 && y < grid.GetLength(0)

let nodeFormsPointsAt (nodeA: Node) (nodeB: Node): Node list =
    let vectorX = nodeB.xCoord - nodeA.xCoord
    let vectorY = nodeB.yCoord - nodeA.yCoord
    
    [-1; 2]  
    |> List.map (fun m -> 
        (nodeA.xCoord + (vectorX * m), 
         nodeA.yCoord + (vectorY * m)))
    |> List.filter isValidGridPoint
    |> List.map (fun (x, y) -> grid.[y, x])
    |> List.filter (fun n -> 
        (n.symbol = '.' || (n.symbol <> '.' && not(n = nodeA) && not(n = nodeB))) && 
        not (n = nodeA) && 
        not (n = nodeB))
    
let allNodePairs = 
    nodeGroups
    |> List.collect (fun group -> 
        [for i in 0 .. group.Length-2 do
         for j in i+1 .. group.Length-1 ->
             (group[i], group[j])]
    )

allNodePairs
|> List.iter (fun (nodeA, nodeB) ->
    printfn "Pair: %c(%d,%d) - %c(%d,%d)" 
        nodeA.symbol nodeA.xCoord nodeA.yCoord 
        nodeB.symbol nodeB.xCoord nodeB.yCoord
    let points = nodeFormsPointsAt nodeA nodeB
    points |> List.iter (fun p -> 
        printfn "  Point: (%d,%d)" p.xCoord p.yCoord))



let allPoints = 
    allNodePairs
    |> List.collect (fun (nodeA, nodeB) -> 
        nodeFormsPointsAt nodeA nodeB)

let uniquePoints = 
    allPoints
    |> Set.ofList

let printGridWithPoints (grid: Node[,]) (points: Set<Node>) =
   for y in 0 .. grid.GetLength(0) - 1 do
       for x in 0 .. grid.GetLength(1) - 1 do
           let node = grid.[y,x]
           if points.Contains(node) then
               printf "#"
           else
               printf "%c" node.symbol
       printfn ""

printGridWithPoints grid uniquePoints

let answer = uniquePoints.Count

printfn "%A" answer

// Part 2
let nodeFormsPointsAt2 (nodeA: Node) (nodeB: Node): Node list =
   let vectorX = nodeB.xCoord - nodeA.xCoord
   let vectorY = nodeB.yCoord - nodeA.yCoord
   [-1000..1000]

   |> List.map (fun m -> 
       (nodeA.xCoord + (vectorX * m), 
        nodeA.yCoord + (vectorY * m)))
   |> List.filter isValidGridPoint
   |> List.map (fun (x, y) -> grid.[y, x])

let allPoints2 = 
   allNodePairs
   |> List.collect (fun (nodeA, nodeB) -> nodeFormsPointsAt2 nodeA nodeB)

let uniquePoints2 = 
   allPoints2
   |> Set.ofList

printfn "\nPart 2:"
printfn "Our result:"
printGridWithPoints grid uniquePoints2


let answer2 = uniquePoints2.Count
printfn "%A" answer2