open Queue
open Expecto

// Test Setup
let tests = ResizeArray()
module Test =
    let equal actual expected msg =
        tests.Add (test msg {
            Expect.equal actual expected ""
        })

    let notEqual actual expected msg =
        tests.Add (test msg {
            Expect.notEqual actual expected ""
        })

    let ok bool msg =
        tests.Add (test msg {
            Expect.isTrue bool ""
        })

    let notOk bool msg =
        tests.Add (test msg {
            Expect.isFalse bool msg
        })

// Utility functions
let que = Queue.ofSeq

let add1   x = x + 1
let double x = x * 2
let isEven x = x % 2 = 0
let square x = x * x

// Actual Tests
let q123 = Queue.empty |> Queue.add 1 |> Queue.add 2 |> Queue.add 3
let r123 = Queue.range 1 3
Test.equal q123 r123 "Queue [1;2;3]"

let r123' = Queue.append (que [10]) (Queue.range 1 3) |> Queue.tail
Test.equal r123 r123' ""

Test.equal    (Queue.range 1 10) (Queue.range 1 10) "Two equal Queues"
Test.notEqual (Queue.range 1 5)  (Queue.range 1 10) "None equal Queues"

let q1t = Queue.range 1 10
Test.equal    [1..10] [for x in q1t -> x] "Test for Expression 1"
Test.equal    [1..10] [for x in q1t -> x] "Test for Expression 2"
Test.notEqual [1..10] [for x in Queue.range 1  5 -> x] "Test notEqual for Expression"

let q1to6 = Queue.range 1 6
Test.ok    (Queue.contains  1 q1to6)   "Queue Contains 1"
Test.notOk (Queue.contains 10 q1to6)   "Queue not Contains 10"
Test.equal (Queue.map double q1to6)    (que [2;4;6;8;10;12]) "Queue.map"
Test.equal (Queue.filter isEven q1to6) (que [2;4;6])         "Queue.filter"

let intern =
    Queue.ofList [1;2;3]
    |> Queue.tail
    |> Queue.add 4
    |> Queue.add 5
    |> Queue.tail
    |> Queue.addMany [6;7]
    |> Queue.tail
    |> Queue.add 8
    |> Queue.add 9
    |> Queue.head
Test.equal intern (ValueSome (4,que [5;6;7;8;9])) "Queue.head"

let q13 = Queue.range 1 3

Test.equal (Queue.toSeq   q1to6 |> Seq.toList) [1;2;3;4;5;6] "Queue.toSeq"
Test.equal (List.ofSeq    q1to6) [1;2;3;4;5;6]     "List.ofSeq"
Test.equal (Queue.toArray q1to6) [|1;2;3;4;5;6|]   "Queue.toArray"
Test.equal (Queue.toList  q1to6) [1;2;3;4;5;6]     "Queue.toList 2"
Test.equal (Queue.ofSeq   (seq [1;2;3])) q13 "Queue.ofSeq"
Test.equal (Queue.ofArray [|1;2;3|])     q13 "Queue.ofArray"
Test.equal (Queue.ofList [1;2;3])        q13 "Queue.ofList"
Test.equal (Queue.take -3 q13) Queue.empty   "Queue.take -3"
Test.equal (Queue.take  2 q13) (que [1;2])   "Queue.take 2"
Test.equal (Queue.take  5 q13) q13           "Queue.take 5"
Test.equal (Queue.skip -3 q13) q13           "Queue.skip -3"
Test.equal (Queue.skip  2 q13) (que [3])     "Queue.skip 2"
Test.equal (Queue.skip  5 q13) (Queue.empty) "Queue.skip 5"
Test.equal (Queue.updateAt 3 10   (Queue.ofSeq [1..10])) (que [1;2;3;10;5;6;7;8;9;10]) "Queue.updateAt 3 10"
Test.equal (Queue.updateAt 100 10 (Queue.range 1 10))    (Queue.range 1 10)            "Queue.updateAt 100 10"
Test.equal (Queue.updateAt -5 10  (Queue.range 1 10))    (Queue.range 1 10)            "Queue.updateAt -5 10"

Test.equal
    (Queue.mapi (fun i x -> (i,x)) (Queue.range 1 10))
    (Queue.indexed (Queue.range 1 10))
    "Queue.mapi"

Test.equal
    (Queue.append (Queue.ofSeq [1..3]) (Queue.ofSeq [4..6]))
    (Queue.range 1 6)
    "Queue.append"

Test.equal
    (Queue.map2 (fun x y -> x + y) (Queue.ofSeq [1..3]) (Queue.ofSeq [10..15]))
    (que [11;13;15])
    "Queue.map2"

Test.equal
    (Queue.map4 (fun a b c d -> a + b + c + d) (que [1;2]) (que [10;11]) (que [5;5;4;12]) (que [9;9;9]))
    (que [25;27])
    "Queue.map4"

let cartesian =
    que [1..5]  |> Queue.bind (fun x ->
    que [10;20] |> Queue.bind (fun y ->
        que [x * y]))

Test.equal
    cartesian
    (que [10;20;20;40;30;60;40;80;50;100])
    "Cartesian through bind"

Test.equal
    cartesian
    (Queue.lift2 (fun x y -> x * y) (Queue.range 1 5) (que [10;20]))
    "cartesian with Queue.lift2"

Test.equal
    (Queue.concat (que [Queue.range 1 3; Queue.range 4 6; Queue.range 7 9]))
    (Queue.range 1 9)
    "Queue.concat"

Test.equal
    (Queue.init 5 double)
    (Queue.range 0 4 |> Queue.map double)
    "Queue.init"

Test.equal
    (Queue.indexed (que ["A";"B";"C"]))
    (que [(0,"A"); (1,"B"); (2,"C")])
    "Queue.indexed"

Test.equal
    (Queue.indexed (que ["A";"B";"C"]))
    (Queue.zip (Queue.range 0 2) (que ["A";"B";"C"]))
    "Queue.indexed 2"

Test.equal
    (Queue.isEmpty (Queue.skip 2 (que [1;2])))
    true
    "Queue.isEmpty"

Test.equal (Queue.item  3 (que [1..10])) (ValueSome 4) "item 3"
Test.equal (Queue.item -2 (que [1..10])) ValueNone     "item -2"
Test.equal (Queue.item 10 (que [1..10])) ValueNone     "item 10"

let evenSquare x =
    if isEven x then ValueSome (square x)
                else ValueNone

let squareEven x =
    let sqr = square x
    if isEven sqr then ValueSome sqr else ValueNone

Test.equal
    (Queue.choose evenSquare (que [1..10]))
    (que [4;16;36;64;100])
    "Queue.choose"

Test.equal
    (Queue.choose evenSquare (que [1..10]))
    (Queue.filterMap isEven square (Queue.range 1 10))
    "Queue.choose vs Queue.filterMap"

Test.equal
    (Queue.choose squareEven (Queue.range 1 10))
    (Queue.mapFilter square isEven (Queue.range 1 10))
    "Queue.choose vs Queue.mapFilter"

Test.equal
    (Queue.range 1 10 |> Queue.filter isEven |> Queue.map square)
    (Queue.filterMap isEven square (Queue.range 1 10))
    "filter->map vs Queue.filterMap"

Test.equal
    (Queue.range 1 10 |> Queue.map square |> Queue.filter isEven)
    (Queue.mapFilter square isEven (Queue.range 1 10))
    "map->filter vs Queue.mapFilter"

Test.equal
    (Queue.zip (que ["A";"B";"C";"D"]) (que [1;2;3]))
    (que ["A",1; "B",2; "C",3])
    "Queue.zip"

Test.equal
    (Queue.reduce (+) (que [1..5]))
    (ValueSome 15)
    "reduce"

Test.equal
    (Queue.reduce (+) (que [10]))
    (ValueSome 10)
    "reduce 2"

Test.equal
    (Queue.reduce (+) Queue.empty)
    ValueNone
    "reduce 3"

Test.equal
    (Queue.reduce (+) (que ["A";"B";"C"]))
    (ValueSome "ABC")
    "reduce 4"

Test.ok
    (que ["ABC"; "AGG"] |>  Queue.forall (fun s -> s.StartsWith "A"))
    "forall"

Test.notOk
    (que ["ABC"; "BGG"] |>  Queue.forall (fun s -> s.StartsWith "A"))
    "forall 2"

Test.ok
    (Queue.any isEven (que [1;3;4]))
    "any"

Test.notOk
    (Queue.any isEven (que [1;3;5]))
    "any 2"

Test.ok
    ((abs ((Queue.sum (que [1.1; 1.5; 3.2])) - 5.8)) < 0.0000001)
    "Sum of Float"

Test.equal
    (Queue.sum (que [1; 2; 3]))
    6
    "Sum of Int"

Test.equal
    (Queue.sort (que [5;12;5;1;-12]))
    (Queue.empty |> Queue.addMany [-12;1;5;5;12])
    "Queue.sort"

Test.equal
    (Queue.range 1 10)
    (Queue.empty |> Queue.addMany [1..10])
    "addMany"

Test.equal
    (Queue.range 1.0 4.0)
    (Queue.empty |> Queue.addMany [1.0;2.0;3.0;4.0])
    "addMany 2"

Test.equal
    (Queue.range 1 10 |> Queue.length)
    10
    "range length"

Test.equal
    (Queue.rangeWithStep 1 2 10)
    (que [1;3;5;7;9])
    "rangeWithStep"

let floatis x y =
    (abs (x - y)) < 0.000001

Test.ok
    (Queue.forall (fun (x,y) -> floatis x y)
        (Queue.zip
            (Queue.rangeWithStep 1.0 0.2 2.3)
            (que [1.0;1.2;1.4;1.6;1.8;2.0;2.2])))
    "rangeWithStep float"

let add4 x y z w = x + y + z + w
Test.equal
    (Queue.lift4 add4 (que [3;7]) (que [10;20]) (Queue.one 100) (que [1000;2000;3000]))
    (que [1113;2113;3113;1123;2123;3123;1117;2117;3117;1127;2127;3127])
    "lift4"

Test.equal
    (Queue.length (Queue.range 1 10))
    10
    "Queue.length"

let complex =
    Queue.range 1 10
    |> Queue.tail
    |> Queue.filter isEven
    |> Queue.add  5
    |> Queue.skip 2
    |> Queue.add  7
    |> Queue.map square
    |> Queue.tail
    |> Queue.addMany [1;2;3]

Test.equal (Queue.length complex) 7           "Complex length"
Test.equal complex (que [64;100;25;49;1;2;3]) "Complex equal"
Test.equal
    (Queue.skipWhile isEven complex |> Queue.sort)
    (que [1;2;3;25;49])
    "skipWhile isEven sort"



(*
printfn "Length Should be  8: %d" (Queue.length (Queue.insertManyAt  2 [1;2;3] (Queue.range 1 5)))
printfn "Length Should be  5: %d" (Queue.length (Queue.insertManyAt 10 [1;2;3] (Queue.range 1 5)))
printfn "Length Should be 13: %d" (Queue.length (Queue.insertManyAtWithExpanding 0 10 [1;2;3] (Queue.range 1 5)))

printfn "insertAt -1 100 [1..5]: %O" (Queue.insertAt -1 100 (Queue.range 1 5))
printfn "insertAt 0 100 empty:   %O" (Queue.insertAt 0 100 Queue.empty)
printfn "insertAt 1 100 empty:   %O" (Queue.insertAt 1 100 Queue.empty)
printfn "insertAt 1 100 [1..10]: %O" (Queue.insertAt 1 100 (Queue.range 1 10))

printfn "insertAtWithExpanding 0 -1 10 empty: %O" (Queue.insertAtWithExpanding 0 -1 10 Queue.empty)
printfn "insertAtWithExpanding 0  4 10 empty: %O" (Queue.insertAtWithExpanding 0  4 10 Queue.empty)
printfn "allPairs: %O" (Queue.allPairs (que ["A";"B"]) (que [1;2]))
printfn "chunkBySize 3 [1..10]: %O" (Queue.chunkBySize 3 (que [1..10]))
printfn "zip3 [1;2;3] [10;20] [4;4;4;4]: %O" (Queue.zip3 (que [1;2;3]) (que [10;20]) (que [4;4;4;4]))
printfn "countBy: %O" (Queue.countBy id (que ["Hallo";"Hallo";"Welt";"Hallo"]))
printfn "exactlyOne [5;1]: %A" (Queue.exactlyOne (que [5;1]))
printfn "exactlyOne [5]:   %A" (Queue.exactlyOne (que [5]))

printfn "findIndex isEven [3;3;5;2]: %A" (Queue.findIndex isEven (que [3;3;5;2]))
printfn "findIndex isEven [3;3;5]:   %A" (Queue.findIndex isEven (que [3;3;5]))

printfn "addMany [4;5;6] [1;2;3]: %O" (Queue.addMany [4;5;6] (Queue.range 1 3))
printfn "rev [1..5]: %O" (Queue.rev (Queue.range 1 5))

printfn "findIndex     isEven [1;2;3;4;5]: %A" (Queue.findIndex     isEven (Queue.range 1 5))
printfn "findIndexBack isEven [1;2;3;4;5]: %A" (Queue.findIndexBack isEven (Queue.range 1 5))
printfn "find          isEven [1;2;3;4;5]: %A" (Queue.find     isEven (Queue.range 1 5))
printfn "findBack      isEven [1;2;3;4;5]: %A" (Queue.findBack isEven (Queue.range 1 5))

printfn "updateAtWithExpanding 0 2  100 [1..10]: %O" (Queue.updateAtWithExpanding 0 2  100 (Queue.range 1 10))
printfn "updateAtWithExpanding 0 15 100 [1..10]: %O" (Queue.updateAtWithExpanding 0 15 100 (Queue.range 1 10))

printfn "insertManyAt  0 [1;2;3] [10..20]: %O" (Queue.insertManyAt  0 [1;2;3] (Queue.range 10 20))
printfn "insertManyAt  4 [1;2;3] [10..20]: %O" (Queue.insertManyAt  4 [1;2;3] (Queue.range 10 20))
printfn "insertManyAt 20 [1;2;3] [10..20]: %O" (Queue.insertManyAt 20 [1;2;3] (Queue.range 10 20))

printfn "insertManyAtWithExpanding 0  0 [1;2;3] [10..20]: %O" (Queue.insertManyAtWithExpanding 0  0 [1;2;3] (Queue.range 10 20))
printfn "insertManyAtWithExpanding 0  4 [1;2;3] [10..20]: %O" (Queue.insertManyAtWithExpanding 0  4 [1;2;3] (Queue.range 10 20))
printfn "insertManyAtWithExpanding 0 20 [1;2;3] [10..20]: %O" (Queue.insertManyAtWithExpanding 0 20 [1;2;3] (Queue.range 10 20))

printfn "slice 0  3 [1..10]: %O" (Queue.slice 0  3 (Queue.range 1 10))
printfn "slice 3  7 [1..10]: %O" (Queue.slice 3  7 (Queue.range 1 10))
printfn "slice 5 20 [1..10]: %O" (Queue.slice 5 20 (Queue.range 1 10))
printfn "slice 0  0 [1..10]: %O" (Queue.slice 0  0 (Queue.range 1 10))

printfn "takeWhile isEven [2;4;6;7;3;2]: %O" (Queue.takeWhile isEven (que [2;4;6;7;3;2]))
printfn "skipWhile isEven [2;4;6;7;3;2]: %O" (Queue.skipWhile isEven (que [2;4;6;7;3;2]))

printfn "equal true:  %b" (Queue.equal (Queue ([1;2;3],[],3)) (Queue ([],[3;2;1],3)))
printfn "equal true:  %b" (Queue.equal (Queue ([1],[3;2],3))  (Queue ([],[3;2;1],3)))
printfn "equal false: %b" (Queue.equal (Queue.range 1 5) (Queue.range 1 6))
printfn "equal false: %b" (Queue.equal (que [1;2;3]) (que [1;5;3]))

printfn "scan: %O" (Queue.scan (fun l x -> x :: l) [] (Queue.range 1 5))
printfn "Last [1..10]: %A" (Queue.last (Queue.range 1 10))

let isGreater x y =
    if   x = y then 0
    elif x > y then 1
    else -1

printfn "compare [1..3] [1..3]:   %d" (Queue.compareWith isGreater (Queue.range 1 3) (Queue.range 1 3))
printfn "compare [1..4] [1..3]:   %d" (Queue.compareWith isGreater (Queue.range 1 4) (Queue.range 1 3))
printfn "compare [1..3] [1..4]:   %d" (Queue.compareWith isGreater (Queue.range 1 3) (Queue.range 1 4))
printfn "compare [1] [2]:         %d" (Queue.compareWith isGreater (Queue.one 1) (Queue.one 2))
printfn "compare [2] [1]:         %d" (Queue.compareWith isGreater (Queue.one 2) (Queue.one 1))
printfn "compare [2;1] [1;2]:     %d" (Queue.compareWith isGreater (que [2;1]) (que [1;2]))
printfn "compare [1;2;3] [1;2;4]: %d" (Queue.compareWith isGreater (que [1;2;3]) (que [1;2;4]))

let add1 x = x + 1
printfn "MapFilter add1 isEven: [1..10]: %O" (Queue.mapFilter add1 isEven (Queue.range 1 10))
printfn "FilterMap isEven add1: [1..10]: %O" (Queue.filterMap isEven add1 (Queue.range 1 10))
printfn "sliceGrow 0  7 12 [1..10]: %O" (Queue.sliceGrow 0  7 12 (Queue.range 1 10))
printfn "sliceGrow 0  9 10 [1..10]: %O" (Queue.sliceGrow 0  9 10 (Queue.range 1 10))
printfn "sliceGrow 0 10 10 [1..10]: %O" (Queue.sliceGrow 0 10 10 (Queue.range 1 10))
printfn "sliceGrow 0 -5  5 [1..10]: %O" (Queue.sliceGrow 0 -5  5 (Queue.range 1 10))
printfn "sliceGrow 0  5  5 [1..10]: %O" (Queue.sliceGrow 0  5  5 (Queue.range 1 10))
printfn "sliceGrow 0 11 13 [1..10]: %O" (Queue.sliceGrow 0 11 13 (Queue.range 1 10))
printfn "sliceGrow 0 -5 14 [1..10]: %O" (Queue.sliceGrow 0 -5 14 (Queue.range 1 10))
printfn "sliceGrow 0 20 10 [1..10]: %O" (Queue.sliceGrow 0 20 10 (Queue.range 1 10))
printfn "sliceGrow 0  5 0  [1..10]: %O" (Queue.sliceGrow 0  5  0 (Queue.range 1 10))
printfn "repeat  0  0: %O" (Queue.repeat  0 0)
printfn "repeat  1  0: %O" (Queue.repeat  1 0)
printfn "repeat -5  0: %O" (Queue.repeat -5 0)
printfn "repeat 10 10: %O" (Queue.repeat 10 0)
printfn "evens|unevens: %O" (Queue.partition isEven (Queue.range 1 10))
printfn "min [1;10;3]: %O" (Queue.min (que [1;10;3]))
printfn "max [1;10;3]: %O" (Queue.max (que [1;10;3]))
printfn "minBy String.length [\"Hallo\";\"Welt\"]: %O" (Queue.minBy String.length (que ["Hallo";"Welt"]))
printfn "maxBy String.length [\"Hallo\";\"Welt\"]: %O" (Queue.maxBy String.length (que ["Hallo";"Welt"]))

*)


// Run Tests
let args = Array.skip 1 <| System.Environment.GetCommandLineArgs()
runTestsWithCLIArgs [] args (testList "Main" (List.ofSeq tests)) |> ignore