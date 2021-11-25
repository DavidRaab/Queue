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

// Actual Tests
let q123 = Queue.empty |> Queue.add 1 |> Queue.add 2 |> Queue.add 3
let r123 = Queue.range 1 3
Test.equal q123 r123 "Queue [1;2;3]"

let r123' = Queue.append (que [10]) (Queue.range 1 3) |> Queue.tail
Test.equal r123 r123' ""

Test.equal    (Queue.range 1 10) (Queue.range 1 10) "Two equal Queues"
Test.notEqual (Queue.range 1 5)  (Queue.range 1 10) "None equal Queues"

Test.equal    [1..10] [for x in Queue.range 1 10 -> x] "Test equal for Expression"
Test.notEqual [1..10] [for x in Queue.range 1  5 -> x] "Test notEqual for Expression"

let add1   x = x + 1
let double x = x * 2
let isEven x = x % 2 = 0

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

(*
printfn "updateAt: 3 10 [1..10]      %O" (Queue.updateAt 3 10   (Queue.ofSeq [1..10]))
printfn "updateAt: 100 10 [1..10]    %O" (Queue.updateAt 100 10 (Queue.range 1 10))

printfn "mapi:    (idx,x)            %O" (Queue.mapi (fun i x -> (i,x)) (Queue.ofSeq [1..10]))
printfn "append:  [1..3] [4..6]:     %O" (Queue.append (Queue.ofSeq [1..3]) (Queue.ofSeq [4..6]))
printfn "map2:    [1;2;3] + [10;11;12;13;14;15]: %O" (Queue.map2 (fun x y -> x + y) (Queue.ofSeq [1..3]) (Queue.ofSeq [10..15]))
printfn "map4:    [1;2] + [10;11] + [5;5;4;12] + [9;9;9]: %O"
    (Queue.map4 (fun a b c d -> a + b + c + d) (que [1;2]) (que [10;11]) (que [5;5;4;12]) (que [9;9;9]))

let toListv =
    que [1..5]  |> Queue.bind (fun x ->
    que [10;20] |> Queue.bind (fun y ->
        que [x * y]))
printfn "[1..5] * [10;20]: %A" (Queue.toList toListv)

let flatten = que [Queue.range 1 3; Queue.range 4 6; Queue.range 7 9]
printfn "Concat: 1..9: %O" (Queue.concat flatten)

printfn "init 5:   %O" (Queue.init 5 (fun x -> x * 2))
printfn "indexed:  %O" (Queue.indexed (que [1..5]))
printfn "Is Empty: %b" (Queue.isEmpty (Queue.skip 2 (que [1;2])))
printfn "Item 3 of [1..10]: %A" (Queue.item 3 (que [1..10]))

let isEven x = x % 2 = 0
let square x = x * x

let chooser x =
    if isEven x then ValueSome (square x)
                else ValueNone
printfn "even->square [1..10]: %O" (Queue.choose chooser (que [1..10]))

printfn "Zip: %O" (Queue.zip (que ["A";"B";"C";"D"]) (que [1;2;3]))

printfn "Reduce: %A" (Queue.reduce (+) (que [1..5]))

printfn "forall StartWithA: %b" (que ["ABC"; "AGG"] |>  Queue.forall (fun s -> s.StartsWith "A"))
printfn "forall StartWithA: %b" (que ["ABC"; "BGG"] |>  Queue.forall (fun s -> s.StartsWith "A"))

printfn "any isEven: %b" (Queue.any isEven (que [1;3;4]))
printfn "any isEven: %b" (Queue.any isEven (que [1;3;5]))

printfn "Sum Float: %f" (Queue.sum (que [1.0; 1.5; 3.2]))
printfn "Sum Int:   %d" (Queue.sum (que [1; 2; 3]))

printfn "Sorted: %O" (Queue.sort (que [5;12;5;1;-12]))

printfn "[1-10]: %O" (Queue.range 1 10)
printfn "[1-10]: %O" (Queue.range 1.0 10.0)

printfn "[1..2..10]: %O" (Queue.rangeWithStep 1 2 10)
printfn "[1.0..0.2..2.3]: %O" (Queue.rangeWithStep 1.0 0.2 2.3)

let add x y = x + y
printfn "lift2: add [1;2] [10;20]: %O" (Queue.lift2 add (que [1;2]) (que [10;20]))

let add4 x y z w = x + y + z + w
let xs = que [3;7]
let ys = que [10;20]
let zs = que [100]
let ws = que [1000;2000;3000]
printfn "lift4 add4 %O %O %O %O: %O" xs ys zs ws (Queue.lift4 add4 xs ys zs ws)

printfn "Length Should be 10: %d" (Queue.length (Queue.range 1 10))
printfn "Length Should be  4: %d" (
    Queue.range 1 10
    |> Queue.tail
    |> ValueOption.map  (Queue.filter (fun x -> x % 2 = 0))
    |> ValueOption.map  (Queue.add  1)
    |> ValueOption.map  (Queue.skip 3)
    |> ValueOption.map  (Queue.add  1)
    |> ValueOption.map  (Queue.map (fun x -> x * 2))
    |> ValueOption.bind  Queue.tail
    |> ValueOption.map  (Queue.addMany [1;2;3])
    |> ValueOption.map  (Queue.skip 2)
    |> ValueOption.map  (Queue.length)
    |> ValueOption.defaultValue -1
)

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