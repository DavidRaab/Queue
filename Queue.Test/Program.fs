open Queue
open Expecto

let que = Queue.ofSeq

let qs =
    Queue.empty
    |> Queue.add 1
    |> Queue.add 2
    |> Queue.add 3

printfn "add 1,2,3 -> %A" qs

let qs2 =
    Queue.tail qs
    |> ValueOption.defaultValue Queue.empty
    |> Queue.add 4
    |> Queue.add 5
    |> Queue.add 6

printfn "[2;3];[6;5;4] -> %A" qs2

printfn "QS2: %A" qs2

printfn "qs2 contains 1: %b" (Queue.contains 1 qs2)
printfn "qs2 contains 6: %b" (Queue.contains 6 qs2)

printfn "qs2 toList: %A" (Queue.toList qs2)
printfn "double qs2: %A" (Queue.map (fun x -> x*2) qs2)
printfn "evens qs2:  %A" (Queue.filter (fun x -> x%2 = 0) qs2)

let t1 =
    Queue.ofList [1;2;3]
    |> Queue.tail |> ValueOption.defaultValue Queue.empty
    |> Queue.add 4
    |> Queue.add 5
    |> Queue.tail |> ValueOption.defaultValue Queue.empty
    |> Queue.add 6
    |> Queue.add 7
    |> Queue.tail |> ValueOption.defaultValue Queue.empty
    |> Queue.add 8
    |> Queue.add 9
    |> Queue.head

printfn "Should be: (4,[5;6;7;8;9]) = %O" t1

printfn "Seq   QS2: %A" (Queue.toSeq   qs2)
printfn "Array QS2: %A" (Queue.toArray qs2)
printfn "List  QS2: %A" (Queue.toList  qs2)

printfn "Seq    1,2,3 = %A" (Queue.ofSeq (seq [1;2;3]))
printfn "Array  1,2,3 = %A" (Queue.ofArray [|1;2;3|])
printfn "List   1,2,3 = %A" (Queue.ofList  [1;2;3])

printfn "Take -3 from 1,2,3: %O" (Queue.take -3 (Queue.range 1 3))
printfn "Take  2 from 1,2,3: %O" (Queue.take  2 (Queue.range 1 3))
printfn "Take  5 from 1,2,3: %O" (Queue.take  5 (Queue.range 1 3))

printfn "Skip -3 from 1,2,3: %O" (Queue.skip -3 (Queue.range 1 3))
printfn "Skip  2 from 1,2,3: %O" (Queue.skip  2 (Queue.range 1 3))
printfn "Skip  5 from 1,2,3: %O" (Queue.skip  5 (Queue.range 1 3))

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
