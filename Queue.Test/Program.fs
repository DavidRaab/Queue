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
Test.equal q123 r123 "add same as range"

let r123' = Queue.append (que [10]) (Queue.range 1 3) |> Queue.tail
Test.equal r123 r123' "append 1"

Test.equal
    (Queue.append (Queue.one 1) (Queue.range 2 5))
    (Queue.one 1 ++ (Queue.range 2 5))
    "append ++"

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
    (Queue.addMany [4;5;6] (Queue.range 1 3))
    (Queue.range 1 6)
    "addMany 3"

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

Test.equal
    (Queue.insertAt -1 100 (Queue.range 1 5))
    (que [1;2;3;4;5])
    "insertAt 1"

Test.equal
    (Queue.insertAt 0 100 Queue.empty)
    (Queue.one 100)
    "insertAt 2"

Test.equal
    (Queue.insertAt 1 100 Queue.empty)
    (Queue.empty)
    "insertAt 3"

Test.equal
    (Queue.insertAt 1 100 (Queue.range 1 10))
    (Queue.concat (que [Queue.one 1; Queue.one 100; Queue.range 2 10]))
    "insertAt 4"

Test.equal
    (Queue.insertAt 3 100 (que [6;4;2]))
    (que [6;4;2;100])
    "insertAt 5"

Test.equal
    (Queue.insertAt 4 100 (que [6;4;2]))
    (que [6;4;2])
    "insertAt 6"

Test.equal
    (Queue.insertAtGrow 0 -1 10 Queue.empty)
    (Queue.empty)
    "insertAtGrow 1"

Test.equal
    (Queue.insertAtGrow 0  4 10 Queue.empty)
    (Queue.add 10 (Queue.repeat 4 0))
    "insertAtGrow 2"

Test.equal
    (Queue.insertAtGrow 0  2 10 (Queue.range 1 5))
    (Queue.insertAt        2 10 (Queue.range 1 5))
    "insertAtGrow 3"

Test.equal
    (Queue.insertAtGrow 0  0 10 Queue.empty)
    (Queue.one 10)
    "insertAtGrow 4"

Test.equal
    (Queue.length (Queue.insertAtGrow 0  9 10 Queue.empty))
    10
    "insertAtGrow 5"

Test.equal
    (Queue.lastIndex (Queue.insertAtGrow 0  9 10 Queue.empty))
    9
    "insertAtGrow 6"

Test.equal
    (Queue.allPairs (que ["A";"B"]) (que [1;2]))
    (que ["A",1; "A",2; "B",1; "B",2])
    "allPairs"

Test.equal
    (Queue.chunkBySize 3 (que [1..10]))
    (que [que [1;2;3]; que [4;5;6]; que [7;8;9]; Queue.one 10])
    "chunkBySize"

Test.equal
    (Queue.chunkBySize 3 Queue.empty)
    (Queue.empty)
    "chunkBySize 2"

Test.equal
    (Queue.chunkBySize 3 (Queue.one 1))
    (Queue.one (Queue.one 1))
    "chunkBySize 3"

Test.equal
    (Queue.zip3 (que [1;2;3]) (que [10;20]) (que [4;4;4;4]))
    (Queue.one (1,10,4) |> Queue.add (2,20,4))
    "Zip3"

let nameOccurrences = Queue.countBy id (que ["Hallo";"Hallo";"Welt";"Hallo"])
Test.equal
    (Queue.find (fun (str,n) -> str = "Hallo") nameOccurrences)
    (ValueSome ("Hallo", 3))
    "CountBy 1"

Test.equal
    (Queue.find (fun (str,n) -> str = "Welt") nameOccurrences)
    (ValueSome ("Welt", 1))
    "CountBy 2"

Test.equal (Queue.length nameOccurrences) 2 "countBy 3"

Test.equal
    (Queue.rev (Queue.range 1 5))
    (que [5;4;3;2;1])
    "rev 1"

Test.equal
    (Queue.rev (que [5;4;3;2;1]))
    (Queue.range 1 5)
    "rev 2"

Test.equal
    (Queue.range 1 5)
    (Queue.rev (Queue.rev (Queue.range 1 5)))
    "rev 3"

Test.equal
    (Queue.rev Queue.empty)
    Queue.empty
    "rev 4"

Test.equal
    (Queue.exactlyOne (Queue.tail (Queue.rev (Queue.one 100 |> Queue.add 10))))
    (ValueSome 100)
    "rev 5"

Test.equal (Queue.exactlyOne    (que [5;1]))               (ValueNone   ) "exactlyOne 1"
Test.equal (Queue.exactlyOne    (que [5;1] |> Queue.tail)) (ValueSome  1) "exactlyOne 2"
Test.equal (Queue.findIndex     isEven (que [3;3;5;2]))    (ValueSome  3) "findIndex 1"
Test.equal (Queue.findIndex     isEven (que [3;3;5]))      (ValueNone   ) "findIndex 2"
Test.equal (Queue.findIndex     isEven (Queue.range 1 5))  (ValueSome  1) "findIndex 3"
Test.equal (Queue.findIndex     isEven (que [1;3;5]))      (ValueNone   ) "findIndex 4"
Test.equal (Queue.findIndexBack isEven (Queue.range 1 5))  (ValueSome  3) "findIndexBack 1"
Test.equal (Queue.findIndexBack isEven (que [5;3;1]))      (ValueNone   ) "findIndexBack 2"
Test.equal (Queue.find          isEven (Queue.range 9 20)) (ValueSome 10) "find 1"
Test.equal (Queue.find          isEven (que [9;13;15]))    (ValueNone   ) "find 2"
Test.equal (Queue.findBack      isEven (Queue.range 9 19)) (ValueSome 18) "findBack 1"
Test.equal (Queue.findBack      isEven (que [15;17;19]))   (ValueNone   ) "findBack 2"

Test.equal
    (Queue.updateAtGrow 0 2 100 (Queue.range 1 10))
    (que [1;2;100] |> Queue.addMany (Queue.range 4 10))
    "updateAtGrow 1"

Test.equal
    (Queue.updateAtGrow 0 15 100 (Queue.range 1 10))
    (Queue.range 1 10 |> Queue.addMany (Queue.repeat 5 0) |> Queue.add 100)
    "updateAtGrow 2"

Test.equal
    (Queue.updateAtGrow 0 -1 100 (Queue.range 1 10))
    (Queue.range 1 10)
    "updateAtGrow 3"

Test.equal
    (Queue.updateAtGrow 0 0 100 (Queue.empty))
    (Queue.one 100)
    "updateAtGrow 4"

Test.equal
    (Queue.insertManyAt  2 [1;2;3] (Queue.range 1 5))
    (que [1;2;1;2;3;3;4;5])
    "insertManyAt 1"

Test.equal
    (Queue.insertManyAt 10 [1;2;3] (Queue.range 1 5))
    (Queue.range 1 5)
    "insertManyAt 2"

Test.equal
    (Queue.insertManyAt  0 [1;2;3] (Queue.range 10 15))
    (que [1;2;3;10;11;12;13;14;15])
    "insertManyAt 3"

Test.equal
    (Queue.insertManyAt -1 [1;2;3] (Queue.range 10 15))
    (Queue.tail (Queue.one 0 |> Queue.addMany (Queue.range 10 15)))
    "insertManyAt 4"

Test.equal
    (Queue.insertManyAt  4 [1;2;3] (Queue.range 10 20))
    (que [10;11;12;13;1;2;3;14;15;16;17;18;19;20])
    "insertManyAt 5"

Test.equal
    (Queue.insertManyAt 20 [1;2;3] (Queue.range 10 20))
    (Queue.append (Queue.range 10 15) (Queue.range 16 20))
    "insertManyAt 6"

Test.equal
    (Queue.insertManyAt 5 [1;2;3] (Queue.range 10 14))
    (Queue.append (Queue.range 10 14) (Queue.range 1 3))
    "insertManyAt 7"

Test.equal
    (Queue.insertManyAt 6 [1;2;3] (Queue.range 10 14))
    (Queue.range 10 14)
    "insertManyAt 8"

Test.equal
    (Queue.insertManyAtGrow 0 10 [1;2;3] (Queue.range 1 5))
    (que [1;2;3;4;5;0;0;0;0;0;1;2;3])
    "insertManyAtGrow 1"

Test.equal
    (Queue.insertManyAtGrow 0  0 [1;2;3] (Queue.range 10 20))
    ((Queue.range 1 3) ++ (Queue.range 10 20))
    "insertManyAtGrow 2"

Test.equal
    (Queue.insertManyAtGrow 0  4 [1;2;3] (Queue.range 10 20))
    ((Queue.range 10 13) ++ (Queue.range 1 3) ++ (Queue.range 14 20))
    "insertManyAtGrow 3"

Test.equal
    (Queue.insertManyAtGrow 0 20 [1;2;3] (Queue.range 10 20))
    (Queue.concat (que [(Queue.range 10 20); (Queue.repeat 9 0); (Queue.range 1 3);]))
    "insertManyAtGrow 4"

Test.equal
    (Queue.item 20 (Queue.insertManyAtGrow 0 20 [1;2;3] (Queue.range 10 20)))
    (ValueSome 1)
    "insertManyAtGrow 5"

Test.equal
    (Queue.slice 20 22 (Queue.insertManyAtGrow 0 20 [1;2;3] (Queue.range 10 20)))
    (Queue.range 1 3)
    "insertManyAtGrow 6"

Test.equal
    (Queue.slice 19 23 (Queue.insertManyAtGrow 0 20 [1;2;3] (Queue.range 10 20)))
    (que [0;1;2;3])
    "insertManyAtGrow 7"

Test.equal
    (Queue.sliceGrow 0 19 23 (Queue.insertManyAtGrow 0 20 [1;2;3] (Queue.range 10 20)))
    (que [0;1;2;3;0])
    "insertManyAtGrow 8"

Test.equal (Queue.slice   0  3 (Queue.range 1 10)) (Queue.range 1 4)  "slice 1"
Test.equal (Queue.slice   3  7 (Queue.range 1 10)) (Queue.range 4 8)  "slice 2"
Test.equal (Queue.slice   5 20 (Queue.range 1 10)) (Queue.range 6 10) "slice 3"
Test.equal (Queue.slice   0  0 (Queue.range 1 10)) (Queue.one 1)      "slice 4"
Test.equal (Queue.slice   2  2 (Queue.range 1 10)) (Queue.one 3)      "slice 5"
Test.equal (Queue.slice -10  3 (Queue.range 1 10)) (Queue.range 1 4)  "slice 6"
Test.equal (Queue.slice  10 12 (Queue.range 1  5)) (Queue.empty)      "slice 7"
Test.equal (Queue.slice -5 -10 (Queue.range 1  5)) (Queue.empty)      "slice 8"
Test.equal (Queue.slice  10  5 (Queue.range 1  5)) (Queue.empty)      "slice 9"

Test.equal (Queue.sliceGrow -1   0   3 (Queue.range 1 10)) (Queue.range 1 4)   "sliceGrow 1"
Test.equal (Queue.sliceGrow -1   3   7 (Queue.range 1 10)) (Queue.range 4 8)   "sliceGrow 2"
Test.equal (Queue.sliceGrow -1   5  20 (Queue.range 1 10)) (Queue.range 6 10 ++ Queue.repeat 11 -1) "sliceGrow 3"
Test.equal (Queue.sliceGrow -1   0   0 (Queue.range 1 10)) (Queue.one 1)       "sliceGrow 4"
Test.equal (Queue.sliceGrow -1   2   2 (Queue.range 1 10)) (Queue.one 3)       "sliceGrow 5"
Test.equal (Queue.sliceGrow -1 -10   3 (Queue.range 1 10)) (Queue.range 1 4)   "sliceGrow 6"
Test.equal (Queue.sliceGrow -1  10  12 (Queue.range 1  5)) (Queue.repeat 3 -1) "sliceGrow 7"
Test.equal (Queue.sliceGrow -1  -5 -10 (Queue.range 1  5)) (Queue.empty)       "sliceGrow 8"
Test.equal (Queue.sliceGrow -1  10   5 (Queue.range 1  5)) (Queue.empty)       "sliceGrow 9"

Test.equal
    (Queue.pick (fun x -> if isEven x then ValueSome (x * 2) else ValueNone) (que [1;3;5]))
    ValueNone
    "pick 1"

Test.equal
    (Queue.pick (fun x -> if isEven x then ValueSome (x * 2) else ValueNone) (que [1;4;5]))
    (ValueSome 8)
    "pick 2"

Test.equal
    (Queue.takeWhile isEven (que [2;4;6;7;3;2]))
    (Queue.rangeWithStep 2 2 6)
    "takeWhile 1"

Test.equal
    (Queue.skipWhile isEven (que [2;4;6;7;3;2]))
    (Queue.skip 3 (que [2;4;6;7;3;2]))
    "takeWhile 2"

Test.ok    (Queue.equal (Queue ([1;2;3],[],3)) (Queue ([],[3;2;1],3))) "equal 1"
Test.ok    (Queue.equal (Queue ([1],[3;2],3))  (Queue ([],[3;2;1],3))) "equal 2"
Test.notOk (Queue.equal (Queue.range 1 5) (Queue.range 1 6))           "equal 3"
Test.notOk (Queue.equal (que [1;2;3]) (que [1;5;3]))                   "equal 4"

Test.equal
    (Queue.scan (fun q x -> Queue.add x q) Queue.empty (Queue.range 1 5))
    (que [
        Queue.empty
        Queue.one 1
        Queue.range 1 2
        Queue.range 1 3
        Queue.range 1 4
        Queue.range 1 5
    ])
    "scan 1"

Test.equal
    (Queue.scan (fun sum x -> sum + x) 0 (Queue.range 1 5))
    (que [0;1;3;6;10;15])
    "scan 2"

Test.equal (Queue.last (Queue.range 1 10)) (ValueSome 10) "last 1"
Test.equal (Queue.last (Queue.empty))      (ValueNone)    "last 2"


let isGreater x y =
    if   x = y then 0
    elif x > y then 1
    else -1

Test.equal (Queue.compareWith isGreater (Queue.range 1 3) (Queue.range 1 3))  0 "compare 1"
Test.equal (Queue.compareWith isGreater (Queue.range 1 4) (Queue.range 1 3))  1 "compare 2"
Test.equal (Queue.compareWith isGreater (Queue.range 1 3) (Queue.range 1 4)) -1 "compare 3"
Test.equal (Queue.compareWith isGreater (Queue.one 1)     (Queue.one 2))     -1 "compare 4"
Test.equal (Queue.compareWith isGreater (Queue.one 2)     (Queue.one 1))      1 "compare 5"
Test.equal (Queue.compareWith isGreater (que [2;1])       (que [1;2]))        1 "compare 6"
Test.equal (Queue.compareWith isGreater (que [1;2;3])     (que [1;2;4]))     -1 "compare 7"
Test.equal (Queue.compareWith isGreater (que [1;2;3])     (que [0;2;4;8]))    1 "compare 8"

Test.equal
    (Queue.mapFilter add1 isEven (Queue.range 1 10))
    (Queue.range 1 10 |> Queue.map add1 |> Queue.filter isEven)
    "mapFilter"

Test.equal
    (Queue.filterMap isEven add1 (Queue.range 1 10))
    (Queue.range 1 10 |> Queue.filter isEven |> Queue.map add1)
    "filterMap"

Test.equal
    (Queue.mapReduce square (-) (Queue.range 1 3))
    (ValueSome -12)
    "mapReduce 1"

Test.equal
    (Queue.mapReduce square (-) Queue.empty)
    ValueNone
    "mapReduce 2"

Test.equal
    (Queue.mapReduce square (-) (Queue.range 1 3))
    (Queue.range 1 3 |> Queue.map square |> Queue.reduce (-))
    "mapReduce 3"

Test.equal
    (Queue.mapFold square (-) 0 (Queue.range 1 3))
    (-14)
    "mapFold 1"

Test.equal
    (Queue.mapFold square (-) 0 Queue.empty)
    0
    "mapFold 2"

Test.equal
    (Queue.mapFold square (-) 0 (Queue.range 1 3))
    (Queue.range 1 3 |> Queue.map square |> Queue.fold (-) 0)
    "mapfold 3"

Test.equal
    (Queue.foldi (fun i q x -> Queue.add (i,x) q) Queue.empty (Queue.range 1 10))
    (Queue.indexed (Queue.range 1 10))
    "foldi"

Test.equal (Queue.repeat  0 0) (Queue.empty)     "repeat 1"
Test.equal (Queue.repeat  1 0) (Queue.one 0)     "repeat 2"
Test.equal (Queue.repeat -5 0) (Queue.empty)     "repeat 3"
Test.equal (Queue.repeat  5 0) (que [0;0;0;0;0]) "repeat 4"
Test.equal (Queue.repeat  3 1) (que [1;1;1])     "repeat 5"

let evens,odds = Queue.partition isEven (Queue.range 1 10)
Test.ok (Queue.forall isEven          evens) "partition 1"
Test.ok (Queue.forall (not << isEven)  odds) "partition 2"

Test.equal (Queue.min (que [1;10;3])) (ValueSome 1)  "min 1"
Test.equal (Queue.min Queue.empty)    (ValueNone)    "min 2"
Test.equal (Queue.max (que [1;10;3])) (ValueSome 10) "max 1"
Test.equal (Queue.max Queue.empty)    (ValueNone)    "max 2"

Test.equal
    (Queue.minBy String.length (que ["Hallo";"Welt"]))
    (ValueSome "Welt")
    "minBy 1"

Test.equal
    (Queue.minBy String.length Queue.empty)
    (ValueNone)
    "minBy 2"

Test.equal
    (Queue.maxBy String.length (que ["Hallo";"Welt"]))
    (ValueSome "Hallo")
    "maxBy 1"

Test.equal
    (Queue.maxBy String.length Queue.empty)
    (ValueNone)
    "maxBy 2"

// Run Tests
let args = Array.skip 1 <| System.Environment.GetCommandLineArgs()
runTestsWithCLIArgs [] args (testList "Main" (List.ofSeq tests)) |> ignore
