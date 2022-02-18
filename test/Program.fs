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

    let eqFloat actual expected msg =
        tests.Add (test msg {
            Expect.floatClose Accuracy.high  actual expected msg
        })


// Utility functions
let add1   x = x + 1
let double x = x * 2
let isEven x = x % 2 = 0
let square x = x * x

let eqFloat x y =
    (abs (x - y)) < 0.000001

let eqFloats expected source =
    (Queue.forall (fun x -> x = true)
        (Queue.map2 eqFloat expected source))


// Actual Tests
Test.equal (Queue.length (Queue.range 1 10))   10                  "range length"
Test.equal (Queue.range 1 10)    (Queue.ofList [1..10])            "range 1"
Test.equal (Queue.range 10 1)    (Queue.ofList (List.rev [1..10])) "range 2"
Test.equal (Queue.range 10 1)    (Queue.rangeWithStep 10 -1 1)     "range 4"
Test.equal (Queue.range 10 1)    (Queue.ofList [10..-1..1])        "range 5"
Test.equal (Queue.range 10 10)   (Queue.ofList [10..10])           "range 6"

Test.ok       (eqFloats (Queue.range 1.5 4.4) (Queue [1.5; 2.5; 3.5])) "range 7"
Test.notEqual (Queue.range 10 1) (Queue.ofList [10..1])                "range 8"

Test.equal (Queue.rangeWithStep 1     2 10) (Queue.ofList [1 ..   2..10]) "rangeWithStep 1"
Test.equal (Queue.rangeWithStep 1   100 10) (Queue.ofList [1 .. 100..10]) "rangeWithStep 2"
Test.equal (Queue.rangeWithStep 10   -1  0) (Queue.ofList [10..  -1.. 0]) "rangeWithStep 3"
Test.equal (Queue.rangeWithStep 10    1  0) (Queue.ofList [10..   1.. 0]) "rangeWithStep 4"
Test.equal (Queue.rangeWithStep 10 -100  0) (Queue.ofList [10..-100.. 0]) "rangeWithStep 5"
Test.equal (Queue.rangeWithStep 10  100 10) (Queue.ofList [10.. 100..10]) "rangeWithStep 6"
Test.equal (Queue.rangeWithStep 10 -100 10) (Queue.ofList [10..-100..10]) "rangeWithStep 7"
Test.equal (Queue.rangeWithStep 1    -1 10) (Queue.ofList [1 ..  -1..10]) "rangeWithStep 8"

Test.ok
    (eqFloats
        (Queue.rangeWithStep 1.0 0.2 2.3)
        (Queue [1.0;1.2;1.4;1.6;1.8;2.0;2.2]))
    "rangeWithStep float"

Test.equal
    (Queue.empty |> Queue.add 1 |> Queue.add 2 |> Queue.add 3)
    (Queue.range 1 3)
    "add"

Test.equal
    (Queue.append (Queue.range 1 3) (Queue.range 4 6) |> Queue.tail)
    (Queue.range 2 6)
    "append 1"

Test.equal
    (Queue.append (Queue.range 1 3) (Queue.range 4 6))
    (Queue.range 1 3 ++ Queue.range 4 6)
    "append ++"

Test.equal    (Queue.range 1 10) (Queue.range 1 10) "Two equal Queues"
Test.notEqual (Queue.range 1 5)  (Queue.range 1 10) "None equal Queues"

let q1t = Queue.range 1 10
Test.equal    [1..10] [for x in q1t -> x] "Test Sequence Expression 1"
Test.equal    [1..10] [for x in q1t -> x] "Test Sequence Expression 2"
Test.notEqual [1..10] [for x in Queue.range 1 5 -> x] "Test notEqual for Expression"

let q1to6 = Queue.range 1 6
Test.ok    (Queue.contains  1 q1to6)   "Queue Contains 1"
Test.notOk (Queue.contains 10 q1to6)   "Queue not Contains 10"
Test.equal (Queue.map double q1to6)    (Queue [2;4;6;8;10;12]) "map"
Test.equal (Queue.filter isEven q1to6) (Queue [2;4;6])         "filter"

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
Test.equal intern (ValueSome (4,Queue [5;6;7;8;9])) "Queue.head"

let q13 = Queue.range 1 3

Test.equal (Queue.toSeq   q1to6 |> Seq.toList) [1;2;3;4;5;6]  "toSeq 1"
Test.equal (List.ofSeq    q1to6)               [1;2;3;4;5;6]  "List.ofSeq"
Test.equal (Queue.toArray q1to6)              [|1;2;3;4;5;6|] "toArray"
Test.equal (Queue.toList  q1to6)               [1;2;3;4;5;6]  "toList 2"
Test.equal (Queue.ofSeq   (seq [1;2;3]))       q13            "ofSeq"
Test.equal (Queue.ofArray [|1;2;3|])           q13            "ofArray"
Test.equal (Queue.ofList [1;2;3])              q13            "ofList"
Test.equal (Queue.take -3 q13)                 Queue.empty    "take -3"
Test.equal (Queue.take  2 q13)                (Queue [1;2])   "take 2"
Test.equal (Queue.take  5 q13)                 q13            "take 5"
Test.equal (Queue.skip -3 q13)                 q13            "skip -3"
Test.equal (Queue.skip  2 q13)                (Queue [3])     "skip 2"
Test.equal (Queue.skip  5 q13)                (Queue.empty)   "skip 5"
Test.equal (Queue.updateAt 3 10   (Queue.ofSeq [1..10])) (Queue [1;2;3;10;5;6;7;8;9;10]) "updateAt 3 10"
Test.equal (Queue.updateAt 100 10 (Queue.range 1 10))    (Queue.range 1 10)              "updateAt 100 10"
Test.equal (Queue.updateAt -5 10  (Queue.range 1 10))    (Queue.range 1 10)              "updateAt -5 10"

Test.equal
    (Queue.mapi (fun i x -> (i,x)) (Queue.range 1 10))
    (Queue.indexed (Queue.range 1 10))
    "mapi"

Test.equal
    (Queue.append (Queue.ofSeq [1..3]) (Queue.ofSeq [4..6]))
    (Queue.range 1 6)
    "append"

Test.equal
    (Queue.map2 (fun x y -> x + y) (Queue.ofSeq [1..3]) (Queue.ofSeq [10..15]))
    (Queue [11;13;15])
    "map2"

Test.equal
    (Queue.map3 (fun x y z -> x + y + z) (Queue.range 1 3) (Queue.range 3 1) (Queue.repeat 3 6))
    (Queue.repeat 3 10)
    "map3"

Test.equal
    (Queue.map4 (fun a b c d -> a + b + c + d) (Queue [1;2]) (Queue [10;11]) (Queue [5;5;4;12]) (Queue [9;9;9]))
    (Queue [25;27])
    "map4"

let cartesian =
    let cartesian =
        Queue [1..5]  |> Queue.bind (fun x ->
        Queue [10;20] |> Queue.bind (fun y ->
            Queue [x * y]))

    Test.equal
        cartesian
        (Queue [10;20;20;40;30;60;40;80;50;100])
        "Cartesian through bind"

    Test.equal
        cartesian
        (Queue.lift2 (fun x y -> x * y) (Queue.range 1 5) (Queue [10;20]))
        "cartesian with Queue.lift2"

Test.equal
    (Queue.concat (Queue [Queue.range 1 3; Queue.range 4 6; Queue.range 7 9]))
    (Queue.range 1 9)
    "concat"

Test.equal
    (Queue.init 5 double)
    (Queue.range 0 4 |> Queue.map double)
    "init"

Test.equal
    (Queue.indexed (Queue ["A";"B";"C"]))
    (Queue [(0,"A"); (1,"B"); (2,"C")])
    "indexed"

Test.equal
    (Queue.indexed (Queue ["A";"B";"C"]))
    (Queue.zip (Queue.range 0 2) (Queue ["A";"B";"C"]))
    "indexed 2"

Test.equal
    (Queue.isEmpty (Queue.skip 2 (Queue [1;2])))
    true
    "isEmpty"

Test.equal (Queue.item  3 (Queue [1..10])) (ValueSome 4) "item 3"
Test.equal (Queue.item -2 (Queue [1..10])) ValueNone     "item -2"
Test.equal (Queue.item 10 (Queue [1..10])) ValueNone     "item 10"

let evenSquare x =
    if isEven x then ValueSome (square x)
                else ValueNone

let squareEven x =
    let sqr = square x
    if isEven sqr then ValueSome sqr else ValueNone

Test.equal
    (Queue.choose evenSquare (Queue [1..10]))
    (Queue [4;16;36;64;100])
    "choose"

Test.equal
    (Queue.choose evenSquare (Queue [1..10]))
    (Queue.filterMap isEven square (Queue.range 1 10))
    "choose vs filterMap"

Test.equal
    (Queue.choose squareEven (Queue.range 1 10))
    (Queue.mapFilter square isEven (Queue.range 1 10))
    "choose vs mapFilter"

Test.equal
    (Queue.range 1 10 |> Queue.filter isEven |> Queue.map square)
    (Queue.filterMap isEven square (Queue.range 1 10))
    "filter->map vs filterMap"

Test.equal
    (Queue.range 1 10 |> Queue.map square |> Queue.filter isEven)
    (Queue.mapFilter square isEven (Queue.range 1 10))
    "map->filter vs mapFilter"

Test.equal
    (Queue.zip (Queue ["A";"B";"C";"D"]) (Queue [1;2;3]))
    (Queue ["A",1; "B",2; "C",3])
    "zip"

Test.equal
    (Queue.reduce (+) (Queue [1..5]))
    (ValueSome 15)
    "reduce"

Test.equal
    (Queue.reduce (+) (Queue [10]))
    (ValueSome 10)
    "reduce 2"

Test.equal
    (Queue.reduce (+) Queue.empty)
    ValueNone
    "reduce 3"

Test.equal
    (Queue.reduce (+) (Queue ["A";"B";"C"]))
    (ValueSome "ABC")
    "reduce 4"

Test.ok
    (Queue ["ABC"; "AGG"] |>  Queue.forall (fun s -> s.StartsWith "A"))
    "forall"

Test.notOk
    (Queue ["ABC"; "BGG"] |>  Queue.forall (fun s -> s.StartsWith "A"))
    "forall 2"

Test.ok
    (Queue.any isEven (Queue [1;3;4]))
    "any"

Test.notOk
    (Queue.any isEven (Queue [1;3;5]))
    "any 2"

Test.eqFloat 5.8 (Queue.sum (Queue [1.1; 1.5; 3.2])) "Sum of Float"

Test.equal
    (Queue.sum (Queue [1; 2; 3]))
    6
    "Sum of Int"

Test.equal
    (Queue.sort (Queue [5;12;5;1;-12]))
    (Queue.empty |> Queue.addMany [-12;1;5;5;12])
    "sort"

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

let lift4 =
    let add4 x y z w = x + y + z + w
    Test.equal
        (Queue.lift4 add4 (Queue [3;7]) (Queue [10;20]) (Queue.one 100) (Queue [1000;2000;3000]))
        (Queue [1113;2113;3113;1123;2123;3123;1117;2117;3117;1127;2127;3127])
        "lift4"
    ()

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
Test.equal complex (Queue [64;100;25;49;1;2;3]) "Complex equal"
Test.equal
    (Queue.skipWhile isEven complex |> Queue.sort)
    (Queue [1;2;3;25;49])
    "skipWhile isEven sort"

Test.equal
    (Queue.insertAt -1 100 (Queue.range 1 5))
    (Queue [1;2;3;4;5])
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
    (Queue.concat (Queue [Queue.one 1; Queue.one 100; Queue.range 2 10]))
    "insertAt 4"

Test.equal
    (Queue.insertAt 3 100 (Queue [6;4;2]))
    (Queue [6;4;2;100])
    "insertAt 5"

Test.equal
    (Queue.insertAt 4 100 (Queue [6;4;2]))
    (Queue [6;4;2])
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
    (Queue.allPairs (Queue ["A";"B"]) (Queue [1;2]))
    (Queue ["A",1; "A",2; "B",1; "B",2])
    "allPairs 1"

Test.equal
    (Queue.allPairs (Queue ["A";"B"]) (Queue [1;2]))
    (Queue.lift2 (fun x y -> x,y) (Queue ["A";"B"]) (Queue [1;2]))
    "allPairs 2"

Test.equal
    (Queue.allPairs (Queue ["A";"B"]) (Queue [1;2]))
    ((Queue ["A";"B"]) |> Queue.bind (fun l -> (Queue [1;2]) |> Queue.bind (fun n -> Queue.one (l,n))))
    "allPairs 3"

Test.equal
    (Queue.allPairs (Queue ["A";"B"]) (Queue [1;2]))
    (Queue [for l in Queue ["A";"B"] do for n in Queue [1;2] do yield l,n])
    "allPairs 4"

Test.equal
    (Queue.chunkBySize 3 (Queue [1..10]))
    (Queue [Queue [1;2;3]; Queue [4;5;6]; Queue [7;8;9]; Queue.one 10])
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
    (Queue.zip3 (Queue [1;2;3]) (Queue [10;20]) (Queue [4;4;4;4]))
    (Queue.one (1,10,4) |> Queue.add (2,20,4))
    "Zip3"

Test.equal
    (Queue.zip4 (Queue [1;2;3]) (Queue [10;20]) (Queue [4;4;4;4]) (Queue [3;3;3]))
    (Queue.one (1,10,4,3) |> Queue.add (2,20,4,3))
    "Zip4"

let countBy =
    let nameOccurrences = Queue.countBy id (Queue ["Hallo";"Hallo";"Welt";"Hallo"])
    Test.equal (Queue.length nameOccurrences) 2 "countBy 3"

    Test.equal
        (Queue.find (fun (str,n) -> str = "Hallo") nameOccurrences)
        (ValueSome ("Hallo", 3))
        "CountBy 1"

    Test.equal
        (Queue.find (fun (str,n) -> str = "Welt") nameOccurrences)
        (ValueSome ("Welt", 1))
        "CountBy 2"

Test.equal (Queue.rev (Queue.range 1 5))   (Queue [5;4;3;2;1]) "rev 1"
Test.equal (Queue.rev (Queue [5;4;3;2;1])) (Queue.range 1 5) "rev 2"
Test.equal (Queue.rev Queue.empty)          Queue.empty      "rev 3"
Test.equal (Queue.range 1 5)               (Queue.rev (Queue.rev (Queue.range 1 5))) "rev 4"

Test.equal
    (Queue.exactlyOne (Queue.tail (Queue.rev (Queue.one 100 |> Queue.add 10))))
    (ValueSome 100)
    "rev 5"

Test.equal
    (Queue.rev (Queue.append (Queue.range 1 10) (Queue.rangeWithStep 6 -1 0)))
    (Queue.append (Queue.range 0 6) (Queue.rangeWithStep 10 -1 1))
    "rev 6"

Test.equal (Queue.exactlyOne    (Queue [5;1]))               (ValueNone   ) "exactlyOne 1"
Test.equal (Queue.exactlyOne    (Queue [5;1] |> Queue.tail)) (ValueSome  1) "exactlyOne 2"
Test.equal (Queue.findIndex     isEven (Queue [3;3;5;2]))    (ValueSome  3) "findIndex 1"
Test.equal (Queue.findIndex     isEven (Queue [3;3;5]))      (ValueNone   ) "findIndex 2"
Test.equal (Queue.findIndex     isEven (Queue.range 1 5))    (ValueSome  1) "findIndex 3"
Test.equal (Queue.findIndex     isEven (Queue [1;3;5]))      (ValueNone   ) "findIndex 4"
Test.equal (Queue.findIndexBack isEven (Queue.range 1 5))    (ValueSome  3) "findIndexBack 1"
Test.equal (Queue.findIndexBack isEven (Queue [5;3;1]))      (ValueNone   ) "findIndexBack 2"
Test.equal (Queue.find          isEven (Queue.range 9 20))   (ValueSome 10) "find 1"
Test.equal (Queue.find          isEven (Queue [9;13;15]))    (ValueNone   ) "find 2"
Test.equal (Queue.findBack      isEven (Queue.range 9 19))   (ValueSome 18) "findBack 1"
Test.equal (Queue.findBack      isEven (Queue [15;17;19]))   (ValueNone   ) "findBack 2"

Test.equal
    (Queue.updateAtGrow 0 2 100 (Queue.range 1 10))
    (Queue [1;2;100] |> Queue.addMany (Queue.range 4 10))
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
    (Queue [1;2;1;2;3;3;4;5])
    "insertManyAt 1"

Test.equal
    (Queue.insertManyAt 10 [1;2;3] (Queue.range 1 5))
    (Queue.range 1 5)
    "insertManyAt 2"

Test.equal
    (Queue.insertManyAt  0 [1;2;3] (Queue.range 10 15))
    (Queue [1;2;3;10;11;12;13;14;15])
    "insertManyAt 3"

Test.equal
    (Queue.insertManyAt -1 [1;2;3] (Queue.range 10 15))
    (Queue.tail (Queue.one 0 |> Queue.addMany (Queue.range 10 15)))
    "insertManyAt 4"

Test.equal
    (Queue.insertManyAt  4 [1;2;3] (Queue.range 10 20))
    (Queue [10;11;12;13;1;2;3;14;15;16;17;18;19;20])
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
    (Queue [1;2;3;4;5;0;0;0;0;0;1;2;3])
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
    (Queue.concat (Queue [(Queue.range 10 20); (Queue.repeat 9 0); (Queue.range 1 3);]))
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
    (Queue [0;1;2;3])
    "insertManyAtGrow 7"

Test.equal
    (Queue.sliceGrow 0 19 23 (Queue.insertManyAtGrow 0 20 [1;2;3] (Queue.range 10 20)))
    (Queue [0;1;2;3;0])
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
    (Queue.pick (fun x -> if isEven x then ValueSome (x * 2) else ValueNone) (Queue [1;3;5]))
    ValueNone
    "pick 1"

Test.equal
    (Queue.pick (fun x -> if isEven x then ValueSome (x * 2) else ValueNone) (Queue [1;4;5]))
    (ValueSome 8)
    "pick 2"

Test.equal
    (Queue.takeWhile isEven (Queue [2;4;6;7;3;2]))
    (Queue.rangeWithStep 2 2 6)
    "takeWhile 1"

Test.equal
    (Queue.skipWhile isEven (Queue [2;4;6;7;3;2]))
    (Queue.skip 3 (Queue [2;4;6;7;3;2]))
    "takeWhile 2"

Test.ok    (Queue.equal (Queue ([1;2;3],[],3)) (Queue ([],[3;2;1],3))) "equal 1"
Test.ok    (Queue.equal (Queue ([1],[3;2],3))  (Queue ([],[3;2;1],3))) "equal 2"
Test.notOk (Queue.equal (Queue.range 1 5) (Queue.range 1 6))           "equal 3"
Test.notOk (Queue.equal (Queue [1;2;3]) (Queue [1;5;3]))               "equal 4"

Test.equal
    (Queue.scan (fun q x -> Queue.add x q) Queue.empty (Queue.range 1 5))
    (Queue [
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
    (Queue [0;1;3;6;10;15])
    "scan 2"

Test.equal
    (Queue.scan (fun sum x -> sum + x) 0 (Queue.empty))
    (Queue [0])
    "scan 3"

Test.equal (Queue.last (Queue.range 1 10)) (ValueSome 10) "last 1"
Test.equal (Queue.last (Queue.empty))      (ValueNone)    "last 2"

let compare =
    let isGreater x y =
        if   x = y then 0
        elif x > y then 1
        else -1

    Test.equal (Queue.compareWith isGreater (Queue.range 1 3) (Queue.range 1 3))    0 "compareWith 1"
    Test.equal (Queue.compareWith isGreater (Queue.range 1 4) (Queue.range 1 3))    1 "compareWith 2"
    Test.equal (Queue.compareWith isGreater (Queue.range 1 3) (Queue.range 1 4))   -1 "compareWith 3"
    Test.equal (Queue.compareWith isGreater (Queue.one 1)     (Queue.one 2))       -1 "compareWith 4"
    Test.equal (Queue.compareWith isGreater (Queue.one 2)     (Queue.one 1))        1 "compareWith 5"
    Test.equal (Queue.compareWith isGreater (Queue [2;1])     (Queue [1;2]))        1 "compareWith 6"
    Test.equal (Queue.compareWith isGreater (Queue [1;2;3])   (Queue [1;2;4]))     -1 "compareWith 7"
    Test.equal (Queue.compareWith isGreater (Queue [1;2;3])   (Queue [0;2;4;8]))    1 "compareWith 8"

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

Test.equal (Queue.repeat  0 0) (Queue.empty)       "repeat 1"
Test.equal (Queue.repeat  1 0) (Queue.one 0)       "repeat 2"
Test.equal (Queue.repeat -5 0) (Queue.empty)       "repeat 3"
Test.equal (Queue.repeat  5 0) (Queue [0;0;0;0;0]) "repeat 4"
Test.equal (Queue.repeat  3 1) (Queue [1;1;1])     "repeat 5"

let partition =
    let evens,odds = Queue.partition isEven (Queue.range 1 10)
    Test.ok (Queue.forall isEven          evens) "partition 1"
    Test.ok (Queue.forall (not << isEven)  odds) "partition 2"

Test.equal (Queue.min (Queue [1;10;3])) (ValueSome 1)  "min 1"
Test.equal (Queue.min Queue.empty)    (ValueNone)      "min 2"
Test.equal (Queue.max (Queue [1;10;3])) (ValueSome 10) "max 1"
Test.equal (Queue.max Queue.empty)    (ValueNone)      "max 2"

Test.equal
    (Queue.minBy String.length (Queue ["Hallo";"Welt"]))
    (ValueSome "Welt")
    "minBy 1"

Test.equal
    (Queue.minBy String.length Queue.empty)
    (ValueNone)
    "minBy 2"

Test.equal
    (Queue.maxBy String.length (Queue ["Hallo";"Welt"]))
    (ValueSome "Hallo")
    "maxBy 1"

Test.equal
    (Queue.maxBy String.length Queue.empty)
    (ValueNone)
    "maxBy 2"

Test.equal
    (Queue.intersperse "on" (Queue.empty))
    (Queue.empty)
    "intersperse 1"

Test.equal
    (Queue.intersperse "on" (Queue.one "Hallo"))
    (Queue.one "Hallo")
    "intersperse 2"

Test.equal
    (Queue.intersperse "on" (Queue ["Foo";"Bar"]))
    (Queue.empty |> Queue.addMany ["Foo";"on";"Bar"])
    "intersperse 3"

Test.equal
    (Queue.intersperse 0 (Queue.range 1 5))
    (Queue.empty |> Queue.addMany [1;0;2;0;3;0;4;0;5])
    "intersperse 4"

Test.equal ((Queue [1;2;3])   < (Queue [1;2;3])) false "Comparision 1"
Test.equal ((Queue [1;2;3])   < (Queue [2;2;3])) true  "Comparision 2"
Test.equal ((Queue [2;2;3])   < (Queue [1;2;3])) false "Comparision 3"
Test.equal ((Queue [1;2;3])   < (Queue [2;2]))   true  "Comparision 4"
Test.equal ((Queue [1;2;3])   < (Queue [0;2]))   false "Comparision 5"
Test.equal ((Queue [1;2])     < (Queue [2;2;3])) true  "Comparision 6"
Test.equal ((Queue [1;2])     < (Queue [0;2;3])) false "Comparision 7"
Test.equal ((Queue [1;2;3])   > (Queue [1;2;3])) false "Comparision 8"
Test.equal ((Queue [1;2;3])   > (Queue [2;2;3])) false "Comparision 9"
Test.equal ((Queue [2;2;3])   > (Queue [1;2;3])) true  "Comparision 10"
Test.equal ((Queue [1;2;3])   > (Queue [2;2]))   false "Comparision 11"
Test.equal ((Queue [1;2;3])   > (Queue [1;2]))   true  "Comparision 12"
Test.equal ((Queue [1;2])     > (Queue [2;2;3])) false "Comparision 13"
Test.equal ((Queue [1;2])     > (Queue [0;2;3])) true  "Comparision 14"
Test.equal (Queue.empty       < Queue.empty)     false "Comparision 15"
Test.equal (Queue.empty       > Queue.empty)     false "Comparision 16"
Test.equal (Queue.empty       < Queue.one 1)     true  "Comparision 17"
Test.equal (Queue.one 1       < Queue.empty)     false "Comparision 18"
Test.equal (Queue.empty       > Queue.one 1)     false "Comparision 19"
Test.equal (Queue.one 1       > Queue.empty)     true  "Comparision 20"
Test.equal ((Queue ["A";"B"]) < Queue ["B";"A"]) true  "Comparision 21"
Test.equal ((Queue ["B";"A"]) < Queue ["A";"B"]) false "Comparision 22"
Test.equal ((Queue ["A";"B"]) > Queue ["B";"A"]) false "Comparision 23"
Test.equal ((Queue ["B";"A"]) > Queue ["A";"B"]) true  "Comparision 24"

Test.equal (Queue.compare (Queue.range 1 3) (Queue.range 2 6)) -1 "compare 1"
Test.equal (Queue.compare (Queue.range 2 6) (Queue.range 1 3))  1 "compare 2"
Test.equal (Queue.compare (Queue.range 1 3) (Queue.range 1 3))  0 "compare 3"

let byStringLength str1 str2 =
    LanguagePrimitives.GenericComparison (String.length str1) (String.length str2)

Test.equal
    (Queue.sortWith byStringLength (Queue ["Hallo";"Welt";"Du";"Sau"]))
    (Queue ["Du";"Sau";"Welt";"Hallo"])
    "sortWith 1"

Test.equal
    (Queue.sortWith byStringLength (Queue ["Hallo";"Welt";"Du";"Sau"]))
    (Queue.sortBy   String.length  (Queue ["Hallo";"Welt";"Du";"Sau"]))
    "sortWith 2"

Test.equal
    (Queue.sort (Queue ["Hallo";"Welt";"Du";"Sau"]))
    (Queue ["Du";"Hallo";"Sau";"Welt"])
    "sort 1"

Test.equal
    (Queue.sort (Queue [4;2;3;1;7;5;9;8;6;10]))
    (Queue.range 1 10)
    "sort 2"

Test.equal
    (Queue.sortDescending (Queue.range 1 10))
    (Queue.rev (Queue.range 1 10))
    "sortDescending 1"

Test.equal
    (Queue.sortDescending (Queue ["Hallo";"Welt";"Du";"Sau"]))
    (Queue ["Welt";"Sau";"Hallo";"Du"])
    "sortDescending 2"

Test.equal
    (Queue.sortDescending (Queue [4;2;3;1;7;5;9;8;6;10]))
    (Queue.rev (Queue.range 1 10))
    "sortDescending 3"

Test.equal
    (Queue.sortBy String.length (Queue ["Hallo";"Welt";"Du";"Sau"]))
    (Queue ["Du";"Sau";"Welt";"Hallo"])
    "sortBy 1"

Test.equal
    (Queue.sortByDescending String.length (Queue ["Hallo";"Welt";"Du";"Sau"]))
    (Queue ["Hallo";"Welt";"Sau";"Du"])
    "sortBy 2"

Test.equal
    (ValueOption.map (eqFloat 5.5) (Queue.average (Queue.range 1.0 10.0)))
    (ValueSome true)
    "average 1"

Test.equal
    (Queue.average (Queue.range 1.0m 10.0m))
    (ValueSome 5.5m)
    "average 2"

Test.equal
    (Queue.average (Queue.rangeWithStep 0.0m 0.1m 1.0m))
    (ValueSome 0.5m)
    "average 3"

Test.equal
    (Queue.average (Queue.rangeWithStep 0.1m 0.1m 1.0m))
    (ValueSome 0.55m)
    "average 4"

Test.equal
    (Queue.average (Queue.one 1.0 |> Queue.tail))
    (ValueNone)
    "average 5"

Test.equal
    (Queue.sumBy snd (Queue [("A",1);("B",2);("C",3)]))
    ((Queue [("A",1);("B",2);("C",3)]) |> Queue.map snd |> Queue.sum)
    "sumBy"

Test.equal (Queue.sumBy snd Queue.empty)   0 "sumBy on Empty 1"
Test.equal (Queue.sumBy snd Queue.empty) 0.0 "sumBy on Empty 2"

let indexer =
    let r1t = Queue.range 1 10
    Test.equal (r1t.[5])        (ValueSome 6)                             "Indexer 1"
    Test.equal (r1t.[0])        (ValueSome 1)                             "Indexer 2"
    Test.equal (r1t.[10])       (ValueNone)                               "Indexer 3"
    Test.equal (r1t.[2..5])     (Queue [3;4;5;6])                         "GetSlice 1"
    Test.equal (r1t.[2..5])     (Queue.slice 2 5 r1t)                     "GetSlice 2"
    Test.equal (r1t.[2..])      (Queue [3;4;5;6;7;8;9;10])                "GetSlice 3"
    Test.equal (r1t.[2..])      (Queue.slice 2 (Queue.lastIndex r1t) r1t) "GetSlice 4"
    Test.equal (r1t.[..3])      (Queue [1;2;3;4])                         "GetSlice 5"
    Test.equal (r1t.[..3])      (Queue.slice 0 3 r1t)                     "GetSlice 6"
    Test.equal (r1t.[*])        (r1t)                                     "GetSlice 7"
    Test.equal (r1t.[-2..(-1)]) (Queue.empty)                             "GetSlice 8"
    Test.equal (r1t.[-2..5])    (Queue.slice 0 5 r1t)                     "GetSlice 9"
    Test.equal (r1t.[-2..5])    (Queue.range 1 6)                         "GetSlice 10"

Test.equal
    (Queue.distinct (Queue.append (Queue.range 1 5) (Queue.range 1 5)))
    (Queue.range 1 5)
    "Distinct 1"

Test.equal
    (Queue.distinct (Queue [1;3;1;5;1;4;6;3;9;5;2]))
    (Queue [1;3;5;4;6;9;2])
    "Distinct 2"

Test.equal
    (Queue.distinctBy snd (Queue.zip (Queue.range 1 100) (Queue [1;3;1;5;1;4;6;3;9;5;2])))
    (Queue [(1,1); (2,3); (4,5); (6,4); (7,6); (9,9); (11,2)])
    "DistinctBy 1"

// GroupBy
let groupBy =
    let gb          = Queue.groupBy String.length (Queue ["Hallo";"Welt";"Wie";"Geht";"Es";"Dir";"Jetzt?"])
    let getLength i = fun (x,_) -> i = x

    Test.equal (Queue.length gb)            5                                       "groubBy 1"
    Test.equal (Queue.find (getLength 2) gb) (ValueSome (2, Queue ["Es"]))          "groupBy 2"
    Test.equal (Queue.find (getLength 3) gb) (ValueSome (3, Queue ["Wie";"Dir"]))   "groubBy 3"
    Test.equal (Queue.find (getLength 4) gb) (ValueSome (4, Queue ["Welt";"Geht"])) "groubBy 4"
    Test.equal (Queue.find (getLength 5) gb) (ValueSome (5, Queue ["Hallo"]))       "groubBy 5"
    Test.equal (Queue.find (getLength 6) gb) (ValueSome (6, Queue ["Jetzt?"]))      "groubBy 6"

Test.equal
    (ValueOption.map (eqFloat 3.0) (Queue.averageBy snd (Queue ["A",1.0; "B",3.0; "C",5.0])))
    (ValueSome true)
    "averageBy 1"

Test.equal
    (Queue.averageBy snd (Queue ["A",1.0m; "B",3.0m; "C",5.0m]))
    (ValueSome 3.0m)
    "averageBy 2"

Test.equal
    (Queue.fold2 (fun state x y -> Queue.add (x+y) state) Queue.empty (Queue [12;3;-10]) (Queue [8;7;10;3]))
    (Queue [20;10;0])
    "fold2"

Test.equal
    (Queue.fold3 (fun state x y z -> Queue.add (x+y+z) state) Queue.empty (Queue [12;3;-10]) (Queue [8;7;10;3]) (Queue [1;2]))
    (Queue [21;12])
    "fold3"

Test.equal
    (Queue.fold4 (fun state x y z w -> Queue.add (x+y+z+w) state) Queue.empty (Queue [12;3;-10]) (Queue [8;7;10;3]) (Queue [1;2]) (Queue [-5;3]))
    (Queue [16;15])
    "fold4"

Test.equal
    (Queue.foldi2 (fun i state x y -> Queue.add (i,x,y) state) Queue.empty (Queue.range 1 6) (Queue.range 10 15))
    (Queue [(0,1,10); (1,2,11); (2,3,12); (3,4,13); (4,5,14); (5,6,15)])
    "foldi2 1"

Test.equal
    (Queue.foldi2 (fun i state x y -> Queue.add (i, x+y) state) Queue.empty (Queue.range 1 3) (Queue.range 10 15))
    (Queue [(0,11); (1,13); (2,15)])
    "foldi2 2"

Test.equal
    (Queue.ofList (Queue.foldi3 (fun i state x y z -> (i,z,(x+y)) :: state) [] (Queue.range 1 3) (Queue [10;10]) (Queue ["A";"B";"C"])))
    (Queue.empty |> Queue.add (1,"B",12) |> Queue.add (0,"A",11))
    "foldi3 1"

Test.equal
    (Queue.ofList
        (Queue.foldi4 (fun i state x y z w -> (i,z,(x+y+w)) :: state)
            []                    // state
            (Queue.range 1 3)     // x
            (Queue [10;10])       // y
            (Queue ["A";"B";"C"]) // z
            (Queue [1;1])))       // w
    (Queue.empty |> Queue.add (1,"B",13) |> Queue.add (0,"A",12))
    "foldi4 1"

Test.equal
    (Queue.ofList (List.fold  (fun l x -> x :: l)        []          [1..10]))
    (Queue.rev    (Queue.fold (fun q x -> Queue.add x q) Queue.empty (Queue.range 1 10)))
    "Behaviour Queue vs List"

let iter =
    let mutable xs = []
    Queue.iter (fun x -> xs <- x :: xs) (Queue.range 1 10)
    Test.equal xs (List.rev [1..10]) "iter"

let iter2 =
    let mutable xs = []
    Queue.iter2 (fun x y -> xs <- (x,y) :: xs) (Queue.range 1 3) (Queue.range 10 100)
    Test.equal xs [(3,12); (2,11); (1,10)] "iter2"

let iteri =
    let mutable xs = []
    Queue.iteri (fun i x -> xs <- (i,x*2) :: xs) (Queue.range 1 5)
    Test.equal
        (Queue.range 1 5 |> Queue.map (fun x -> x*2) |> Queue.indexed)
        (Queue.rev (Queue.ofList xs))
        "iteri"

let iteri2 =
    let mutable xs = []
    Queue.iteri2 (fun i x y -> xs <- (i,x+y) :: xs) (Queue.range 1 3) (Queue.range 100 200)
    Test.equal
        ((Queue.indexed (Queue.zip (Queue.range 1 3) (Queue.range 100 200))) |> Queue.map (fun (i,(x,y)) -> (i,x+y)))
        (Queue.rev (Queue.ofList xs))
        "iteri2"

Test.equal
    (Queue.mapi2 (fun  idx  x y   -> idx, x+y)                           (Queue.range 10 20) (Queue.range 20 25))
    (Queue.map   (fun (idx,(x,y)) -> idx, x+y) (Queue.indexed (Queue.zip (Queue.range 10 20) (Queue.range 20 25))))
    "mapi2"

Test.equal
    (Queue.mapi3 (fun idx x y z -> idx, x+y+z) (Queue [1;3;5]) (Queue [100;200]) (Queue [10;50]))
    (Queue [(0,111); (1,253)])
    "mapi3"

Test.equal
    (Queue.mapi4 (fun idx x y z w -> idx, x+y+z+w) (Queue [1;3;5;9]) (Queue [100;200;300]) (Queue [10;50;70]) (Queue [7;3]))
    (Queue [(0,118); (1,256)])
    "mapi4"

Test.equal
    (Queue.pairwise (Queue.range 1 5))
    (Queue [(1,2); (2,3); (3,4); (4,5)])
    "pairwise 1"

Test.equal
    (Queue.pairwise (Queue.range 1 10))
    (Queue.zip (Queue.range 1 10) (Queue.tail (Queue.range 1 10)))
    "pairwise 2"

Test.equal
    (Queue.pairwise Queue.empty)
    (Queue.empty)
    "pairwise 3"

Test.equal
    (Queue.pairwise (Queue.one 1))
    (Queue.empty)
    "pairwise 4"

Test.equal
    (Queue.ofList (1  :: 2 :: 3 :: []))
    (Queue.prepend 1 (Queue.prepend 2 (Queue.prepend 3 Queue.empty)))
    "prepend 1"

Test.equal
    (Queue.ofList (List.fold (fun state x -> x :: state) [] [1..10]))
    (Queue.fold (fun state x -> Queue.prepend x state) Queue.empty (Queue.range 1 10))
    "prepend 2"

let permuts =
    let permuts = Queue.permutations (Queue.range 1 3)

    Test.equal (Seq.length permuts) 6                      "permuts 1"
    Test.equal (Seq.contains (Queue [1;2;3]) permuts) true "permuts 2"
    Test.equal (Seq.contains (Queue [1;3;2]) permuts) true "permuts 3"
    Test.equal (Seq.contains (Queue [2;1;3]) permuts) true "permuts 4"
    Test.equal (Seq.contains (Queue [2;3;1]) permuts) true "permuts 5"
    Test.equal (Seq.contains (Queue [3;1;2]) permuts) true "permuts 6"
    Test.equal (Seq.contains (Queue [3;2;1]) permuts) true "permuts 7"

let permute =
    let xs = [1..10]
    let qs = Queue.ofList xs

    let rightShiftBy x = fun i -> (i+x) % 10

    Test.equal
        (Queue.permute (rightShiftBy 1) qs)
        (ValueSome (Queue.ofList (List.permute (rightShiftBy 1) xs)))
        "permute 1"

    Test.equal
        (Queue.permute (rightShiftBy 3) qs)
        (ValueSome (Queue.ofList (List.permute (rightShiftBy 3) xs)))
        "permute 2"

    Test.equal
        (Queue.permute (rightShiftBy 5) qs)
        (ValueSome (Queue.ofList (List.permute (rightShiftBy 5) xs)))
        "permute 3"

    Test.equal
        (Queue.permute (rightShiftBy 7) qs)
        (ValueSome (Queue.ofList (List.permute (rightShiftBy 7) xs)))
        "permute 4"

    Test.equal
        (Queue.permute (rightShiftBy 9) qs)
        (ValueSome (Queue.ofList (List.permute (rightShiftBy 9) xs)))
        "permute 5"

    Test.equal
        (Queue.permute (rightShiftBy 10) qs)
        (ValueSome (Queue.ofList (List.permute id xs)))
        "permute 6"

    Test.equal
        (Queue.permute (fun i -> i + 1) qs)
        ValueNone
        "permute 7"

Test.equal
    (Queue.reduceBack (+) (Queue ["A";"B";"C"]))
    (ValueSome "ABC")
    "reduceBack 1"

Test.equal (Queue.removeAt  0 (Queue.range 1  2)) (Queue.one 2)                         "removeAt 1"
Test.equal (Queue.removeAt 10 (Queue.range 1  5)) (Queue.range 1 5)                     "removeAt 2"
Test.equal (Queue.removeAt  5 (Queue.range 1 10)) (Queue.range 1 5 ++ Queue.range 7 10) "removeAt 3"
Test.equal (Queue.removeAt -3 (Queue.range 1  5)) (Queue.addMany [1..5] Queue.empty)    "removeAt 4"
Test.equal (Queue.removeAt 20 (Queue.range 1  5)) (Queue.range 1 5)                     "removeAt 5"

Test.equal (Queue.removeManyAt  0  3 (Queue.range 1 10)) (Queue.range 4 10)             "removeManyAt 1"
Test.equal (Queue.removeManyAt -2  3 (Queue.range 1 10)) (Queue.range 1 10)             "removeManyAt 2"
Test.equal (Queue.removeManyAt  0  0 (Queue.range 1 10)) (Queue.range 1 10)             "removeManyAt 3"
Test.equal (Queue.removeManyAt  2 -3 (Queue.range 1 10)) (Queue.range 1 10)             "removeManyAt 4"
Test.equal (Queue.removeManyAt  2  3 (Queue.range 1 10)) (Queue [1;2] ++ Queue [6..10]) "removeManyAt 5"

Test.equal
    (Queue.scanBack (+) (Queue ["A";"B";"C"]) "")
    (Queue [""; "C"; "BC"; "ABC"])
    "scanBack 1"

Test.equal
    (Queue.prependMany [1..5] (Queue.one 0))
    (Queue [5;4;3;2;1;0])
    "prependMany"

Test.equal
    (Queue.append (Queue [1;2]) (Queue.range 1 1000))
    (Queue.Queue ([1;2],(List.rev [1..1000]),1002))
    "Optimized append 1"

Test.equal
    (Queue.splitAt 4 (Queue.range 1 10))
    (Queue.slice 0 3 (Queue.range 1 10), Queue.slice 4 9 (Queue.range 1 10))
    "splitAt 1"

Test.equal
    (Queue.splitAt -1 (Queue.range 1 10))
    (Queue.empty, (Queue.range 1 10))
    "splitAt 2"

Test.equal
    (Queue.splitAt 20 (Queue.range 1 10))
    (Queue.range 1 10, Queue.empty)
    "splitAt 3"

Test.equal
    (Queue.splitInto 2 (Queue.range 1 10))
    (Queue [Queue.range 1 5; Queue.range 6 10])
    "splitInto 1"

Test.equal
    (Queue.splitInto 4 (Queue.range 1 10))
    (Queue [Queue [1;2;3] ; Queue [4;5;6] ; Queue [7;8] ; Queue [9;10]])
    "splitInto 2"

Test.equal
    (Queue.splitInto 10 (Queue.range 1 10))
    (Queue.map Queue.one (Queue.range 1 10))
    "splitInto 3"

Test.equal
    (Queue.splitInto 20 (Queue.range 1 10))
    (Queue.map Queue.one (Queue.range 1 10))
    "splitInto 4"

Test.equal
    (Queue.splitInto 0 (Queue.range 1 10))
    (Queue.empty)
    "splitInto 5"

Test.equal
    (Queue.splitInto -10 (Queue.range 1 10))
    Queue.empty
    "splitInto 6"

Test.equal
    (Queue.transpose [Queue [10;20;30]; Queue [11;21;31]])
    (Queue [Queue [10;11]; Queue [20;21]; Queue [30;31]])
    "transpose 1"

Test.equal
    (Queue.transpose [Queue [10;20;30;40]; Queue [11;21;31]])
    (Queue [Queue [10;11]; Queue [20;21]; Queue [30;31]])
    "transpose 2"

Test.equal
    (Queue.transpose [Queue [10;20;30]; Queue [11;21;31;41]])
    (Queue [Queue [10;11]; Queue [20;21]; Queue [30;31]])
    "transpose 3"

Test.equal
    (Queue.transpose [Queue []])
    Queue.empty
    "transpose 4"

Test.equal
    (Queue.transpose [Queue.one 10])
    (Queue.one (Queue.one 10))
    "transpose 5"

let unzip =
    let xs = Queue.range   1  10
    let ys = Queue.range 100 250
    let zs = Queue ['A' .. 'Z']
    let ws = Queue ["Hi"; "There"; "Wohoo"]

    Test.equal
        (Queue.unzip  (Queue.zip xs ys))
        (Queue.take 10 xs, Queue.take 10 ys)
        "unzip"

    Test.equal
        (Queue.unzip3 (Queue.zip3 xs ys zs))
        (Queue.take 10 xs, Queue.take 10 ys, Queue.take 10 zs)
        "unzip3"

    Test.equal
        (Queue.unzip4 (Queue.zip4 xs ys zs ws))
        (Queue.take 3 xs, Queue.take 3 ys, Queue.take 3 zs, Queue.take 3 ws)
        "unzip4"

Test.equal (Queue.windowed -1 (Queue.range 1 10)) (Queue.empty)                            "windowed 1"
Test.equal (Queue.windowed  0 (Queue.range 1 10)) (Queue.empty)                            "windowed 2"
Test.equal (Queue.windowed  1 (Queue.range 1 10)) (Queue.map Queue.one (Queue.range 1 10)) "windowed 3"
Test.equal
    (Queue.windowed  3 (Queue.range 1 10))
    (Queue [
        Queue [1;2;3]
        Queue [2;3;4]
        Queue [3;4;5]
        Queue [4;5;6]
        Queue [5;6;7]
        Queue [6;7;8]
        Queue [7;8;9]
        Queue [8;9;10]
    ])
    "windowed 4"

Test.equal
    (Queue.windowed 3 (Queue.range 1 10))
    (Queue (List.map Queue (List.windowed 3 [1..10])))
    "windowed 5"

Test.ok    (Queue.forall2 (=)     (Queue.range 1 3) (Queue.range 1 3))     "forall2 1"
Test.ok    (Queue.forall2 (=)     (Queue.range 1 3) (Queue.range 1 5))     "forall2 2"
Test.ok    (Queue.forall2 (=)     (Queue.empty)     (Queue.range 1 3))     "forall2 3"
Test.notOk (Queue.forall2 (=)     (Queue [1;2;3])   (Queue [1;2;4]))       "forall2 4"
Test.ok    (Queue.forall2 eqFloat (Queue.range 1 3) (Queue.range 1.0 3.0)) "forall2 5"

Test.equal
    (Queue.except (Queue [1;4;7;10]) (Queue.range 0 9))
    (Queue [0;2;3;5;6;8;9])
    "except"

Test.notOk (Queue.any2 (fun x y -> (x+y) % 2 = 0) (Queue.range 1 4) (Queue.range 2  5))   "any2 1"
Test.notOk (Queue.any2 (fun x y -> (x+y) % 2 = 0) (Queue.range 1 4) (Queue.range 2 10))   "any2 2"
Test.ok    (Queue.any2 (fun x y -> (x+y) % 2 = 0) (Queue.range 1 4) (Queue.range 1 4))    "any2 3"
Test.ok    (Queue.any2 (fun x y -> (x+y) % 2 = 0) (Queue.range 1 4) (Queue [2;3;4;4] ))   "any2 4"
Test.ok    (Queue.any2 (fun x y -> (x+y) % 2 = 0) (Queue.range 1 4) (Queue [2;3;4;4;20])) "any2 5"

Test.equal
    (Queue.findRemove (fun x -> x > 10) (Queue.range 1 20))
    (ValueSome (11, (Queue.append (Queue.range 1 10) (Queue.range 12 20))))
    "findRemove 1"

Test.equal
    (Queue.findRemove (fun x -> x > 100) (Queue.range 1 20))
    (ValueNone)
    "findRemove 2"

Test.equal
    (Queue.findRemove (fun x -> x < 10) (Queue.range 1 20))
    (ValueSome (1, (Queue.range 2 20)))
    "findRemove 3"

Test.equal
    (Queue.findRemoveBack (fun x -> x > 10) (Queue.range 1 20))
    (ValueSome (20, Queue.range 1 19))
    "findRemoveBack 1"

Test.equal
    (Queue.findRemoveBack (fun x -> x > 100) (Queue.range 1 20))
    (ValueNone)
    "findRemoveBack 2"

Test.equal
    (Queue.findRemoveBack (fun x -> x < 10) (Queue.range 1 20))
    (ValueSome (9, (Queue.append (Queue.range 1 8) (Queue.range 10 20))))
    "findRemoveBack 3"

Test.equal
    (Queue.sameLength (Queue.range 10 20) (Queue.range 1 5))
    (Queue.range 10 14, Queue.range 1 5)
    "sameLength 1"

Test.equal
    (Queue.sameLength (Queue.range 1 5) (Queue.range 10 20))
    (Queue.range 1 5, Queue.range 10 14)
    "sameLength 2"

Test.equal
    (Queue.sameLength (Queue.range 10 30) (Queue.range 10 30))
    (Queue.range 10 30, Queue.range 10 30)
    "sameLength 3"

Test.equal
    (Queue.sameLength Queue.empty (Queue.range 10 30))
    (Queue.empty, Queue.empty)
    "sameLength 4"

Test.equal
    (Queue.foldBack2 (fun a b xs -> (a+b) :: xs) (Queue [1..5]) (Queue.range 10 20) [])
    ([17;19;21;23;25])
    "foldBack2 1"

Test.equal
    (Queue.foldBack2 (fun a b xs -> (a+b) :: xs) (Queue.empty) (Queue.range 10 20) [])
    ([])
    "foldBack2 2"

let toSet =
    let set =
        Queue.concat (Queue [Queue.range 1 5; Queue.range 1 10; Queue.range 5 1; Queue [20;30]])
        |> Queue.toSet

    Test.equal (Set.count set) 12 "toSet 1"
    Test.ok (Set.contains  1 set) "toSet 2"
    Test.ok (Set.contains  2 set) "toSet 3"
    Test.ok (Set.contains  3 set) "toSet 4"
    Test.ok (Set.contains  4 set) "toSet 5"
    Test.ok (Set.contains  5 set) "toSet 6"
    Test.ok (Set.contains  6 set) "toSet 7"
    Test.ok (Set.contains  7 set) "toSet 8"
    Test.ok (Set.contains  8 set) "toSet 9"
    Test.ok (Set.contains  9 set) "toSet 10"
    Test.ok (Set.contains 10 set) "toSet 11"
    Test.ok (Set.contains 20 set) "toSet 12"
    Test.ok (Set.contains 30 set) "toSet 13"

let toMap =
    let queue = Queue ["Hallo"; "Welt"; "Du"; "Gehst"; "Unter"]

    let m1 = Queue.toMap (fun x -> x, String.length x) queue
    Test.equal (Map.count m1)            5       "toMap 1"
    Test.equal (Map.tryFind "Hallo" m1) (Some 5) "toMap 2"
    Test.equal (Map.tryFind "Welt"  m1) (Some 4) "toMap 3"
    Test.equal (Map.tryFind "Du"    m1) (Some 2) "toMap 4"
    Test.equal (Map.tryFind "Gehst" m1) (Some 5) "toMap 5"
    Test.equal (Map.tryFind "Unter" m1) (Some 5) "toMap 6"

    let m2 = Queue.toMap (fun x -> String.length x, x) queue
    Test.equal (Map.count m2)      3             "toMap 7"
    Test.equal (Map.tryFind 2 m2) (Some "Du")    "toMap 8"
    Test.equal (Map.tryFind 4 m2) (Some "Welt")  "toMap 9"
    Test.equal (Map.tryFind 5 m2) (Some "Unter") "toMap 10"

    let m3 = Queue.toMapWithFold String.length (fun s x -> x :: s) [] queue
    Test.equal (Map.count m3)      3              "toMap 11"
    Test.equal (Map.tryFind 2 m3) (Some ["Du"])   "toMap 12"
    Test.equal (Map.tryFind 4 m3) (Some ["Welt"]) "toMap 13"
    Test.equal (Map.tryFind 5 m3) (Some ["Unter";"Gehst";"Hallo"]) "toMap 14"

    Test.equal
        (Queue.toMapGroupBy String.length queue)
        (Map (Queue.groupBy String.length queue))
        "toMap 15"

Test.equal
    (Queue (Set [10;9;5;1;2;3;4;8;7;6]))
    (Queue.range 1 10)
    "Queue constructor from Set"

Test.equal
    (Queue (Array.create 10 0))
    (Queue.repeat 10 0)
    "Queue constructor from Array"

Test.equal
    (Queue [1..10])
    (Queue.range 1 10)
    "Queue constructor from List"

Test.equal
    (Queue (seq {1..10}))
    (Queue.range 1 10)
    "Queue constructor from Seq"

let ofMap =
    let q = Queue.ofMap (Map [(1,"Hallo"); (2,"Trouble"); (10,"Rumble")])

    let search x from = fst from = x
    Test.equal (Queue.length q) 3 "ofMap Length"
    Test.equal (Queue.find (search  1) q) (ValueSome ( 1,"Hallo"))   "ofMap 1"
    Test.equal (Queue.find (search  2) q) (ValueSome ( 2,"Trouble")) "ofMap 2"
    Test.equal (Queue.find (search 10) q) (ValueSome (10,"Rumble"))  "ofMap 3"

let toQueuefromMap =
    let q = Queue (Map [(1,"Hallo"); (2,"Trouble"); (10,"Rumble")])

    let kv k v = System.Collections.Generic.KeyValuePair(k,v)
    let search x (KeyValue (key,_)) = key = x

    Test.equal (Queue.length q) 3 "Queue from Map Length"
    Test.equal (Queue.find (search  1) q) (ValueSome (kv  1 "Hallo"))   "Queue from Map 1"
    Test.equal (Queue.find (search  2) q) (ValueSome (kv  2 "Trouble")) "Queue from Map 2"
    Test.equal (Queue.find (search 10) q) (ValueSome (kv 10 "Rumble"))  "Queue from Map 3"

Test.equal (Queue.itemMany [-1;0;3;5;50] (Queue.range 1 10)) (Queue [1;4;6]) "itemMany 1"
Test.equal (Queue.itemMany []            (Queue.range 1 10)) (Queue.empty)   "itemMany 2"
Test.equal (Queue.itemMany [20]          (Queue.range 1 10)) (Queue.empty)   "itemMany 3"
Test.equal (Queue.itemMany [9]           (Queue.range 1 10)) (Queue [10])    "itemMany 4"
Test.equal (Queue.itemMany [0;9]         (Queue.range 1 10)) (Queue [1;10])  "itemMany 5"
Test.equal (Queue.itemMany [7;5;2]       (Queue.range 1 10)) (Queue [8;6;3]) "itemMany 6"

Test.equal
    (Queue.choosei (fun i x -> if isEven i && String.length x > 0 then ValueSome (i,x) else ValueNone)
        //      0    1  2   3       4       5      6     7
        (Queue ["Hi";"";"";"There";"Whuba";"Luba";"Dub";"Dub"]))
    (Queue [(0,"Hi"); (4,"Whuba"); (6,"Dub")])
    "choosei"

Test.equal
    (Queue.partitionMap
        (fun x ->
            if   x % 3 = 0 && x % 5 = 0 then Choice2Of2 "FizzBuzz"
            elif x % 3 = 0              then Choice2Of2 "Fizz"
            elif x % 5 = 0              then Choice2Of2 "Buzz"
            else Choice1Of2 (string x)) (Queue.range 1 15))
    (Queue ["1";"2";"4";"7";"8";"11";"13";"14"], Queue ["Fizz";"Buzz";"Fizz";"Fizz";"Buzz";"Fizz";"FizzBuzz"])
    "partitionMap"

Test.equal
    (Queue.toList2 (Queue [Queue.range 1 5; Queue.range 1 3; Queue.range 6 10]))
    ([[1..5]; [1..3]; [6..10]])
    "toList2"

Test.equal
    (Queue.ofList2 [[1..5]; [1..3]; [6..10]])
    (Queue.empty
        |> Queue.add (Queue.range 1 5)
        |> Queue.add (Queue.range 1 3)
        |> Queue.add (Queue.range 6 10))
    "ofList2"

// Run Tests
let args = Array.skip 1 <| System.Environment.GetCommandLineArgs()
runTestsWithCLIArgs [] args (testList "Main" (List.ofSeq tests)) |> ignore