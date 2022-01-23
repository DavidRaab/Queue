# F#: Immutable Queue

A **Queue** is a *first-in-first-out* data-structure. It provides fast appending O(1) to the end of a **Queue** but also
fast prepeding O(1) to a **Queue**.

The immutable **List** provided by F# is a *first-in-last-out* data-structure or a **Stack** and only provides fast prepeding O(1).

This **Queue** implements all the functions you know from the **List** module with a few more and some changes. **Queue** is designed as a replacement for **List**, but not as a drop-in replacement.

# Design Philosophy / Why you want to use Queue

The Design Philosophy of `Queue` opposed to `List` is:

1. To never throw an Exception.
2. Use `ValueOption` instead of `Option`.

To achieve the first goal some functions are changed to return a `ValueOption`, provide reasonable results like
returning an empty **Queue** or provide input checking for arguments.

List already provides some of these. For example you have `List.find` and `List.tryFind`. In this module there
is only `Queue.find` and it will return a `ValueOption` by default. There is no *find* version that will throw
an exception.

But the `List` module still have a lot of functions that throw exceptions in various ways with
no equivalent *try* function.

```fsharp
// Throws ArgumentException
let bs = List.map2 (fun x y -> x + y) [1..10] [1..8]

// Queue [2;4;6;8;10;12;14;16]
let cs = Queue.map2 (fun x y -> x + y) (Queue.range 1 10) (Queue.range 1 8)

// Throws ArgumentException
let ds = List.tail []

// Queue.empty
let es = Queue.tail Queue.empty

// Throws InvalidOperationException
let fs = List.take 5 [1;2;3]

// Queue [1;2;3]
let gs = Queue.take 5 (Queue [1;2;3])
```

Functions are either changed to return something useful like `Queue.map2` that only iterates
on so many elements that are possible on both `Queue`s . Or if not possible, are changed to
return an `ValueOption` instead.

```fsharp
// Throws an ArgumentException
let xs = List.reduce (fun x y -> x + y) []

// Returns: ValueNone
let ys = Queue.reduce (fun x y -> x + y) Queue.empty

// Returns: (ValueSome 55)
let zs = Queue.reduce (fun x y -> x + y) (Queue.range 1 10)
```

As an overview. There are still these `List` functions that can throw exceptions and have no *try* equivalent you must consider when you use a `List`: `average`, `averageBy`, `chunkBySize`, `exactlyOne`, `except`, `exists2`, `forall2`,
`head`, `init`, `insertAt`, `insertManyAt`, `item`, `last`, `max`, `maxBy`, `min`,
`minBy`, `permute`, `reduce`, `reduceBack`, `removeAt`, `removeManyAt`, `skip`, `splitAt`, `splitInto`, `tail`,
`take`, `transpose`, `updateAt`, `windowed`. None of those throw an exception in the `Queue` implementation!

On top, there are additional functions you don't find in other modules. To name the most important.

* `Queue.map3`, `Queue.map4`
* `Queue.lift2`, `Queue.lift3`, `Queue.lift4`
* `Queue.fold3`, `Queue.foldi`, `Queue.foldi2`, `Queue.foldi3`
* `Queue.mapFold` (it works different to `List.mapFold`)
* `Queue.mapReduce`
* `Queue.mapFilter`
* `Queue.filterMap`
* `Queue.toSet`
* `Queue.toMap`
* `Queue.toMapWithFold`
* `Queue.sliceGrow`, `Queue.insertAtGrow`, `Queue.updateAtGrow`, `Queue.insertManyAtGrow`
* `Queue.findRemove`
* `Queue.intersperse`
* `Queue.permutations`

Because of the implementation of the Queue some operations have a better performance.

* Adding `Queue.add` and prepending `Queue.prepend` are both O(1)
* `Queue.rev` is O(1)
* `Queue.length` is O(1)
* `Queue.append` is O(N) of the smaller list.

`Queue` also implements Comparision, Equality and IEnumerable (Seq) so you can compare two `Queue`s, traverse it with the `for` keyword or pass it wherever a `seq` is needed.

You can also transform every sequence to a Queue by just writing `Queue` in front of it. Like you are used with `Map` or `Set`.

```fsharp
let bs = Queue [1..10]
let cs = Queue [|1..10|]
let ds = Queue (seq {1..10})

// Queue<KeyValuePair<int,string>>
let m1 = Queue       (Map [(1,"Hi"); (10,"There")]

// Queue<int * string>
let m2 = Queue.ofMap (Map [(1,"Hi"); (10,"There")]
```

For usage, currently, it is best to just look into the test file in [../test/Program.fs](https://github.com/DavidRaab/Queue/blob/master/test/Program.fs)

# Implementation Details

The internal data-structure is quite easy to understand. It keeps track of three values. Two immutable lists and the amount of elements in the Queue.

Added items to the Queue are added to a List named `Added`. This way we can provide O(1) for adding elements.

When you traverse the Queue it will traverse the internal `Queue` list that has the items in Queue-order. If that is empty, it will reverse the `Added` list once and store the result into `Queue`. This way traversing is amortized O(1). Meaning: Most operations are O(1) and some are O(N).

So if you do the following:

```fsharp
let bs = Queue.range 1 10
let cs = Queue.add 11 bs
let ds = Queue.skip 3 cs
let es = ds |> Queue.add 12 |> Queue.add 13 |> Queue.add 14
let fs = Queue.prepend 3 es
let gs = Queue.skip 3 fs
```

The internal data-structure would look like these:

```fsharp
let bs = Queue([], [10;9;8;7;6;5;4;3;2;1], 10)
let cs = Queue([], [11;10;9;8;7;6;5;4;3;2;1], 11)
let ds = Queue([4;5;6;7;8;9;10;11], [], 8)
let es = Queue([4;5;6;7;8;9;10;11], [14;13;12], 11)
let fs = Queue([3;4;5;6;7;8;9;10;11], [14;13;12], 12)
let gs = Queue([6;7;8;9;10;11], [14;13;12], 9)
```
