# F#: Immutable Queue

A **Queue** is a *first-in-first-out* data-structure. It is fast O(1) to add things at the end of a **Queue**.
The immutable **List** provided by F# is a *first-in-last-out* data-structure or a **Stack**.

This **Queue** implements all the functions you know from the **List** module with a few more and some changes.

# Design Philosophy / Why you want to Use

The Design Philosophy of `Queue` opposed to `List` is:

1. To never throw an Exception.
2. Use `ValueOption` instead of `Option`.

To achieve the first goal some functions are changed to return a `ValueOption`. In `List` for
example you have `List.find` and `List.tryFind`. In this module there is only `Queue.find`
and it will return an `ValueOption` by default. There is no *find* version that will throw an Exception.

But the List module still have a lot of functions that throw exceptions in various ways with
no equivalent *try* function.

```fsharp
// Throws an ArgumentException
let xs = List.map2 (fun x y -> x + y) [1..10] [1..8] 

// Returns: Queue [2;4;6;8;10;12;14;16]
let ys = Queue.map2 (fun x y -> x + y) (Queue.range 1 10) (Queue.range 1 8)
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
* `Queue.sliceGrow`, `Queue.insertAtGrow`, ...
* `Queue.findRemove`
* `Queue.intersperse`
* `Queue.permutations`

Because of the implementation of the Queue some Operations have a better performance.

* Adding `Queue.add` and Prepending `Queue.prepend` are both O(1)
* `Queue.rev` is O(1)
* `Queue.length` is O(1)
* `Queue.append` is O(N) of the smaller list.

`Queue` also implements Comparision, Equality and IEnumerable (Seq) so you compare two `Queue`s, traverse it with the `for` keyword or use it wherever you can
pass a sequence.

You can also transform every sequence to a Queue by just writing `Queue` in front of it. Like you are used with `Map` or `Set`.

```fsharp
let xs = Queue [1..10]
let ys = Queue [|1..10|]
let zs = Queue (seq {1..10})
```

For usage, currently it is best to just look into the test file in [../test/Program.fs](https://github.com/DavidRaab/Queue/blob/master/test/Program.fs)

# Implementation Details

The internal data-structure is quite easy to understand. It keeps track of three values. Two immutable lists and the amount of elememnts in the Queue.

Added items to the Queue are added to a List named `Added`. This way we can provide O(1) for Adding elements.

When you traverse the Queue it will traverse the internal `Queue` List that has the items in Queue-order. If that is empty, it will reverse the `Added` list
and stores the result into `Queue`. This way traversing is Amortized O(1). Meaning: Most operations are O(1) and some are O(N).

So if you do the following:

```fsharp
let as = Queue.range 1 10
let bs = Queue.add 11
let cs = Queue.skip 3
let ds = Queue.add 12
let es = Queue.prepend 3
```

The internal data-structure would look like these:

```fsharp
let as = Queue([], [10;9;8;7;6;5;4;3;2;1], 10)
let bs = Queue([], [11;10;9;8;7;6;5;4;3;2;1], 11)
let cs = Queue([4;5;6;7;8;9;10;11], [], 8)
let ds = Queue([4;5;6;7;8;9;10;11], [12], 9)
let es = Queue([3;4;5;6;7;8;9;10;11], [12], 10)
```