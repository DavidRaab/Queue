namespace Queue

type Queue<[<EqualityConditionalOn; ComparisonConditionalOn>]'a>(q,a,length) =
    static member Empty : Queue<'a> = Queue([],[],0)

    new(sequence:seq<'a>) =
        match sequence with
        | :? array<'a> as xs -> Queue(Array.toList xs, [], Array.length xs)
        | :? list<'a>  as xs -> Queue(xs, [], List.length xs)
        | _                  ->
            let mutable amount = 0
            let added = Seq.fold (fun xs x -> amount <- amount + 1; x :: xs) [] sequence
            Queue([],added,amount)

    member _.Queue : list<'a> = q
    member _.Added : list<'a> = a
    member _.Length = length

    member _.Head () : voption<'a * Queue<'a>> =
        if length > 1 then
            match q,a with
                | x::xs, a -> ValueSome (x, Queue(xs,a,(length-1)))
                |    [], a ->
                    let newQ = List.rev a
                    ValueSome (List.head newQ, Queue((List.tail newQ),[],(length-1)))
        elif length = 1 then
            if   List.isEmpty q
            then ValueSome (List.head a, Queue([],[],0))
            else ValueSome (List.head q, Queue([],[],0))
        else
            ValueNone

    member this.Add (x:'a) : Queue<'a> =
        Queue(this.Queue, x :: this.Added, this.Length + 1)

    override this.Equals (obj:obj) : bool =
        match obj with
        | :? Queue<'a> as other ->
            let rec loop (queue1:Queue<'a>) (queue2:Queue<'a>) =
                match queue1.Head(), queue2.Head() with
                | ValueSome (x,q1), ValueSome (y,q2) -> if Unchecked.equals x y then loop q1 q2 else false
                | ValueNone,        ValueNone        -> true
                | _                                  -> failwith "Not Possible"
            if   this.Length = other.Length
            then loop this other
            else false
        | _ ->
            false

    override this.GetHashCode () : int = Unchecked.hash this

    member private this.getEnumerator () =
        let mutable queue   = this
        let mutable current = Unchecked.defaultof<_>
        { new System.Collections.Generic.IEnumerator<'a> with
            member _.Current with get () : 'a  = current
            member _.Current with get () : obj = box current
            member _.MoveNext () =
                match queue.Head() with
                | ValueNone ->
                    current <- Unchecked.defaultof<_>
                    false
                | ValueSome (x,t) ->
                    queue   <- t
                    current <- x
                    true
            member _.Reset() =
                queue   <- this
                current <- Unchecked.defaultof<_>
            member _.Dispose () = ()
        }

    interface System.Collections.Generic.IEnumerable<'a> with
        override this.GetEnumerator(): System.Collections.Generic.IEnumerator<'a> =
            this.getEnumerator ()

        override this.GetEnumerator(): System.Collections.IEnumerator =
            this.getEnumerator ()

    interface System.IComparable with
        override this.CompareTo(other:obj) =
            match other with
            | :? Queue<'a> as other ->
                let rec loop (queue1:Queue<'a>) (queue2:Queue<'a>) =
                    match queue1.Head (), queue2.Head () with
                    | ValueSome (x,q1), ValueSome (y,q2) ->
                        match Unchecked.compare x y with
                        | 0 -> loop q1 q2
                        | x -> x
                    | ValueNone  , ValueNone   ->  0
                    | ValueSome _, ValueNone   ->  1
                    | ValueNone  , ValueSome _ -> -1
                loop this other
            | _ -> failwith "Comparison of two different types"

module Queue =
    // Creation of Queue
    let empty<'a> : Queue<'a> = Queue.Empty

    let add x (queue: Queue<'a>) =
        queue.Add x

    let addMany (xs: seq<'a>) (queue: Queue<'a>) =
        Seq.fold (fun q x -> add x q) queue xs

    let prepend (x: 'a) (queue: Queue<'a>) =
        Queue(x :: queue.Queue, queue.Added, queue.Length + 1)

    let prependMany (xs: seq<'a>) (queue: Queue<'a>) =
        Seq.fold (fun q x -> prepend x q) queue xs

    let one (x: 'a) : Queue<'a> =
        Queue ([],[x],1)

    let unfold generator (state:'State) =
        let rec loop q state =
            match generator state with
            | ValueSome (x,state) -> loop (add x q) state
            | ValueNone           -> q
        loop empty state

    let repeat count x =
        unfold (fun counter ->
            if   counter < count
            then ValueSome (x,counter+1)
            else ValueNone
        ) 0

    let inline range start stop =
        if start = stop then
            one start
        elif start < stop then
            start |> unfold (fun x ->
                if   x <= stop
                then ValueSome (x, x+LanguagePrimitives.GenericOne)
                else ValueNone
            )
        else
            start |> unfold (fun x ->
                if   x >= stop
                then ValueSome (x, x-LanguagePrimitives.GenericOne)
                else ValueNone
            )

    let inline rangeWithStep start step stop =
        if start = stop then
            one start
        elif start < stop && step > LanguagePrimitives.GenericZero then
            start |> unfold (fun x ->
                if   x <= stop
                then ValueSome (x, x+step)
                else ValueNone
            )
        elif stop < start && step < LanguagePrimitives.GenericZero then
            start |> unfold (fun x ->
                if   x >= stop
                then ValueSome (x, x+step)
                else ValueNone
            )
        else
            empty

    let init length generator =
        let gen idx =
            if   idx < length
            then ValueSome (generator idx, idx+1)
            else ValueNone
        if   length > 0
        then unfold gen 0
        else empty

    // Low-Level & Basic Implementations
    let isEmpty (queue:Queue<'a>) =
        queue.Length = 0

    let length (q:Queue<'a>) =
        q.Length

    let head (q:Queue<'a>) =
        q.Head()

    let equal (q1:Queue<'a>) (q2:Queue<'a>) =
        q1.Equals(q2)

    let rev (queue : Queue<'a>)=
        Queue(queue.Added, queue.Queue, queue.Length)

    let tail queue =
        match head queue with
        | ValueSome (h,t) -> t
        | ValueNone       -> Queue([],[],0)

    let fold f (state:'State) (queue : Queue<'a>) =
        let rec loop first remaining state =
            match first with
            | []    ->
                if   List.isEmpty remaining
                then state
                else loop remaining [] state
            | x::xs -> loop xs remaining (f state x)
        loop queue.Queue (List.rev queue.Added) state

    let fold2 f (state:'State) queue1 queue2 =
        let rec loop state q1 q2 =
            match head q1, head q2 with
            | ValueSome (x1,q1), ValueSome (x2,q2) -> loop (f state x1 x2) q1 q2
            | _                                    -> state
        loop state queue1 queue2

    let fold3 f (state: 'State) queue1 queue2 queue3 =
        let rec loop state q1 q2 q3 =
            match head q1, head q2, head q3 with
            | ValueSome (x1, q1), ValueSome (x2, q2), ValueSome (x3, q3) -> loop (f state x1 x2 x3) q1 q2 q3
            | _ -> state
        loop state queue1 queue2 queue3

    let scan f (state:'State) queue =
        let folder (state,states) x =
            let newState = f state x
            (newState, add newState states)
        snd (fold folder (state,one state) queue)

    let foldBack f queue (state:'State) =
        let rec loop queue state =
            match head queue with
            | ValueSome (x,queue) -> loop queue (f x state)
            | ValueNone           -> state
        loop (rev queue) state

    let foldBack2 f queue1 queue2 (state:'State) =
        let rec loop q1 q2 state =
            match head q1, head q2 with
            | ValueSome (x,q1), ValueSome (y,q2) -> loop q1 q2 (f x y state)
            | ValueNone       , ValueNone        -> state
            | ValueSome _     , ValueNone        -> state
            | ValueNone       , ValueSome _      -> state
        loop (rev queue1) (rev queue2) state

    let scanBack f queue (state:'State) =
        let folder x (state,states) =
            let newState = f x state
            (newState, add newState states)
        snd (foldBack folder queue (state,one state))

    let apply fq xq =
        let rec loop fq xq state =
            match head fq, head xq with
            | ValueSome (f,fq), ValueSome (x,xq) -> loop fq xq (add (f x) state)
            | ValueNone       , ValueNone        -> state
            | ValueNone       , ValueSome _      -> state
            | ValueSome _     , ValueNone        -> state
        loop fq xq empty

    let bind (f : 'a -> Queue<'b>) queue =
        fold (fun state x -> addMany (f x) state) empty queue

    let map f queue =
        let folder q x =
            add (f x) q
        fold folder empty queue


    // Side-Effects
    let rec iter f queue =
        match head queue with
        | ValueSome (h,t) -> f h; iter f t
        | ValueNone       -> ()

    let rec iter2 f queue1 queue2 =
        match head queue1, head queue2 with
        | ValueSome (x1,q1), ValueSome(x2,q2) -> f x1 x2; iter2 f q1 q2
        | _                                   -> ()

    let iteri f queue =
        let rec loop idx queue =
            match head queue with
            | ValueSome (x,t) -> f idx x; loop (idx+1) t
            | ValueNone       -> ()
        loop 0 queue

    let iteri2 f queue1 queue2 =
        let rec loop idx q1 q2 =
            match head q1, head q2 with
            | ValueSome (x1,q1), ValueSome (x2,q2) -> f idx x1 x2; loop (idx+1) q1 q2
            | ValueSome _, ValueNone   -> ()
            | ValueNone  , ValueSome _ -> ()
            | ValueNone  , ValueNone   -> ()
        loop 0 queue1 queue2


    // Utilities
    let lastIndex queue =
        length queue - 1

    let append (queue1:Queue<'a>) (queue2:Queue<'a>) =
        if   queue1.Length < queue2.Length
        then prependMany (rev queue1) queue2
        else addMany      queue2      queue1

    let last (queue : Queue<'a>) =
        match queue.Queue, queue.Added with
        | [],[]      -> ValueNone
        | r ,[]      -> ValueSome (List.last r)
        | _ ,last::a -> ValueSome last

    let mapReduce mapper reducer queue =
        let rec loop state queue =
            match head queue with
            | ValueSome (x,queue) -> loop (reducer state (mapper x)) queue
            | ValueNone           -> ValueSome state
        match head queue with
        | ValueNone           -> ValueNone
        | ValueSome (x,queue) -> loop (mapper x) queue

    let mapFold mapper folder (state:'State) queue =
        let rec loop state queue =
            match head queue with
            | ValueSome (x,queue) -> loop (folder state (mapper x)) queue
            | ValueNone           -> state
        loop state queue

    let max queue =
        let folder state x =
            match state with
            | ValueSome y -> ValueSome (max x y)
            | ValueNone   -> ValueSome x
        fold folder ValueNone queue

    let maxBy (projection: 'a -> 'Key) queue =
        let folder state item =
            match state with
            | ValueSome (max,_) as orig ->
                let itemMax = projection item
                if itemMax > max then ValueSome (itemMax,item) else orig
            | ValueNone ->
                ValueSome (projection item, item)
        ValueOption.map snd (fold folder ValueNone queue)

    let min queue =
        let folder state x =
            match state with
            | ValueSome y -> ValueSome (min x y)
            | ValueNone   -> ValueSome x
        fold folder ValueNone queue

    let minBy (projection: 'a -> 'Key) queue =
        let folder state item =
            match state with
            | ValueSome (min,_) as orig ->
                let itemMin = projection item
                if itemMin < min then ValueSome (itemMin,item) else orig
            | ValueNone ->
                ValueSome (projection item, item)
        ValueOption.map snd (fold folder ValueNone queue)

    let foldi f (state:'State) queue =
        fold (fun (idx,state) x ->
            (idx+1, f idx state x)
        ) (0,state) queue
        |> snd

    let foldi2 f (state:'State) queue1 queue2 =
        fold2 (fun (idx,state) x1 x2 ->
            (idx+1, f idx state x1 x2)
        ) (0,state) queue1 queue2
        |> snd

    let foldi3 f (state:'State) queue1 queue2 queue3 =
        fold3 (fun (idx,state) x1 x2 x3 ->
            (idx+1, f idx state x1 x2 x3)
        ) (0,state) queue1 queue2 queue3
        |> snd

    let mapFilter mapper predicate queue =
        fold (fun q x ->
            let x = mapper x
            if predicate x then add x q else q
        ) empty queue

    let filterMap predicate mapper queue =
        fold (fun q x ->
            if predicate x then add (mapper x) q else q
        ) empty queue

    let concat queues = bind id queues

    let map2 f queue1 queue2 =
        let rec loop newQ q1 q2 =
            match head q1, head q2 with
            | ValueSome (x1,q1), ValueSome (x2,q2) ->
                loop (add (f x1 x2) newQ) q1 q2
            | _ -> newQ
        loop empty queue1 queue2

    let map3 f queue1 queue2 queue3 =
        let rec loop newQ q1 q2 q3 =
            match head q1, head q2, head q3 with
            | ValueSome (x1,q1), ValueSome (x2,q2), ValueSome (x3,q3) ->
                loop (add (f x1 x2 x3) newQ) q1 q2 q3
            | _ -> newQ
        loop empty queue1 queue2 queue3

    let map4 f queue1 queue2 queue3 queue4 =
        let rec loop newQ q1 q2 q3 q4 =
            match head q1, head q2, head q3, head q4 with
            | ValueSome (x1,q1), ValueSome (x2,q2), ValueSome (x3,q3), ValueSome (x4,q4) ->
                loop (add (f x1 x2 x3 x4) newQ) q1 q2 q3 q4
            | _ -> newQ
        loop empty queue1 queue2 queue3 queue4

    let mapi f queue =
        let folder (idx,q) x =
            idx+1, add (f idx x) q
        snd (fold folder (0,empty) queue)

    let mapi2 f queue1 queue2 =
        let folder (idx,q) x y =
            idx+1, add (f idx x y) q
        snd (fold2 folder (0,empty) queue1 queue2)

    let lift2 f queue1 queue2 =
        queue1 |> bind (fun x1 ->
        queue2 |> bind (fun x2 ->
            one (f x1 x2)))

    let lift3 f queue1 queue2 queue3 =
        lift2 f queue1 queue2 |> bind (fun f ->
        queue3 |> bind (fun x ->
            one (f x)))

    let lift4 f queue1 queue2 queue3 queue4 =
        lift3 f queue1 queue2 queue3 |> bind (fun f ->
        queue4 |> bind (fun x ->
            one (f x)))

    let partition predicate queue =
        fold (fun (ts,fs) x ->
            if   predicate x
            then add x ts,       fs
            else       ts, add x fs
        ) (empty,empty) queue

    let compare (queue1:Queue<'a>) (queue2:Queue<'a>) =
        LanguagePrimitives.GenericComparison queue1 queue2

    let compareWith comparer queue1 queue2 =
        let rec loop queue1 queue2 =
            match head queue1, head queue2 with
            | ValueSome (x,q1), ValueSome (y,q2) ->
                match comparer x y with
                | 0 -> loop q1 q2
                | x -> x
            | ValueSome _, ValueNone   ->  1
            | ValueNone  , ValueSome _ -> -1
            | ValueNone  , ValueNone   ->  0
        loop queue1 queue2

    let allPairs queue1 queue2 =
        lift2 (fun x y -> (x,y)) queue1 queue2

    let chunkBySize size queue =
        let rec loop amount queue inner outer =
            if amount = size then
                loop 0 queue empty (add inner outer)
            else
                match head queue with
                | ValueSome (x,t) -> loop (amount+1) t (add x inner) outer
                | ValueNone       -> add inner outer
        if   size < 0 || isEmpty queue
        then empty
        else loop 0 queue empty empty

    let item idx queue =
        let rec loop i queue =
            match head queue with
            | ValueSome (x,t) ->
                if   i = idx
                then ValueSome x
                else loop (i+1) t
            | ValueNone -> ValueNone
        loop 0 queue

    let indexed queue =
        mapi (fun i x -> (i,x)) queue

    let filter predicate q =
        let folder q x =
            if predicate x then add x q else q
        fold folder empty q

    let choose f queue =
        let folder q x =
            match f x with
            | ValueSome x -> add x q
            | ValueNone   -> q
        fold folder empty queue

    let contains x (queue:Queue<'a>) =
        List.contains x queue.Queue || List.contains x queue.Added

    let except itemsToExclude queue =
        fold (fun q x ->
            if   contains x itemsToExclude
            then q
            else add x q
        ) empty queue

    let take amount q =
        let gen (amount,q) =
            if amount > 0 then
                match head q with
                | ValueSome (h,t) -> ValueSome (h, ((amount-1),t))
                | ValueNone       -> ValueNone
            else
                ValueNone
        if   amount <= 0
        then empty
        else unfold gen (amount,q)

    let skip amount queue =
        let rec loop amount q =
            if amount > 0 then
                match head q with
                | ValueSome (_,t) -> loop (amount-1) t
                | ValueNone       -> empty
            else
                q
        if   amount <= 0
        then queue
        else loop amount queue

    let takeWhile predicate queue =
        let rec loop queue newQ =
            match head queue with
            | ValueSome (x,t) ->
                if   predicate x
                then loop t (add x newQ)
                else newQ
            | ValueNone -> newQ
        loop queue empty

    let skipWhile predicate queue =
        let rec loop queue =
            match head queue with
            | ValueSome (x,t) ->
                if   predicate x
                then loop t
                else queue
            | ValueNone -> empty
        loop queue

    let sameLength (queue1:Queue<'a>) (queue2:Queue<'b>) =
        if   queue1.Length = queue2.Length then (queue1, queue2)
        elif queue1.Length > queue2.Length then (take queue2.Length queue1, queue2)
        else                                    (queue1, take queue1.Length queue2)

    let distinct queue =
        let seen = System.Collections.Generic.Dictionary()
        let mutable value = false
        let rec loop newQ queue =
            match head queue with
            | ValueSome(x,t) ->
                value <- false
                match seen.TryGetValue(x, &value) with
                | true  ->                   loop newQ         t
                | false -> seen.Add(x,true); loop (add x newQ) t
            | ValueNone -> newQ
        loop empty queue

    let distinctBy (projection: 'a -> 'Key) queue =
        let seen = System.Collections.Generic.Dictionary()
        let mutable value = false
        let rec loop newQ queue =
            match head queue with
            | ValueSome(x,t) ->
                let key = projection x
                value <- false
                match seen.TryGetValue(key, &value) with
                | true  ->                     loop newQ         t
                | false -> seen.Add(key,true); loop (add x newQ) t
            | ValueNone -> newQ
        loop empty queue

    let slice start stop queue =
        let lidx  = lastIndex queue
        let start = if start < 0   then 0    else start
        let stop  = if stop > lidx then lidx else stop

        if   start > stop
        then empty
        else take (stop-start+1) (skip start queue)

    let sliceGrow zeroElement start stop queue =
        let start = if start < 0 then 0 else start
        let lidx  = lastIndex queue

        if   stop <  start then empty
        elif stop <= lidx  then slice start stop queue
        else
            let s = slice start lidx queue
            let r = repeat ((stop-start+1)-(length s)) zeroElement
            append s r

    let insertAt index value queue =
        let rec loop index queue newQ =
            match index, head queue with
            | 0,   ValueNone       -> add value newQ
            | 0,   ValueSome (x,t) -> loop -1 t (add x (add value newQ))
            | idx, ValueNone       -> newQ
            | idx, ValueSome (x,t) -> loop (idx-1) t (add x newQ)
        if   (index-1) = lastIndex queue          then add value queue
        elif index < 0 || index > lastIndex queue then queue
        else loop index queue empty

    let insertAtGrow zeroElement index value queue =
        let rec loop index queue newQ =
            match index, head queue with
            | (idx,ValueNone) when idx < 0 -> newQ
            | 0,   ValueNone               -> add value newQ
            | 0,   ValueSome (x,t)         -> loop -1      t           (add x           (add value newQ))
            | idx, ValueNone               -> loop (idx-1) empty (add zeroElement newQ)
            | idx, ValueSome (x,t)         -> loop (idx-1) t           (add x           newQ)
        if   index < 0
        then queue
        else loop index queue empty

    let updateAt index value queue =
        let folder idx q x =
            if   index = idx
            then add value q
            else add x     q
        if   index < 0 || index >= (length queue - 1)
        then queue
        else foldi folder empty queue

    let updateAtGrow zeroElement index value queue =
        let rec loop index queue newQ =
            match index, head queue with
            | 0,     ValueNone                -> add value newQ
            | 0,     ValueSome (x,t)          -> loop -1 t (add value newQ)
            | index, ValueNone when index < 0 -> newQ
            | index, ValueNone                -> loop (index-1) empty (add zeroElement newQ)
            | index, ValueSome (x,t)          -> loop (index-1) t           (add x newQ)
        if   index < 0
        then queue
        else loop index queue empty

    let insertManyAt index values queue =
        let rec loop index queue newQ =
            match index, head queue with
            | 0,   ValueNone       -> addMany values newQ
            | 0,   ValueSome (x,t) -> loop -1 t (add x (addMany values newQ))
            | idx, ValueNone       -> newQ
            | idx, ValueSome (x,t) -> loop (idx-1) t (add x newQ)
        if   index < 0
        then queue
        else loop index queue empty

    let insertManyAtGrow zeroElement index values queue =
        let rec loop index queue newQ =
            match index, head queue with
            | (idx,ValueNone) when idx < 0 -> newQ
            | 0,   ValueNone               -> addMany values newQ
            | 0,   ValueSome (x,t)         -> loop -1      t           (add x (addMany values newQ))
            | idx, ValueNone               -> loop (idx-1) empty (add zeroElement newQ)
            | idx, ValueSome (x,t)         -> loop (idx-1) t           (add x           newQ)
        if   index < 0
        then queue
        else loop index queue empty

    let zip queue1 queue2 =
        fold2 (fun q x y -> add (x,y) q) empty queue1 queue2

    let zip3 queue1 queue2 queue3 =
        fold3 (fun q x y z -> add (x,y,z) q) empty queue1 queue2 queue3

    let unzip queue =
        fold (fun (l,r) (x,y) -> add x l, add y r) (empty,empty) queue

    let unzip3 queue =
        fold (fun (l,m,r) (x,y,z) -> add x l, add y m, add z r) (empty,empty,empty) queue

    let reduce reducer queue =
        let folder state x =
            reducer state x
        ValueOption.map (fun (x,t) -> fold folder x t) (head queue)

    let reduceBack reducer queue =
        let folder state x =
            reducer x state
        ValueOption.map (fun (x,t) -> fold folder x t) (head (rev queue))

    let inline sum queue =
        let folder acc x =
            (acc: ^a) + x
        fold folder LanguagePrimitives.GenericZero queue

    let inline sumBy (projection: 'a -> ^b) queue =
        let folder acc x =
            (acc: ^b) + (projection x)
        fold folder LanguagePrimitives.GenericZero queue

    let forall predicate queue =
        let rec loop q =
            match head q with
            | ValueSome (x,t) ->
                if   predicate x
                then loop t
                else false
            | ValueNone -> true
        loop queue

    let forall2 predicate queue1 queue2 =
        let rec loop q1 q2 =
            match head q1, head q2 with
            | ValueSome (x,q1), ValueSome (y,q2) ->
                if   predicate x y
                then loop q1 q2
                else false
            | _ -> true
        loop queue1 queue2

    let any predicate queue =
        let rec loop q =
            match head q with
            | ValueSome (x,t) ->
                if   predicate x
                then true
                else loop t
            | ValueNone -> false
        loop queue

    let any2 predicate queue1 queue2 =
        let rec loop q1 q2 =
            match head q1, head q2 with
            | ValueSome (x,q1), ValueSome (y,q2) -> if predicate x y then true else loop q1 q2
            | ValueSome (x,q1), ValueNone        -> false
            | ValueNone       , ValueSome (y,q2) -> false
            | ValueNone       , ValueNone        -> false
        loop queue1 queue2

    let countBy (projection: 'a -> 'Key) queue =
        let dict = System.Collections.Generic.Dictionary()
        let mutable count  = 0
        let rec loop queue =
            match head queue with
            | ValueSome (x,t) ->
                let key = projection x
                match dict.TryGetValue(key, &count) with
                | false -> dict.Add(key, 1)
                | true  -> dict.[key] <- count + 1
                loop t
            | ValueNone -> ()
        loop queue

        Seq.fold (fun queue (KeyValue (key,value)) ->
            add (key,value) queue
        ) empty dict

    let groupBy (projection: 'a -> 'Key) queue =
        let dict = System.Collections.Generic.Dictionary()
        let mutable q = empty
        let rec loop queue =
            match head queue with
            | ValueSome (x,t) ->
                let key = projection x
                match dict.TryGetValue(key, &q) with
                | false -> dict.Add(key, one x)
                | true  -> dict.[key] <- add x q
                loop t
            | ValueNone -> ()
        loop queue

        Seq.fold (fun queue (KeyValue (key,value)) ->
            add (key,value) queue
        ) empty dict

    let exactlyOne queue =
        if   length queue = 1
        then ValueOption.map fst (head queue)
        else ValueNone

    let find predicate queue =
        let rec loop queue =
            match head queue with
            | ValueSome (x,t) ->
                if   predicate x
                then ValueSome x
                else loop t
            | ValueNone -> ValueNone
        loop queue

    let findRemove predicate queue =
        let rec loop newQ queue =
            match head queue with
            | ValueSome (x,queue) ->
                if   predicate x
                then ValueSome (x, append newQ queue)
                else loop (add x newQ) queue
            | ValueNone -> ValueNone
        loop empty queue

    let findBack predicate queue =
        fold (fun alreadyFound x ->
            if   predicate x
            then ValueSome x
            else alreadyFound
        ) ValueNone queue

    let findRemoveBack predicate queue =
        findRemove predicate (rev queue)
        |> ValueOption.map (fun (x,queue) -> (x,rev queue))

    let findIndex predicate queue =
        let rec loop idx queue =
            match head queue with
            | ValueSome (x,t) ->
                if   predicate x
                then ValueSome idx
                else loop (idx+1) t
            | ValueNone -> ValueNone
        loop 0 queue

    let findIndexBack predicate queue =
        foldi (fun idx alreadyFound x ->
            if   predicate x
            then ValueSome idx
            else alreadyFound
        ) ValueNone queue

    let pick chooser queue =
        let rec loop queue =
            match head queue with
            | ValueSome (x,queue) ->
                match chooser x with
                | ValueNone   -> loop queue
                | ValueSome x -> ValueSome x
            | ValueNone -> ValueNone
        loop queue

    let intersperse insert queue =
        let rec loop target queue =
            match head queue with
            | ValueSome (x,t) -> loop (add insert target |> add x) t
            | ValueNone       -> target
        match head queue with
        | ValueNone       -> empty
        | ValueSome (x,t) -> loop (add x empty) t

    let inline average queue =
        match reduce (fun x y -> x + y) queue with
        | ValueNone     -> ValueNone
        | ValueSome sum -> ValueSome (LanguagePrimitives.DivideByInt sum (length queue))

    let inline averageBy mapper queue =
        match head queue with
        | ValueSome (acc,t) ->
            ValueSome
                (LanguagePrimitives.DivideByInt
                    (fold (fun acc x -> acc + (mapper x)) (mapper acc) t)
                    (length t + 1))
        | ValueNone -> ValueNone

    let pairwise queue =
        zip queue (tail queue)

    let splitAt index queue =
        foldi (fun idx (left,right) x ->
            if   idx < index
            then (add x left,       right)
            else (      left, add x right)
        ) (empty,empty) queue

    let splitInto count queue =
        if   count <= 0           then empty
        elif count > length queue then map one queue
        else
            let size     = length queue
            let overhead = size % ((size / count) * count)
            let amounts  = List.init count (fun idx -> if idx < overhead then (size / count) + 1 else size /count)

            let rec loop take amounts queue current result =
                match take, head queue with
                | _     , ValueNone           -> add current result
                | 0     , ValueSome (x,queue) -> loop (List.head amounts - 1) (List.tail amounts) queue (add x empty)   (add current result)
                | amount, ValueSome (x,queue) -> loop (amount-1)               amounts            queue (add x current)        result
            loop (List.head amounts) (List.tail amounts) queue empty empty

    let permute indexMap queue =
        let length = length queue
        let source = [| for x in queue -> x |]
        let result = Array.zeroCreate length
        let setted = Array.create length 0uy

        let rec fromTo i stop =
            let idx = indexMap i
            if idx < 0 || idx >= length then
                ()
            else
                result.[idx] <- source.[i]
                setted.[idx] <- 1uy
                if i < stop then fromTo (i+1) stop
        fromTo 0 (length-1)

        if Array.contains 0uy setted
        then ValueNone
        else ValueSome (Array.fold (fun q x -> add x q) empty result)

    let rec permutations queue =
        let between insert queue = seq {
            for i=0 to length queue do
                yield insertAt i insert queue
        }

        match length queue with
        | 0 -> Seq.empty
        | 1 -> seq { queue }
        | _ ->
            match head queue with
            | ValueSome (x,queue) -> seq {
                for q in permutations queue do
                    yield! between x q
                }
            | ValueNone -> failwith "Cannot happen"

    let removeAt index queue =
        if index < 0 || index > lastIndex queue
        then queue
        else
            append
                (slice 0         (index-1)         queue)
                (slice (index+1) (lastIndex queue) queue)

    let removeManyAt index count queue =
        if index < 0 || index > lastIndex queue || count <= 0
        then queue
        else
            append
                (slice 0             (index-1)         queue)
                (slice (index+count) (lastIndex queue) queue)

    let transpose queues =
        let xs = Array.ofSeq queues
        if Array.isEmpty xs then
            empty
        else
            let count = Array.min (Array.map length xs)
            let res   = Array.create count empty
            for queue in xs do
                let rec loop i q =
                    if i < count then
                        match head q with
                        | ValueSome (x,q) ->
                            res.[i] <- add x (res.[i])
                            loop (i+1) q
                        | ValueNone -> ()
                loop 0 queue
            Queue res

    let windowed windowSize (queue:Queue<'a>) =
        if   windowSize <= 0 then empty
        elif windowSize  = 1 then map one queue
        else
            let rec loop res queue =
                let  newQ = take windowSize queue
                if   length newQ = windowSize
                then loop (add newQ res) (tail queue)
                else res
            loop empty queue

    // Mappings
    let replicate = repeat
    let collect   = bind
    let exists    = any
    let exists2   = any2
    let singleton = one

    // Converter
    let ofArray xs : Queue<'a> =
        Array.fold (fun q x -> q.Add x) Queue.Empty xs

    let ofList xs : Queue<'a> =
        List.fold (fun q x -> q.Add x) Queue.Empty xs

    let ofSeq (xs:seq<'a>) : Queue<'a> =
        match xs with
        | :? array<'a> as xs -> ofArray xs
        | :? list<'a>  as xs -> ofList xs
        | _                  -> Seq.fold (fun q x -> q.Add x) Queue.Empty xs

    let toSeq q =
        let unfolder q =
            match head q with
            | ValueNone       -> None
            | ValueSome (h,t) -> Some (h,t)
        Seq.unfold unfolder q

    let toArray q =
        let unfolder q =
            match head q with
            | ValueNone       -> None
            | ValueSome (h,t) -> Some (h,t)
        Array.unfold unfolder q

    let toList q =
        let folder x xs =
            x :: xs
        foldBack folder q []

    // Sorting
    let sort queue =
        let a = toArray queue
        Array.sortInPlace a
        ofArray a

    let sortDescending queue =
        let a = toArray queue
        ofArray (Array.sortDescending a)

    let sortBy (projection:'a -> 'Key) queue =
        let a = toArray queue
        Array.sortInPlaceBy projection a
        ofArray a

    let sortByDescending (projection: 'a -> 'Key) queue =
        let a = toArray queue
        ofArray (Array.sortByDescending projection a)

    let sortWith comparer queue =
        let a = toArray queue
        Array.sortInPlaceWith comparer a
        ofArray a

#nowarn "60"
type Queue<'a> with
    override q.ToString() =
        sprintf "Queue %A" (Queue.toList q)
    member this.Item with get(i:int) =
        Queue.item i this
    member this.GetSlice(start,stop) : Queue<'a> =
        Queue.slice
            (defaultArg start 0)
            (defaultArg stop (Queue.lastIndex this))
            this
    static member (++) (q1:Queue<'a>,q2:Queue<'a>) : Queue<'a> =
        Queue.append q1 q2
