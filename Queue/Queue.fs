namespace Queue

[<CustomEquality;CustomComparison>]
type Queue<[<EqualityConditionalOn; ComparisonConditionalOn>]'a> =
    Queue of queue:list<'a> * added:list<'a> * length:int

    with
    member this.Length =
        let (Queue (_,_,length)) = this
        length

    member this.Head () =
        match this with
        | Queue([] ,[],_)  -> ValueNone
        | Queue([x],[],1)  -> ValueSome (x,Queue([],[],0))
        | Queue([] ,[x],1) -> ValueSome (x,Queue([],[],0))
        | Queue([] ,r,l)   ->
            let newQ = List.rev r
            ValueSome (List.head newQ, Queue((List.tail newQ),[],(l-1)))
        | Queue(q,a,l)   ->
            ValueSome (List.head q,    Queue((List.tail q),a,(l-1)))

    override this.Equals obj =
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

    override this.GetHashCode () = Unchecked.hash this

    member private this.asSequence () =
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
            this.asSequence ()

        override this.GetEnumerator(): System.Collections.IEnumerator =
            this.asSequence ()

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
            | _ -> failwith "Boom"

module Queue =
    // Creation of Queue
    let empty               = Queue ([],[],0)
    let private queue q a l = Queue (q,a,l)

    let add x (Queue (q,r,l)) =
        queue q (x::r) (l+1)

    let addMany xs queue =
        Seq.fold (fun q x -> add x q) queue xs

    let prepend x (Queue (q,r,l)) =
        queue (x::q) r (l+1)

    let prependMany xs queue =
        Seq.fold (fun q x -> prepend x q) queue xs

    let one x =
        add x empty

    let unfold generator (state:'State) =
        let rec loop q state =
            match generator state with
            | ValueNone           -> q
            | ValueSome (x,state) -> loop (add x q) state
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
    let isEmpty (Queue (xs,ys,_)) =
        xs = [] && ys = []

    let length (q:Queue<'a>) =
        q.Length

    let head (q:Queue<'a>) =
        q.Head()

    let equal (q1:Queue<'a>) (q2:Queue<'a>) =
        q1.Equals(q2)

    let tail queue =
        match head queue with
        | ValueSome (h,t) -> t
        | ValueNone       -> empty

    let fold f (state:'State) queue =
        let rec loop state queue =
            match head queue with
            | ValueSome (x,t) -> loop (f state x) t
            | ValueNone       -> state
        loop state queue

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
        let (Queue (q,a,_)) = queue
        let rec loop state a q =
            match a,q with
            | [],[]   -> state
            | x::a,q  -> loop (f x state) a  q
            | [],x::q -> loop (f x state) [] q
        loop state a (List.rev q)

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
        fold (fun state x -> addMany (f x) state)  empty queue

    let map f queue =
        let folder q x =
            add (f x) q
        fold folder empty queue

    let rev queue =
        let (Queue (q,a,amount)) = queue
        Queue.Queue (a,q,amount)

    // Utilities
    let lastIndex queue =
        length queue - 1

    let append (queue1:Queue<'a>) (queue2:Queue<'a>) =
        if   queue1.Length < queue2.Length
        then prependMany (rev queue1) queue2
        else addMany      queue2      queue1

    let last (Queue (r,a,_)) =
        match r,a with
        | [],[]      -> ValueNone
        | r ,[]      -> ValueSome (List.last r)
        | _ ,last::a -> ValueSome last

    let mapReduce mapper reducer queue =
        let rec loop state queue =
            match head queue with
            | ValueNone           -> ValueSome state
            | ValueSome (x,queue) -> loop (reducer state (mapper x)) queue
        match head queue with
        | ValueNone           -> ValueNone
        | ValueSome (x,queue) -> loop (mapper x) queue

    let mapFold mapper folder (state:'State) queue =
        let rec loop state queue =
            match head queue with
            | ValueNone           -> state
            | ValueSome (x,queue) -> loop (folder state (mapper x)) queue
        loop state queue

    let max queue =
        let folder state x =
            match state with
            | ValueNone   -> ValueSome x
            | ValueSome y -> ValueSome (max x y)
        fold folder ValueNone queue

    let maxBy (projection: 'a -> 'Key) queue =
        let folder state item =
            match state with
            | ValueNone                 -> ValueSome (projection item, item)
            | ValueSome (max,_) as orig ->
                let itemMax = projection item
                if itemMax > max then ValueSome (itemMax,item) else orig
        ValueOption.map snd (fold folder ValueNone queue)

    let min queue =
        let folder state x =
            match state with
            | ValueNone   -> ValueSome x
            | ValueSome y -> ValueSome (min x y)
        fold folder ValueNone queue

    let minBy (projection: 'a -> 'Key) queue =
        let folder state item =
            match state with
            | ValueNone                 -> ValueSome (projection item, item)
            | ValueSome (min,_) as orig ->
                let itemMin = projection item
                if itemMin < min then ValueSome (itemMin,item) else orig
        ValueOption.map snd (fold folder ValueNone queue)

    let foldi f (state:'State) queue =
        fold (fun (idx,state) x ->
            (idx+1, f idx state x)
        ) (0,state) queue
        |> snd

    let fold2i f (state:'State) queue1 queue2 =
        fold2 (fun (idx,state) x1 x2 ->
            (idx+1, f idx state x1 x2)
        ) (0,state) queue1 queue2
        |> snd

    let fold3i f (state:'State) queue1 queue2 queue3 =
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
        apply (map f queue1) queue2

    let map3 f queue1 queue2 queue3 =
        apply (map2 f queue1 queue2) queue3

    let map4 f queue1 queue2 queue3 queue4 =
        apply (map3 f queue1 queue2 queue3) queue4

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
            | ValueNone  , ValueNone   ->  0
            | ValueSome _, ValueNone   ->  1
            | ValueNone  , ValueSome _ -> -1
            | ValueSome (x,q1), ValueSome (y,q2) ->
                match comparer x y with
                | 0 -> loop q1 q2
                | x -> x
        loop queue1 queue2

    let allPairs queue1 queue2 =
        lift2 (fun x y -> (x,y)) queue1 queue2

    let chunkBySize size queue =
        let rec loop amount queue inner outer =
            if amount = size then
                loop 0 queue empty (add inner outer)
            else
                match head queue with
                | ValueNone       -> add inner outer
                | ValueSome (x,t) -> loop (amount+1) t (add x inner) outer
        if   size < 0 || isEmpty queue
        then empty
        else loop 0 queue empty empty

    let item idx queue =
        let rec loop i queue =
            match head queue with
            | ValueNone       -> ValueNone
            | ValueSome (x,t) ->
                if i = idx then
                    ValueSome x
                else
                    loop (i+1) t
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
            | ValueNone   -> q
            | ValueSome x -> add x q
        fold folder empty queue

    let contains x (Queue (q,a,_)) =
        List.contains x q || List.contains x a

    let take amount q =
        let gen (amount,q) =
            if amount > 0 then
                match head q with
                | ValueNone       -> ValueNone
                | ValueSome (h,t) -> ValueSome (h, ((amount-1),t))
            else
                ValueNone
        if   amount <= 0
        then empty
        else unfold gen (amount,q)

    let skip amount queue =
        let rec loop amount q =
            if amount > 0 then
                match head q with
                | ValueNone       -> empty
                | ValueSome (_,t) -> loop (amount-1) t
            else
                q
        if   amount <= 0
        then queue
        else loop amount queue

    let takeWhile predicate queue =
        let rec loop queue newQ =
            match head queue with
            | ValueNone       -> newQ
            | ValueSome (x,t) ->
                if   predicate x
                then loop t (add x newQ)
                else newQ
        loop queue empty

    let skipWhile predicate queue =
        let rec loop queue =
            match head queue with
            | ValueNone       -> empty
            | ValueSome (x,t) ->
                if   predicate x
                then loop t
                else queue
        loop queue

    let distinct queue =
        let seen = System.Collections.Generic.Dictionary()
        let mutable value = false
        let rec loop newQ queue =
            match head queue with
            | ValueNone      -> newQ
            | ValueSome(x,t) ->
                value <- false
                match seen.TryGetValue(x, &value) with
                | true  ->                   loop newQ         t
                | false -> seen.Add(x,true); loop (add x newQ) t
        loop empty queue

    let distinctBy (projection: 'a -> 'Key) queue =
        let seen = System.Collections.Generic.Dictionary()
        let mutable value = false
        let rec loop newQ queue =
            match head queue with
            | ValueNone      -> newQ
            | ValueSome(x,t) ->
                let key = projection x
                value <- false
                match seen.TryGetValue(key, &value) with
                | true  ->                     loop newQ         t
                | false -> seen.Add(key,true); loop (add x newQ) t
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
            | 0,   ValueSome (x,t)         -> loop -1      t     (add x           (add value newQ))
            | idx, ValueNone               -> loop (idx-1) empty (add zeroElement newQ)
            | idx, ValueSome (x,t)         -> loop (idx-1) t     (add x           newQ)
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
            | index, ValueSome (x,t)          -> loop (index-1) t     (add x newQ)
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
            | 0,   ValueSome (x,t)         -> loop -1      t     (add x (addMany values newQ))
            | idx, ValueNone               -> loop (idx-1) empty (add zeroElement newQ)
            | idx, ValueSome (x,t)         -> loop (idx-1) t     (add x           newQ)
        if   index < 0
        then queue
        else loop index queue empty

    let zip queue1 queue2 =
        fold2 (fun q x y -> add (x,y) q) empty queue1 queue2

    let zip3 queue1 queue2 queue3 =
        fold3 (fun q x y z -> add (x,y,z) q) empty queue1 queue2 queue3

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
            | ValueNone       -> true
            | ValueSome (x,t) ->
                if predicate x then
                    loop t
                else
                    false
        loop queue

    let any predicate queue =
        let rec loop q =
            match head q with
            | ValueNone       -> false
            | ValueSome (x,t) ->
                if predicate x then
                    true
                else
                    loop t
        loop queue

    let countBy (projection: 'a -> 'Key) queue =
        let dict = System.Collections.Generic.Dictionary()
        let mutable count  = 0
        let rec loop queue =
            match head queue with
            | ValueNone       -> ()
            | ValueSome (x,t) ->
                let key = projection x
                match dict.TryGetValue(key, &count) with
                | false -> dict.Add(key, 1)
                | true  -> dict.[key] <- count + 1
                loop t
        loop queue

        Seq.fold (fun queue (KeyValue (key,value)) ->
            add (key,value) queue
        ) empty dict

    let groupBy (projection: 'a -> 'Key) queue =
        let dict = System.Collections.Generic.Dictionary()
        let mutable q = empty
        let rec loop queue =
            match head queue with
            | ValueNone       -> ()
            | ValueSome (x,t) ->
                let key = projection x
                match dict.TryGetValue(key, &q) with
                | false -> dict.Add(key, one x)
                | true  -> dict.[key] <- add x q
                loop t
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
            | ValueNone       -> ValueNone
            | ValueSome (x,t) ->
                if   predicate x
                then ValueSome x
                else loop t
        loop queue

    let findBack predicate queue =
        fold (fun alreadyFound x ->
            if   predicate x
            then ValueSome x
            else alreadyFound
        ) ValueNone queue

    let findIndex predicate queue =
        let rec loop idx queue =
            match head queue with
            | ValueNone       -> ValueNone
            | ValueSome (x,t) ->
                if   predicate x
                then ValueSome idx
                else loop (idx+1) t
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
            | ValueNone           -> ValueNone
            | ValueSome (x,queue) ->
                match chooser x with
                | ValueNone   -> loop queue
                | ValueSome x -> ValueSome x
        loop queue

    let intersperse insert queue =
        let rec loop target queue =
            match head queue with
            | ValueNone       -> target
            | ValueSome (x,t) -> loop (add insert target |> add x) t
        match head queue with
        | ValueNone       -> empty
        | ValueSome (x,t) -> loop (add x empty) t

    let inline average queue =
        match reduce (fun x y -> x + y) queue with
        | ValueNone     -> ValueNone
        | ValueSome sum -> ValueSome (LanguagePrimitives.DivideByInt sum (length queue))

    let inline averageBy mapper queue =
        match head queue with
        | ValueNone         -> ValueNone
        | ValueSome (acc,t) ->
            ValueSome
                (LanguagePrimitives.DivideByInt
                    (fold (fun acc x -> acc + (mapper x)) (mapper acc) t)
                    (length t + 1))

    let pairwise queue =
        zip queue (tail queue)

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
            | ValueNone           -> failwith "Cannot happen"
            | ValueSome (x,queue) -> seq {
                for q in permutations queue do
                    yield! between x q
            }

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

    // Mappings
    let replicate = repeat
    let collect   = bind
    let exists    = any
    let singleton = one

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

    // Converter
    let ofArray xs =
        let folder q x =
            add x q
        Array.fold folder empty xs

    let ofList xs =
        let folder q x =
            add x q
        List.fold folder empty xs

    let ofSeq (xs:seq<_>) =
        match xs with
        | :? array<_> as xs -> ofArray xs
        | :? list<_>  as xs -> ofList xs
        | _ ->
            let folder q x =
                add x q
            Seq.fold folder empty xs

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
    override q.ToString()            = sprintf "Queue %A" (Queue.toList q)
    member this.Item with get(i:int) = Queue.item i this
    member this.GetSlice(start,stop) =
        let start,stop = defaultArg start 0, defaultArg stop (Queue.lastIndex this)
        Queue.slice start stop this
    static member (++) (q1,q2)       = Queue.append q1 q2
