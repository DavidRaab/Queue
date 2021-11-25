namespace Queue

open System.Collections.Generic

[<CustomEquality;NoComparison>]
type Queue<[<EqualityConditionalOn; ComparisonConditionalOn>]'a> =
    Queue of queue:list<'a> * added:list<'a> * length:int

    with
    member this.Length =
        let (Queue (_,_,length)) = this
        length

    member this.Head () =
        match this with
        | Queue([],[],_) -> ValueNone
        | Queue([],r,l)  ->
            let newQ = List.rev r
            ValueSome (List.head newQ, Queue((List.tail newQ),[],(l-1)))
        | Queue(q,a,l)   ->
            ValueSome (List.head q,    Queue((List.tail q),a,(l-1)))

    override this.Equals obj =
        match obj with
        | :? Queue<'a> as other ->
            let rec loop (queue1:Queue<'a>) (queue2:Queue<'a>) =
                match queue1.Head(), queue2.Head() with
                | ValueNone,        ValueNone        -> true
                | ValueSome (x,q1), ValueSome (y,q2) -> if Unchecked.equals x y then loop q1 q2 else false
                | _                                  -> failwith "Not Possible"
            if   this.Length = other.Length
            then loop this other
            else false
        | _ ->
            false

    override this.GetHashCode () = Unchecked.hash this

    member private this.asSequence () =
        let s = seq {
            let mutable queue  = this
            let mutable notEnd = true
            while notEnd do
                match queue.Head () with
                | ValueNone       -> notEnd <- false
                | ValueSome (h,t) ->
                    queue <- t
                    yield h
        }
        s.GetEnumerator()

    interface IEnumerable<'a> with
        override this.GetEnumerator(): IEnumerator<'a> =
            this.asSequence ()

        override this.GetEnumerator(): System.Collections.IEnumerator =
            this.asSequence ()

module Queue =
    // Creation of Queue
    let empty               = Queue ([],[],0)
    let private queue q a l = Queue (q,a,l)

    let add x (Queue (q,r,l)) =
        queue q (x::r) (l+1)

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

    let addMany xs queue =
        Seq.fold (fun q x -> add x q) queue xs

    let inline range start stop =
        if stop < start then
            empty
        else
            start |> unfold (fun x ->
                if x <= stop then
                    ValueSome (x, x+LanguagePrimitives.GenericOne)
                else
                    ValueNone
            )

    let inline rangeWithStep start step stop =
        if stop < start then
            empty
        else
            start |> unfold (fun x ->
                if x <= stop then
                    ValueSome (x, x+step)
                else
                    ValueNone
            )

    let init length generator =
        let gen idx =
            if idx < length then
                ValueSome (generator idx, idx+1)
            else
                ValueNone
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
        | ValueNone       -> empty
        | ValueSome (h,t) -> t

    let fold f (state:'State) queue =
        let rec loop state queue =
            match head queue with
            | ValueNone       -> state
            | ValueSome (x,t) -> loop (f state x) t
        loop state queue

    let scan f (state:'State) queue =
        let rec loop state states queue =
            match head queue with
            | ValueNone       -> add state states
            | ValueSome (x,t) -> loop (f state x) (add state states) t
        loop state empty queue

    let fold2 f (state:'State) queue1 queue2 =
        let rec loop state q1 q2 =
            match head q1, head q2 with
            | ValueNone        , ValueNone         -> state
            | ValueNone        , ValueSome _       -> state
            | ValueSome _      , ValueNone         -> state
            | ValueSome (x1,q1), ValueSome (x2,q2) -> loop (f state x1 x2) q1 q2
        loop state queue1 queue2

    let foldBack f q (state:'State) =
        let rec loop state q =
            match q with
            | Queue([],[]  ,_) -> state
            | Queue(q ,[]  ,l) -> loop state (queue [] (List.rev q) l)
            | Queue(q ,x::a,l) -> loop (f x state) (queue q a (l-1))
        loop state q

    let apply fq xq =
        let rec loop fq xq state =
            match head fq, head xq with
            | ValueNone       , ValueNone        -> state
            | ValueNone       , ValueSome _      -> state
            | ValueSome _     , ValueNone        -> state
            | ValueSome (f,fq), ValueSome (x,xq) -> loop fq xq (add (f x) state)
        loop fq xq empty

    let append queue1 queue2 =
        let folder state x =
            add x state
        fold folder queue1 queue2

    let bind f queue =
        let folder state x =
            append state (f x)
        fold folder empty queue

    let map f queue =
        let folder q x =
            add (f x) q
        fold folder empty queue

    // Utilities
    let lastIndex queue =
        length queue - 1

    let last (Queue (r,a,_)) =
        match r,a with
        | [],[]      -> ValueNone
        | r ,[]      -> ValueSome (List.last r)
        | _ ,last::a -> ValueSome last

    let max queue =
        let folder state x =
            match state with
            | ValueNone   -> ValueSome x
            | ValueSome y -> ValueSome (max x y)
        fold folder ValueNone queue

    let maxBy projection queue =
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

    let minBy projection queue =
        let folder state item =
            match state with
            | ValueNone                 -> ValueSome (projection item, item)
            | ValueSome (min,_) as orig ->
                let itemMin = projection item
                if itemMin < min then ValueSome (itemMin,item) else orig
        ValueOption.map snd (fold folder ValueNone queue)

    let rev queue =
        foldBack add queue empty

    let foldi f (state:'State) queue =
        fold (fun (idx,state) x ->
            (idx+1, f idx state x)
        ) (0,state) queue
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

    let compareWith comparer queue1 queue2 =
        let rec loop queue1 queue2 =
            match head queue1, head queue2 with
            | ValueNone  , ValueNone   ->  0
            | ValueSome _, ValueNone   ->  1
            | ValueNone  , ValueSome _ -> -1
            | ValueSome (x,q1), ValueSome (y,q2) ->
                let ret = comparer x y
                if   ret = 0
                then loop q1 q2
                else ret
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
        if   size < 0
        then add empty empty
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
        if   index < 0 || index >= lastIndex queue
        then queue
        else loop index queue empty

    let insertAtWithExpanding zeroElement index value queue =
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

    let updateAtWithExpanding zeroElement index value queue =
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

    let insertManyAtWithExpanding zeroElement index values queue =
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
        fold2 (fun q (x,y) z -> add (x,y,z) q) empty (zip queue1 queue2) queue3

    let reduce reducer queue =
        let folder state x =
            reducer state x
        ValueOption.map (fun (x,t) -> fold folder x t) (head queue)

    let inline sum queue =
        match reduce (+) queue with
        | ValueNone   -> LanguagePrimitives.GenericZero<_>
        | ValueSome x -> x

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
        let mutable count = Unchecked.defaultof<_>
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

    let exactlyOne queue =
        match head queue with
        | ValueNone       -> ValueNone
        | ValueSome (x,t) ->
            match head t with
            | ValueNone   -> ValueSome x
            | ValueSome _ -> ValueNone

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

    // Mappings
    let replicate = repeat
    let collect   = bind
    let exists    = any

    // Side-Effects
    let rec iter f queue =
        match head queue with
        | ValueNone       -> ()
        | ValueSome (h,t) -> f h; iter f t

    let iteri f queue =
        let rec loop idx queue =
            match head queue with
            | ValueNone       -> ()
            | ValueSome (x,t) -> f idx x; loop (idx+1) t
        loop 0 queue

    let id = 3

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

#nowarn "60"
type Queue<'a> with
    override q.ToString() = sprintf "Queue %A" (Queue.toList q)
