use "debug"
use "collections"

interface Visitor[K, V]
    fun apply(k: K, v: V)

class val HAMTNode[K: Equatable[K] val, V: Any val]
    let key: (K | None)
    let value: (V | None)
    let children: Array[(HAMTNode[K, V] val | None)] val

    new create(k: (K | None), v: (V | None), c: Array[(HAMTNode[K, V] val | None)] val) =>
        key = k
        value = v
        children = c

    new with_kv(k: K, v: V) =>
        key = k
        value = v
        children = recover Array[(HAMTNode[K, V] val | None)].init(None, 1 << 6) end

    new empty() =>
        key = None
        value = None
        children = recover Array[(HAMTNode[K, V] val | None)].init(None, 1 << 6) end

    fun insert(index: U64, k: K, v: V): HAMTNode[K, V] iso^ =>
        match key
        | None if index == 0 => recover HAMTNode[K, V](k, v, children) end
        | let here: K if (index == 0) and (k == here) => recover HAMTNode[K, V](k, v, children) end
        | let here: (K | None) =>
            try
                let head = index and ((1 << 6) - 1)
                let tail = index >> 6
                let c = recover iso children.clone() end
                match c(head.usize())?
                | None => c(head.usize())? = recover HAMTNode[K, V].empty().insert(tail, k, v) end
                | let node: HAMTNode[K, V] val => c(head.usize())? = recover node.insert(tail, k, v) end
                end
                recover HAMTNode[K, V](here, value, consume c) end
            else
                // we know this will never happen
                Debug.out("Array subscription failed. This must not happen")
                recover HAMTNode[K, V].empty() end
            end
        end

    fun get(index: U64, k: K): (V | None) =>
        match key
        | None if index == 0 => None
        | let here: K if (index == 0) and (here == k) => value
        | let here: (K | None) =>
            try
                let head = index and ((1 << 6) - 1)
                let tail = index >> 6
                match children(head.usize())?
                | None => None
                | let node: HAMTNode[K, V] val => node.get(tail, k)
                end
            else
                // we know this will never happen
                Debug.out("Array subscription failed. This must not happen")
                None
            end
        end

    fun val visit(visitor: Visitor[K, V]) =>
        try
            let k = key as K
            let v = value as V
            visitor(k, v)
        end
        for child in children.values() do
            try
                let c = child as HAMTNode[K, V]
                c.visit(visitor)
            end
        end

class val HAMT[K: Equatable[K] val, V: Any val, H: HashFunction[K] val]
    let root: HAMTNode[K, V] val
    let count: USize

    new create() =>
        root = recover HAMTNode[K, V].empty() end
        count = 0

    new init(r: HAMTNode[K, V] val, c: USize) =>
        root = r
        count = c

    fun insert(k: K, v: V): HAMT[K, V, H] val =>
        recover HAMT[K, V, H].init(root.insert(H.hash(k), k, v), count + 1) end

    fun get(k: K): (V | None) =>
        root.get(H.hash(k), k)

    fun get_root(): HAMTNode[K, V] val =>
        root

    fun apply(k: K): V ? =>
        match get(k)
        | None => error
        | let v: V => v
        end

    fun size(): USize => count

type HAMTIs[K: Equatable[K] val, V: Any val] is HAMT[K, V, HashIs[K]]

actor Main
    new create(env: Env) =>
        let a = recover val HAMTIs[String, I32] end
        let b = recover val a.insert("hoge", 100) end
        let c = recover val b.insert("hige", 200) end
        let d = recover val c.insert("hage", 300) end

        let root = d.get_root()
        root.visit({(s: String, x: Int) => env.out.print(s + ":" + x.string()) })

        env.out.print(d.get("hoge").string())
        env.out.print(d.get("hige").string())
        env.out.print(d.get("hege").string())
        env.out.print(d.get("hage").string())

