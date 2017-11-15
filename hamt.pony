use "debug"
use "collections"

interface Visitor[K, V]
    fun apply(k: K, v: V)

class val HAMTNode[K: Equatable[K] val, V: Any val]
    let key: (K | None)
    let value: (V | None)
    let children: Array[(HAMTNode[K, V] val | None)] val

    new create(k: (K | None), v: (V | None), c: Array[(HAMTNode[K, V] val | None)] val) =>
        key = consume k
        value = consume v
        children = c

    new with_kv(k: (K | None), v: (V | None)) =>
        key = consume k
        value = consume v
        children = recover Array[(HAMTNode[K, V] val | None)].init(None, 1 << 6) end

    fun val insert(index: U64, k: K, v: V): HAMTNode[K, V] val =>
        match key
        | None if index == 0 => recover HAMTNode[K, V](consume k, consume v, children) end
        | let here: K if (index == 0) and (k == here) => recover HAMTNode[K, V](consume k, consume v, children) end
        | let here: (K | None) =>
            try
                let head = index and ((1 << 6) - 1)
                let tail = index >> 6
                let c = recover iso children.clone() end
                match c(head.usize())?
                | None => c(head.usize())? = (recover val HAMTNode[K, V].with_kv(None, None) end).insert(tail, consume k, consume v)
                | let node: HAMTNode[K, V] val => c(head.usize())? = node.insert(tail, consume k, consume v)
                end
                recover HAMTNode[K, V](here, value, consume c) end
            else
                // we know this will never happen
                Debug.out("Array subscription failed. This must not happen")
                recover HAMTNode[K, V](None, None, recover Array[(HAMTNode[K, V] val | None)] end) end
            end
        end

    fun val get(index: U64, k: K): (V | None) =>
        match value
        | None if index == 0 => None
        | let here: K if (index == 0) and (here == k) => value
        | let here: (K | None) =>
            try
                let head = index and ((1 << 6) - 1)
                let tail = index >> 6
                match children(head.usize())?
                | None => None
                | let node: HAMTNode[K, V] val => node.get(tail, consume k)
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
        root = recover HAMTNode[K, V].with_kv(None, None) end
        count = 0

    new init(r: HAMTNode[K, V] val, c: USize) =>
        root = r
        count = c

    fun val insert(k: K, v: V): HAMT[K, V, H] val =>
        recover HAMT[K, V, H].init(root.insert(H.hash(k), consume k, consume v), count + 1) end

    fun val get(k: K): (V | None) =>
        root.get(H.hash(k), consume k)

    fun val get_root(): HAMTNode[K, V] val =>
        root

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
