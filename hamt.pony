use "debug"
use "collections"

interface Visitor[T]
    fun apply(v: T)

class val HAMTNode[T: Equatable[T] val]
    let value: (T | None)
    let children: Array[(HAMTNode[T] val | None)] val

    new create(v: (T | None), c: Array[(HAMTNode[T] val | None)] val) =>
        value = consume v
        children = c

    new with_value(v: (T | None)) =>
        value = consume v
        children = recover Array[(HAMTNode[T] val | None)].init(None, 1 << 6) end

    fun val insert(index: U64, v: T): HAMTNode[T] val =>
        match value
        | None if index == 0 => recover HAMTNode[T](consume v, children) end
        | let here: T if (index == 0) and (v == here) => this
        | let here: (T | None) =>
            try
                let head = index and ((1 << 6) - 1)
                let tail = index >> 6
                let c = recover iso children.clone() end
                match c(head.usize())?
                | None => c(head.usize())? = (recover val HAMTNode[T].with_value(None) end).insert(tail, consume v)
                | let node: HAMTNode[T] val => c(head.usize())? = node.insert(tail, consume v)
                end
                recover HAMTNode[T](here, consume c) end
            else
                // we know this will never happen
                Debug.out("Array subscription failed. This must not happen")
                recover HAMTNode[T](None, recover Array[(HAMTNode[T] val | None)] end) end
            end
        end

    fun val get(index: U64, v: T): (T | None) =>
        match value
        | None if index == 0 => None
        | let here: T if (index == 0) and (here == v) => here
        | let here: (T | None) =>
            try
                let head = index and ((1 << 6) - 1)
                let tail = index >> 6
                match children(head.usize())?
                | None => None
                | let node: HAMTNode[T] val => node.get(tail, consume v)
                end
            else
                // we know this will never happen
                Debug.out("Array subscription failed. This must not happen")
                None
            end
        end

    fun val visit(visitor: Visitor[T]) =>
        try
            let v = value as T
            visitor(v)
        end
        for child in children.values() do
            try
                let c = child as HAMTNode[T]
                c.visit(visitor)
            end
        end

class val HAMT[T: Equatable[T] val, H: HashFunction[T] val]
    let root: HAMTNode[T] val
    let count: USize

    new create() =>
        root = recover HAMTNode[T].with_value(None) end
        count = 0

    new init(r: HAMTNode[T] val, c: USize) =>
        root = r
        count = c

    fun val insert(v: T): HAMT[T, H] val =>
        recover HAMT[T, H].init(root.insert(H.hash(v), consume v), count + 1) end

    fun val get(v: T): (T | None) =>
        root.get(H.hash(v), consume v)

    fun val get_root(): HAMTNode[T] val =>
        root

type HAMTIs[T: Equatable[T] val] is HAMT[T, HashIs[T]]

actor Main
    new create(env: Env) =>
        let a = recover val HAMTIs[String] end
        let b = recover val a.insert("hoge") end
        let c = recover val b.insert("hige") end
        let d = recover val c.insert("hage") end

        let root = d.get_root()
        root.visit({(s: String) => env.out.print(s) })

        env.out.print(d.get("hoge").string())
        env.out.print(d.get("hige").string())
        env.out.print(d.get("hege").string())
        env.out.print(d.get("hage").string())
