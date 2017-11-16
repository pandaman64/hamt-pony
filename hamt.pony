use "debug"
use "collections"

class Bitmap
    let value: U64

    new create(v: U64) =>
        value = v

    fun exists(index: U64): Bool =>
        (value and (1 << index)) != 0

    fun array_index(index: U64): USize =>
        Debug.out(index.string())
        var ret: USize = 0
        var i: U64 = 0
        while i < index do
            if exists(i) then
                ret = ret + 1
            end
            i = i + 1
        end
        ret

    fun add(v: U64): Bitmap iso^ =>
        recover Bitmap(value or (1 << v)) end

interface Visitor[K, V]
    fun apply(k: K, v: V)

class val HAMTNode[K: Equatable[K] val, V: Any val]
    let key: (K | None)
    let value: (V | None)
    let bitmap: Bitmap val
    let children: Array[HAMTNode[K, V] val] val

    new create(k: (K | None), v: (V | None), b: Bitmap val, c: Array[HAMTNode[K, V] val] val) =>
        key = k
        value = v
        bitmap = b
        children = c

    new with_kv(k: K, v: V) =>
        key = k
        value = v
        bitmap = recover Bitmap(0) end
        children = recover Array[HAMTNode[K, V] val] end

    new empty() =>
        key = None
        value = None
        bitmap = recover Bitmap(0) end
        children = recover Array[HAMTNode[K, V] val] end

    fun insert(index: U64, k: K, v: V): HAMTNode[K, V] iso^ =>
        match key
        | None if index == 0 => recover HAMTNode[K, V](k, v, bitmap.add(0),  children) end
        | let here: K if (index == 0) and (k == here) => recover HAMTNode[K, V](k, v, bitmap, children) end
        | let here: (K | None) =>
            try
                let head = index and ((1 << 6) - 1)
                let tail = index >> 6

                Debug.out("head:" + head.string())
                Debug.out("tail:" + tail.string())

                var b = bitmap
                let c = recover iso children.clone() end

                if bitmap.exists(head) then
                    let i = bitmap.array_index(head)
                    c(i)? = recover c(i)?.insert(tail, k, v) end
                else
                    b = bitmap.add(head)
                    c.push(recover HAMTNode[K, V].empty().insert(tail, k, v) end)
                end

                recover HAMTNode[K, V](here, value, b, consume c) end
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

                Debug.out("head:" + head.string())
                Debug.out("tail:" + tail.string())

                if bitmap.exists(head) then
                    children(bitmap.array_index(head))?.get(tail, k)
                else
                    None
                end
            else
                // we know this will never happen
                Debug.out("Array subscription failed. This must not happen")
                None
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

