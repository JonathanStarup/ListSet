mod JonathanStarup {
//
// -- INVARIANT --
// The list of ListSet never contain duplicates at any point.
//
// -- OPTIMIZATION --
// - A: Exploit the unordered nature of the inner list (`UnorderedList`)
// - B: Other things being equal, maintain invariants while traversing to minimize
//   peak memory usage
//

///
/// A linear representation of a set, requiring only `Eq[t]`.
///
/// The set is very principled so no function is allowed to see the inner order
/// of the elements in the set. `toString` is the one exception, but the output
/// of that should not be relied on.
///
enum ListSet[t](List[t])

instance Eq[ListSet[t]] with Eq[t] {
    pub def eq(x: ListSet[t], y: ListSet[t]): Bool =
        ListSet.eq(x, y)
}

instance ToString[ListSet[t]] with ToString[t], Eq[t] {
    pub def toString(x: ListSet[t]): String =
        ListSet.toString(x)
}

instance LowerBound[ListSet[t]] with Eq[t] {
    pub def minValue(): ListSet[t] =
        ListSet.empty()
}

instance PartialOrder[ListSet[t]] with Eq[t] {
    pub def lessEqual(x: ListSet[t], y: ListSet[t]): Bool =
        ListSet.isSubsetOf(x, y)
}

instance JoinLattice[ListSet[t]] with Eq[t] {
    pub def leastUpperBound(x: ListSet[t], y: ListSet[t]): ListSet[t] =
        ListSet.union(x, y)
}

instance MeetLattice[ListSet[t]] with Eq[t] {
    pub def greatestLowerBound(x: ListSet[t], y: ListSet[t]): ListSet[t] =
        ListSet.intersection(x, y)
}

instance SemiGroup[ListSet[t]] with Eq[t] {
    pub def combine(x: ListSet[t], y: ListSet[t]): ListSet[t] =
        ListSet.union(x, y)
}

instance CommutativeSemiGroup[ListSet[t]] with Eq[t]

instance Collectable[ListSet[t]] with Eq[t] {
    type Elm = t
    type Aef = {}
    pub def collect(iter: Iterator[t, ef, r]): ListSet[t] \ ef + r =
        ListSet.collect(iter)
}

///
/// The functions of ListSet all contain `Eq[t]` even though it is not required
/// to be well-typed.
///
mod ListSet {
    use Eq.{eq, neq}
    use Foldable.foldLeft
    use JonathanStarup.{UnorderedList => UList}
    use JonathanStarup.ListOps

    /// Returns the list of `s`.
    def extract(s: ListSet[t]): List[t] = {
        let ListSet(l) = s;
        l
    }

    /// Returns the empty `ListSet`.
    pub def empty(): ListSet[t] with Eq[t] =
        ListSet(Nil)

    ///
    /// Returns a set that has the elements of `s` and also `x`.
    ///
    pub def insert(x: t, s: ListSet[t]): ListSet[t] with Eq[t] =
        s |> extract |> listInsert(x) |> ListSet

    def listInsert(x: t, l: List[t]): List[t] with Eq[t] =
        if (List.memberOf(x, l)) l else x :: l

    ///
    /// Returns a set that has the elements of `s`, except `x`.
    ///
    /// This is a linear time operation.
    ///
    pub def remove(x: t, s: ListSet[t]): ListSet[t] with Eq[t] =
        s |> extract |> listRemove(x) |> ListSet

    def listRemove(x: t, l: List[t]): List[t] with Eq[t] =
        listRemoveHelper(x, Nil, l)

    def listRemoveHelper(x: t, acc: List[t], l: List[t]): List[t] with Eq[t] =
        match l {
            case hd :: tl =>
                if (hd == x) UList.append(acc, tl)
                else listRemoveHelper(x, hd :: acc, tl)
            case Nil => acc
        }

    ///
    /// Returns `true` if `s` contains `x`.
    ///
    /// This is a linear time operation.
    ///
    pub def memberOf(x: t, s: ListSet[t]): Bool with Eq[t] =
        s |> extract |> List.memberOf(x)

    ///
    /// Returns a set that contains the elements of `s1` and `s2`.
    ///
    /// This is a quadratic time operation.
    ///
    pub def union(s1: ListSet[t], s2: ListSet[t]): ListSet[t] with Eq[t] =
        listUnion(extract(s1), extract(s2)) |> ListSet

    def listUnion(l1: List[t], l2: List[t]): List[t] with Eq[t] =
        (l1, l2) ||> foldLeft(acc -> elm -> listInsert(elm, acc))

    ///
    /// Returns a set that contains the elements in both sets.
    ///
    /// This is a quadratic time operation.
    ///
    pub def intersection(s1: ListSet[t], s2: ListSet[t]): ListSet[t] with Eq[t] =
        listIntersection(extract(s1), extract(s2)) |> ListSet

    def listIntersection(l1: List[t], l2: List[t]): List[t] with Eq[t] =
        l1 |> UList.filter(elm -> List.memberOf(elm, l2))

    ///
    /// Returns a set that contains the elements in `s1` but not in `s2`.
    ///
    /// This is a quadratic time operation.
    ///
    pub def difference(s1: ListSet[t], s2: ListSet[t]): ListSet[t] with Eq[t] =
        listDifference(extract(s1), extract(s2)) |> ListSet

    def listDifference(l1: List[t], l2: List[t]): List[t] with Eq[t] =
        (l1, l2) ||> foldLeft(acc -> elm -> listRemove(elm, acc))

    ///
    /// Returns `true` if the two sets contain the same elements.
    ///
    /// This is a quadratic time operation.
    ///
    pub def eq(s1: ListSet[t], s2: ListSet[t]): Bool with Eq[t] =
        listEq(extract(s1), extract(s2))

    def listEq(l1: List[t], l2: List[t]): Bool with Eq[t] =
        ListOps.sizeEq(l1, l2) and listIsSubsetOf(l1, l2)

    ///
    /// Returns `true` if `s2` contains all elements of `s1`.
    ///
    /// This is a quadratic time operation.
    ///
    pub def isSubsetOf(s1: ListSet[t], s2: ListSet[t]): Bool with Eq[t] =
        listIsSubsetOf(extract(s1), extract(s2))

    def listIsSubsetOf(l1: List[t], l2: List[t]): Bool with Eq[t] =
        l1 |> List.forAll(elm -> List.memberOf(elm, l2))

    ///
    /// Returns the number of elements in `s`.
    ///
    /// This is a linear time operation.
    ///
    pub def size(s: ListSet[t]): Int32 with Eq[t] =
        s |> extract |> List.size

    /// Returns a string representation of `s`.
    pub def toString(s: ListSet[t]): String with ToString[t], Eq[t] = region rc {
        use StringBuilder.appendString!;
        let sb = StringBuilder.empty(rc);
        appendString!("ListSet(", sb);
        match extract(s) {
            case hd :: tl =>
                appendString!("${hd}", sb);
                tl |> List.forEach(x -> appendString!(", ${x}", sb))
            case Nil => ()
        };
        appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    /// Returns a set of the elements of `iter`.
    pub def collect(iter: Iterator[t, ef, r]): ListSet[t] \ ef + r with Eq[t] =
        (Nil, iter) ||> Iterator.foldLeft(s -> elm -> listInsert(elm, s)) |> ListSet

    ///
    /// The optimized version of `size(s1) == size(s2)`.
    ///
    /// This is a linear time operation.
    ///
    pub def eqSizes(s1: ListSet[t], s2: ListSet[t]): Bool with Eq[t] =
        ListOps.sizeEq(extract(s1), extract(s2))

    ///
    /// Returns the number of elements where `f(x) == true`.
    ///
    /// This is a linear time operation.
    ///
    pub def count(f: t -> Bool, s: ListSet[t]): Int32 with Eq[t] =
        s |> extract |> List.count(f)

    ///
    /// Returns `true` if `s` has an element where `f(x) == true`.
    ///
    /// This is a linear time operation.
    ///
    pub def exists(f: t -> Bool, s: ListSet[t]): Bool with Eq[t] =
        s |> extract |> List.exists(f)

    ///
    /// Returns a set with the elements of `s` where `f(x) == true`.
    ///
    /// This is a linear time operation.
    ///
    pub def filter(f: t -> Bool, s: ListSet[t]): ListSet[t] with Eq[t] =
        s |> extract |> UList.filter(f) |> ListSet

    ///
    /// Returns the set of output elements where `f(x) == Some(_)`.
    ///
    /// This is a quadratic time operation.
    ///
    pub def filterMap(f: t -> Option[tt], s: ListSet[t]): ListSet[tt] with Eq[t], Eq[tt] =
        s |> extract |> listFilterMap(f) |> ListSet

    def listFilterMap(f: t -> Option[tt], l: List[t]): List[tt] with Eq[tt] =
        listFilterMapHelper(f, Nil, l)

    def listFilterMapHelper(f: t -> Option[tt], acc: List[tt], l: List[t]): List[tt] with Eq[tt] = match l {
        case hd :: tl => match f(hd) {
            case Some(v) => listFilterMapHelper(f, listInsert(v, acc), tl)
            case None => listFilterMapHelper(f, acc, tl)
        }
        case Nil => acc
    }

    ///
    /// Returns a set containing the elements of all the sets in `s`.
    ///
    /// This is a quadratic time operation.
    ///
    pub def flatten(s: ListSet[ListSet[t]]): ListSet[t] with Eq[t] =
        (Nil, extract(s)) ||> List.foldLeft(acc -> extract >> listUnion(acc)) |> ListSet

    ///
    /// Returns the combination of all elements in `s`, as defined by the
    /// `Monoid` instance.
    ///
    /// This is a linear time operation.
    ///
    pub def fold(s: ListSet[t]): t with Monoid[t], Eq[t] =
        (Monoid.empty(), extract(s)) ||> List.foldLeft(Monoid.combine)

    ///
    /// Returns the mapped combination of all elements in `s`, as defined by the
    /// `Monoid` instance.
    ///
    /// This is a linear time operation.
    ///
    pub def foldMap(f: t -> tt, s: ListSet[t]): tt with Monoid[tt], Eq[t] =
        (Monoid.empty(), extract(s)) ||> List.foldLeft(acc -> f >> Monoid.combine(acc))

    /// Returns `true` if `f` returns true for all elements in `s`.
    pub def forAll(f: t -> Bool, s: ListSet[t]): Bool with Eq[t] =
        s |> extract |> List.forAll(f)

    ///
    /// Returns `true` if there are no elements in `s`.
    ///
    /// This is a constant time operation.
    ///
    pub def isEmpty(s: ListSet[t]): Bool with Eq[t] =
        s |> extract |> List.isEmpty

    ///
    /// Returns `true` if there is at least one element in `s`.
    ///
    /// This is a constant time operation.
    ///
    pub def nonEmpty(s: ListSet[t]): Bool with Eq[t] =
        s |> extract |> List.nonEmpty

    ///
    /// Returns `true` if `s2` contains all the elements of `s1` and at least
    /// one element not in `s1`.
    ///
    /// This is a quadratic time operation.
    ///
    pub def isProperSubsetOf(s1: ListSet[t], s2: ListSet[t]): Bool with Eq[t] =
        listIsProperSubsetOf(extract(s1), extract(s2))

    def listIsProperSubsetOf(l1: List[t], l2: List[t]): Bool with Eq[t] =
        ListOps.sizeLessThan(l1, l2) and listIsSubsetOf(l1, l2)

    ///
    /// Returns a new set with the mapped elements of `s`.
    ///
    /// This is a quadratic time operation.
    ///
    pub def map(f: t -> tt, s: ListSet[t]): ListSet[tt] with Eq[t], Eq[tt] =
        s |> extract |> listMap(f) |> ListSet

    def listMap(f: t -> tt, l: List[t]): List[tt] with Eq[tt] =
        listMapHelper(f, Nil, l)

    def listMapHelper(f: t -> tt, acc: List[tt], l: List[t]): List[tt] with Eq[tt] = match l {
        case hd :: tl => listMapHelper(f, listInsert(f(hd), acc), tl)
        case Nil => acc
    }

    ///
    /// Returns the largest element in `s`, defined by `cmp`.
    ///
    /// This is a linear time operation.
    ///
    pub def maximumBy(cmp: t -> t -> Comparison, s: ListSet[t]): Option[t] with Eq[t] =
        s |> extract |> List.maximumBy(cmp)

    ///
    /// Returns the smallest element in `s`, defined by `cmp`.
    ///
    /// This is a linear time operation.
    ///
    pub def minimumBy(cmp: t -> t -> Comparison, s: ListSet[t]): Option[t] with Eq[t] =
        s |> extract |> List.minimumBy(cmp)

    ///
    /// Returns `(s1, s2)` where `s1` are the elements of `s` where `f` is
    /// `true`, and `s2` are the elements where `f` is `false`.
    ///
    /// This is a linear time operation.
    ///
    pub def partition(f: t -> Bool, s: ListSet[t]): (ListSet[t], ListSet[t]) with Eq[t] =
        s |> extract |> UList.partition(f) |> (match (x, y) -> (ListSet(x), ListSet(y)))

    ///
    /// Returns a set with the elements between `b` (inclusive) and `e`
    /// (exclusive).
    ///
    /// The above range contains zero elements if `b >= e`.
    ///
    /// This is a linear time operation.
    ///
    pub def range(b: Int32, e: Int32): ListSet[Int32] =
        List.range(b, e) |> ListSet

    ///
    /// Returns a set with the elements of `s` but with `src` replaced by `dst`.
    ///
    /// This is a linear time operation.
    ///
    pub def replace(src: {src = t}, dst: {dst = t}, s: ListSet[t]): ListSet[t] with Eq[t] =
        s |> extract |> listReplace(src, dst) |> ListSet

    def listReplace(src: {src = t}, dst: {dst = t}, l: List[t]): List[t] with Eq[t] =
        match UList.removeOpt(src#src, l) {
            case Some(removed) => listInsert(dst#dst, removed)
            case None => l
        }

    ///
    /// Returns a set build by repeatedly applying `f` to `state` until it
    /// returns `None`. Each `Some` gives the next state and a new element to
    /// add to the set.
    ///
    /// This is a quadratic time operation.
    ///
    pub def unfold(f: state -> Option[(t, state)] \ ef, state: state): ListSet[t] \ ef with Eq[t] =
        listUnfold(f, Nil, state) |> ListSet

    def listUnfold(f: state -> Option[(t, state)] \ ef, acc: List[t], state0: state): List[t] \ ef with Eq[t] =
        match f(state0) {
            case Some(v, state1) => listUnfold(f, listInsert(v, acc), state1)
            case None => acc
        }

    ///
    /// Returns a set build by repeatedly applying `f` until it returns `None`.
    /// Each `Some` adds a new element to the set.
    ///
    /// This is a quadratic time operation.
    ///
    pub def unfoldWithIter(f: Unit -> Option[t] \ ef): ListSet[t] \ ef with Eq[t] =
        listUnfoldWithIter(f, Nil) |> ListSet

    def listUnfoldWithIter(f: Unit -> Option[t] \ ef, acc: List[t]): List[t] \ ef with Eq[t] =
        match f() {
            case Some(v) => listUnfoldWithIter(f, listInsert(v, acc))
            case None => acc
        }

    /// TODO: docs
    pub def sumWith(f: t -> Int32, s: ListSet[t]): Int32 with Eq[t] =
        s |> extract |> List.sumWith(f)

    /// TODO: docs
    pub def subsets(s: ListSet[t]): ListSet[ListSet[t]] with Eq[t] =
        s |> extract |> listSubsets |> List.map(ListSet) |> ListSet

    def listSubsets(l: List[t]): List[List[t]] with Eq[t] =
        (Nil :: Nil, l) ||> List.foldLeft(acc -> elm -> listUnion(acc |> List.map(listInsert(elm)), acc))

    /// TODO: docs
    pub def singleton(x: t): ListSet[t] with Eq[t] =
        ListSet(x :: Nil)

}

mod UnorderedList {

    pub def append(l1: List[t], l2: List[t]): List[t] = match l1 {
        case hd :: tl => append(tl, hd :: l2)
        case Nil => l2
    }

    pub def removeOpt(x: t, l: List[t]): Option[List[t]] with Eq[t] =
        removeOptHelper(x, Nil, l)

    def removeOptHelper(x: t, acc: List[t], l: List[t]): Option[List[t]] with Eq[t] = match l {
        case hd :: tl =>
            if (hd == x) Some(append(acc, tl))
            else removeOptHelper(x, hd :: acc, tl)
        case Nil => None
    }

    pub def partition(f: t -> Bool, l: List[t]): (List[t], List[t]) =
        partitionHelper(f, Nil, Nil, l)

    def partitionHelper(f: t -> Bool, accTrue: List[t], accFalse: List[t], l: List[t]): (List[t], List[t]) = match l {
        case hd :: tl =>
            if (f(hd)) partitionHelper(f, hd :: accTrue, accFalse, tl)
            else partitionHelper(f, accTrue, hd :: accFalse, tl)
        case Nil => (accTrue, accFalse)
    }

    pub def filter(f: t -> Bool, l: List[t]): List[t] with Eq[t] =
        filterHelper(f, Nil, l)

    def filterHelper(f: t -> Bool, acc: List[t], l: List[t]): List[t] with Eq[t] = match l {
        case hd :: tl =>
            if (f(hd)) filterHelper(f, hd :: acc, tl)
            else filterHelper(f, acc, tl)
        case Nil => acc
    }

}

mod ListOps {

    pub def sizeLessThan(l1: List[t], l2: List[t]): Bool = match (l1, l2) {
        case (_ :: tl1, _ :: tl2) => sizeLessThan(tl1, tl2)
        case (Nil, _ :: _) => true
        case (_, Nil) => false
    }

    pub def sizeEq(l1: List[t], l2: List[t]): Bool = match (l1, l2) {
        case (_ :: next1, _ :: next2) => sizeEq(next1, next2)
        case (Nil, Nil) => true
        case _ => false
    }

}

}
