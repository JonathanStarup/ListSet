//
// -- INVARIANT --
// The list of ListSet never contain duplicates at any point.
//

/// A linear representation of a set, requiring only `Eq[t]`.
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
///
mod ListSet {
    use Eq.{eq, neq}
    use Foldable.foldLeft

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
        // TODO OPTIMIZATION: this could stop early
        l |> List.filter(neq(x))

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
        l1 |> List.filter(elm -> List.memberOf(elm, l2))


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
        listEqSizes(l1, l2) and listIsSubsetOf(l1, l2)

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
        (empty(), iter) ||> Iterator.foldLeft(s -> elm -> insert(elm, s))

    ///
    /// The optimized version of `size(s1) == size(s2)`.
    ///
    /// This is a linear time operation.
    ///
    pub def eqSizes(s1: ListSet[t], s2: ListSet[t]): Bool with Eq[t] =
        listEqSizes(extract(s1), extract(s2))

    def listEqSizes(l1: List[t], l2: List[t]): Bool = match (l1, l2) {
        case (_ :: next1, _ :: next2) => listEqSizes(next1, next2)
        case (Nil, Nil) => true
        case _ => false
    }

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
        s |> extract |> List.filter(f) |> ListSet

    ///
    /// Returns the set of output elements where `f(x) == Some(_)`.
    ///
    /// This is a quadratic time operation.
    ///
    pub def filterMap(f: t -> Option[tt], s: ListSet[t]): ListSet[tt] with Eq[t], Eq[tt] =
        s |> extract |> listFilterMap(f) |> ListSet

    def listFilterMap(f: t -> Option[tt], l: List[t]): List[tt] with Eq[t], Eq[tt] =
        l |> List.filterMap(f) |> List.foldLeft(acc -> elm -> listInsert(elm, acc), Nil)

}
