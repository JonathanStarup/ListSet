mod JonathanStarup.Test.Property.ListSetGenerator {
    use Collectable.collect
    use JonathanStarup.ListSet
    use Result.Err
    use Result.Ok

    /// Returns a random set of the given length.
    pub def fromLength(length: Int32): c \ Collectable.Aef[c] + Random with Collectable[c] where Collectable.Elm[c] ~ Int32 =
        region local {
            fromLengthIterator(local, length) |> collect
        }

    def fromLengthIterator(rc: Region[r], length: Int32): Iterator[Int32, r + Random, r] \ r =
        use Ref.{fresh, get, transform};
        let runningLength = fresh(rc, length);
        Iterator.iterate(rc, () -> {
            if (get(runningLength) <= 0) None
            else {
                runningLength |> transform(Sub.sub(1));
                Some(Random.randomInt32())
            }
        })

    ///
    /// Returns an iterator of `amount` sets of increasing lengths.
    ///
    /// The iterator always have at least three sets of lengths 0, 1, and 2.
    ///
    pub def randomIterator(
        rc: Region[r], amount: Int32
    ): Iterator[c, r + Random + aef, r] \ r
    with Collectable[c] where Collectable.Elm[c] ~ Int32, Collectable.Aef[c] ~ aef = {
        use Ref.{fresh, get, transform};
        let runningAmount = fresh(rc, amount);
        let runningLen = fresh(rc, 2);
        let iter = Iterator.iterate(rc)(() -> {
            if (get(runningAmount) <= 0) None
            else {
                runningAmount |> transform(x -> x - 1);
                // [1, 5]
                let lenInc = nextNatWithMax(5) + 1;
                runningLen |> transform(Add.add(lenInc));
                Some(fromLength(get(runningLen)))
            }
        });
        // We cant use fromLength now, so we do it lazily.
        def consThing(k, it) = {
            let justK = Iterator.singleton(rc, () -> checked_ecast(fromLength(k))) |> Iterator.map(f -> f());
            Iterator.append(justK, it)
        };
        iter |>
            consThing(2) |>
            consThing(1) |>
            consThing(0)
    }

    def nextNatWithMax(max: Int32): Int32 \ Random = {
        Int32.modulo(Random.randomInt32(), Int32.max(max, 1))
    }
}
