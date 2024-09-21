mod JonathanStarup.Test.Property.ListSetGenerator {
    use Collectable.collect
    use JonathanStarup.ListSet
    use Random.nextInt32
    use Random.nextNatWithMax
    use Result.Err
    use Result.Ok

    /// Returns a random set of the given length.
    pub def fromLength(r: Random, length: Int32): c \ IO + Collectable.Aef[c] with Collectable[c] where Collectable.Elm[c] ~ Int32 =
        region local {
            fromLengthIterator(local, r, length) |> collect
        }

    def fromLengthIterator(rc: Region[r], r: Random, length: Int32): Iterator[Int32, r + IO, r] \ r =
        use Ref.{fresh, get, transform};
        let runningLength = fresh(rc, length);
        Iterator.iterate(rc, () -> {
            if (get(runningLength) <= 0) None
            else {
                runningLength |> transform(Sub.sub(1));
                Some(nextInt32(r))
            }
        })

    ///
    /// Returns an iterator of `amount` sets of increasing lengths.
    ///
    /// The iterator always have at least three sets of lengths 0, 1, and 2.
    ///
    pub def randomIterator(
        rc: Region[r], r: Random, amount: Int32
    ): Iterator[c, r + IO + aef, r] \ r + IO + aef
    with Collectable[c] where Collectable.Elm[c] ~ Int32, Collectable.Aef[c] ~ aef = {
        use Ref.{fresh, get, transform};
        let runningAmount = fresh(rc, amount);
        let runningLen = fresh(rc, 2);
        let iter = Iterator.iterate(rc)(() -> {
            if (get(runningAmount) <= 0) None
            else {
                runningAmount |> transform(x -> x - 1);
                // [1, 5]
                let lenInc = nextNatWithMax(r, 5) + 1;
                runningLen |> transform(Add.add(lenInc));
                Some(fromLength(r, get(runningLen)))
            }
        });
        iter |>
            Iterator.cons(fromLength(r, 2)) |>
            Iterator.cons(fromLength(r, 1)) |>
            Iterator.cons(fromLength(r, 0))
    }
}
