//
// Copyright 2024 Jonathan Lindegaard Starup
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

mod JonathanStarup.Test.Property.TestListSet {
    use JonathanStarup.ListSet
    use JonathanStarup.Test.Property.ListSetGenerator
    use Abort.abort
    use Formattable.format

    def runTest(
        tests: Int32, seed: Int64, prop: c -> Unit \ Assert
    ): Unit \ Assert + (Collectable.Aef[c] - Random)
    with Collectable[c] where Collectable.Elm[c] ~ Int32 = region local {
        let f = () ->
            ListSetGenerator.randomIterator(local, tests) |>
                Iterator.forEach(prop);
        Random.runWithSeed(seed, f)
    }

    @Test
    pub def testEmpty(): Unit \ Assert = {
        def prop(l) = {
            let expectedLength = List.isEmpty(l);
            let s = ListSet.fromIterable(l);
            ListSet.isEmpty(s) |> Assert.assertEq(expected = expectedLength)
        };
        prop |> runTest(100, 8168590i64)
    }

    @Test
    pub def testSize(): Unit \ Assert = {
        def prop(l) = {
            let expectedLength = List.length(l);
            let s = ListSet.fromIterable(l);
            ListSet.size(s) |> Assert.assertEq(expected = expectedLength)
        };
        prop |> runTest(10_000, 578956838i64)
    }

    @Test
    pub def testInsertRedundant(): Unit \ Assert = {
        def prop(l) = {
            let s1 = ListSet.fromIterable(l);
            let s2 = l |>
                List.head |>
                Option.map(hd -> ListSet.insert(hd, s1)) |>
                Option.getWithDefault(s1);
            Assert.assertEq(expected = s1, s2)
        };
        prop |> runTest(1_000, -6859625i64)
    }

}
