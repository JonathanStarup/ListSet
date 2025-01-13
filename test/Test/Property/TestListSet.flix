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

    def runCrash(f: Unit -> Unit \ ef): Bool \ ef + IO - Abort =
        run {f(); true} with handler Abort {
            def abort(msg, _) = {
                println("Test failed: ${msg}");
                false
            }
        }

    def _assertEq(x: t, y: t): Unit \ Abort with Eq[t], ToString[t] = {
        if (x == y) ()
        else abort("${x} != ${y}")
    }

    def assertExpect(expect: {expect = t}, actual: t): Unit \ Abort with Eq[t], ToString[t] = {
        if (expect#expect == actual) ()
        else abort("found ${actual} but expected ${expect#expect}")
    }

    def runTest(
        tests: Int32, seed: Int64, prop: c -> Unit \ Abort
    ): Bool \ IO + (Collectable.Aef[c] - Abort - Random)
    with Collectable[c] where Collectable.Elm[c] ~ Int32 = region local {
        let f = () ->
            ListSetGenerator.randomIterator(local, tests) |>
                Iterator.forEach(prop);
        let g = () -> runCrash(f);
        Random.runWithSeed(seed, g)
    }

    @test
    pub def testEmpty(): Bool \ IO = {
        def prop(l) = {
            let expectedLength = List.isEmpty(l);
            let s = ListSet.fromIterable(l);
            ListSet.isEmpty(s) |> assertExpect(expect = expectedLength)
        };
        prop |> runTest(100, 8168590i64)
    }

    @test
    pub def testSize(): Bool \ IO = {
        def prop(l) = {
            let expectedLength = List.length(l);
            let s = ListSet.fromIterable(l);
            ListSet.size(s) |> assertExpect(expect = expectedLength)
        };
        prop |> runTest(10_000, 578956838i64)
    }

    @test
    pub def testInsertRedundant(): Bool \ IO = {
        def prop(l) = {
            let s1 = ListSet.fromIterable(l);
            let s2 = l |>
                List.head |>
                Option.map(hd -> ListSet.insert(hd, s1)) |>
                Option.getWithDefault(s1);
            assertExpect(expect = s1, s2)
        };
        prop |> runTest(1_000, -6859625i64)
    }

}
