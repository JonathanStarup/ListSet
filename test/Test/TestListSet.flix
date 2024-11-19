//
// Copyright 2017 Liam Palmer
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

//
// This file is copied from the ListSet tests in github.com/flix/flix/
//

mod JonathanStarup.Test.TestListSet {

    use Hash.hash
    use JonathanStarup.ListSet
    use ToString.toString

    /////////////////////////////////////////////////////////////////////////////
    // Collectable.collect                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def collect01(): Bool = region rc {
        Collectable.collect((Iterator.empty(rc): Iterator[Int32, rc, rc])) == ListSet.empty()
    }

    @test
    def collect02(): Bool = region rc {
        Collectable.collect(Iterator.singleton(rc, 1)) == ListSet.singleton(1)
    }

    @test
    def collect03(): Bool = region rc {
        Collectable.collect(Iterator.range(rc, 1, 3)) == ListSet.fromIterable(List#{1, 2})
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def size01(): Bool = ListSet.size((ListSet.empty(): ListSet[Int32])) == 0

    @test
    def size02(): Bool = ListSet.size(ListSet.singleton(1)) == 1

    @test
    def size03(): Bool = ListSet.size(ListSet.fromIterable(List#{1, 2})) == 2

    @test
    def size04(): Bool = ListSet.size(ListSet.fromIterable(List#{1, 2, 3})) == 3

    @test
    def size05(): Bool = ListSet.size(ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14})) == 13

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def empty01(): Bool = (ListSet.empty(): ListSet[Unit]) == ListSet.empty()

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def singleton01(): Bool = ListSet.singleton(1) == ListSet.singleton(1)

    @test
    def singleton02(): Bool = ListSet.singleton(2) == ListSet.singleton(2)

    @test
    def singleton03(): Bool = ListSet.singleton(true) == ListSet.fromIterable(List#{true})

    @test
    def singleton04(): Bool = ListSet.singleton(false) == ListSet.fromIterable(List#{false})

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def insert01(): Bool = ListSet.insert(1, ListSet.empty()) == ListSet.singleton(1)

    @test
    def insert02(): Bool = ListSet.insert(1, ListSet.singleton(1)) == ListSet.singleton(1)

    @test
    def insert03(): Bool = ListSet.insert(1, ListSet.singleton(2)) == ListSet.fromIterable(List#{1, 2})

    @test
    def insert04(): Bool = ListSet.insert(1, ListSet.fromIterable(List#{2, 3})) == ListSet.fromIterable(List#{1, 2, 3})

    @test
    def insert05(): Bool = ListSet.insert(1, ListSet.fromIterable(List#{1, 2, 3})) == ListSet.fromIterable(List#{1, 2, 3})

    @test
    def insert06(): Bool = ListSet.insert(1, ListSet.fromIterable(List#{2, 1, 3})) == ListSet.fromIterable(List#{2, 1, 3})

    @test
    def insert07(): Bool = ListSet.insert(1, ListSet.fromIterable(List#{2, 1, 3, 7, 8, 9, 10})) == ListSet.fromIterable(List#{2, 1, 3, 7, 8, 9, 10})

    @test
    def insert08(): Bool = ListSet.insert(11, ListSet.fromIterable(List#{2, 1, 3, 7, 8, 9, 10, 35})) == ListSet.fromIterable(List#{11, 2, 1, 3, 7, 8, 9, 10, 35})

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def remove01(): Bool = ListSet.remove(1, ListSet.empty()) == ListSet.empty()

    @test
    def remove02(): Bool = ListSet.remove(1, ListSet.singleton(1)) == ListSet.empty()

    @test
    def remove03(): Bool = ListSet.remove(1, ListSet.singleton(2)) == ListSet.singleton(2)

    @test
    def remove04(): Bool = ListSet.remove(2, ListSet.fromIterable(List#{2, 3})) == ListSet.singleton(3)

    @test
    def remove05(): Bool = ListSet.remove(3, ListSet.fromIterable(List#{2, 3})) == ListSet.singleton(2)

    @test
    def remove06(): Bool = ListSet.remove(1, ListSet.fromIterable(List#{2, 3})) == ListSet.fromIterable(List#{2, 3})

    @test
    def remove07(): Bool = ListSet.remove(0, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})) == ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})

    @test
    def remove08(): Bool = ListSet.remove(1, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})) == ListSet.fromIterable(List#{2, 3, 4, 5, 6, 7})

    @test
    def remove09(): Bool = ListSet.remove(2, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})) == ListSet.fromIterable(List#{1, 3, 4, 5, 6, 7})

    @test
    def remove10(): Bool = ListSet.remove(6, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})) == ListSet.fromIterable(List#{1, 2, 3, 4, 5, 7})

    @test
    def remove11(): Bool = ListSet.remove(7, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})) == ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6})

    @test
    def remove12(): Bool = ListSet.remove(8, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})) == ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isEmpty01(): Bool = ListSet.isEmpty((ListSet.empty(): ListSet[Unit])) == true

    @test
    def isEmpty02(): Bool = ListSet.isEmpty(ListSet.singleton(1)) == false

    @test
    def isEmpty03(): Bool = ListSet.isEmpty(ListSet.fromIterable(List#{1, 2})) == false

    @test
    def isEmpty04(): Bool = ListSet.isEmpty(ListSet.fromIterable(List#{1, 2, 3})) == false

    @test
    def isEmpty05(): Bool = ListSet.isEmpty(ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8})) == false

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def nonEmpty01(): Bool = ListSet.nonEmpty((ListSet.empty(): ListSet[Unit])) == false

    @test
    def nonEmpty02(): Bool = ListSet.nonEmpty(ListSet.singleton(1)) == true

    @test
    def nonEmpty03(): Bool = ListSet.nonEmpty(ListSet.fromIterable(List#{1, 2})) == true

    @test
    def nonEmpty04(): Bool = ListSet.nonEmpty(ListSet.fromIterable(List#{1, 2, 3})) == true

    @test
    def nonEmpty05(): Bool = ListSet.nonEmpty(ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8})) == true

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def memberOf01(): Bool = ListSet.memberOf(1, ListSet.empty()) == false

    @test
    def memberOf02(): Bool = ListSet.memberOf(1, ListSet.singleton(2)) == false

    @test
    def memberOf03(): Bool = ListSet.memberOf(1, ListSet.singleton(1)) == true

    @test
    def memberOf04(): Bool = ListSet.memberOf(1, ListSet.fromIterable(List#{1, 2})) == true

    @test
    def memberOf05(): Bool = ListSet.memberOf(2, ListSet.fromIterable(List#{1, 2})) == true

    @test
    def memberOf06(): Bool = ListSet.memberOf(0, ListSet.fromIterable(List#{1, 2})) == false

    @test
    def memberOf07(): Bool = ListSet.memberOf(3, ListSet.fromIterable(List#{1, 2})) == false

    @test
    def memberOf08(): Bool = ListSet.memberOf(0, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

    @test
    def memberOf09(): Bool = ListSet.memberOf(1, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

    @test
    def memberOf10(): Bool = ListSet.memberOf(2, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

    @test
    def memberOf11(): Bool = ListSet.memberOf(10, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

    @test
    def memberOf12(): Bool = ListSet.memberOf(12, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

    /////////////////////////////////////////////////////////////////////////////
    // isSubsetOf                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isSubsetOf01(): Bool = ListSet.isSubsetOf((ListSet.empty(): ListSet[Unit]), ListSet.empty()) == true

    @test
    def isSubsetOf02(): Bool = ListSet.isSubsetOf(ListSet.empty(), ListSet.singleton(1)) == true

    @test
    def isSubsetOf03(): Bool = ListSet.isSubsetOf(ListSet.singleton(1), ListSet.singleton(1)) == true

    @test
    def isSubsetOf04(): Bool = ListSet.isSubsetOf(ListSet.singleton(1), ListSet.singleton(2)) == false

    @test
    def isSubsetOf05(): Bool = ListSet.isSubsetOf(ListSet.empty(), ListSet.fromIterable(List#{1, 2})) == true

    @test
    def isSubsetOf06(): Bool = ListSet.isSubsetOf(ListSet.singleton(1), ListSet.fromIterable(List#{1, 2})) == true

    @test
    def isSubsetOf07(): Bool = ListSet.isSubsetOf(ListSet.singleton(2), ListSet.fromIterable(List#{1, 2})) == true

    @test
    def isSubsetOf08(): Bool = ListSet.isSubsetOf(ListSet.singleton(3), ListSet.fromIterable(List#{1, 2})) == false

    @test
    def isSubsetOf09(): Bool = ListSet.isSubsetOf(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{1, 2})) == true

    @test
    def isSubsetOf10(): Bool = ListSet.isSubsetOf(ListSet.fromIterable(List#{2, 1}), ListSet.fromIterable(List#{1, 2})) == true

    @test
    def isSubsetOf11(): Bool = ListSet.isSubsetOf(ListSet.fromIterable(List#{3, 1}), ListSet.fromIterable(List#{1, 2})) == false

    @test
    def isSubsetOf12(): Bool = ListSet.isSubsetOf(ListSet.fromIterable(List#{1, 2, 3}), ListSet.fromIterable(List#{1, 2})) == false

    @test
    def isSubsetOf13(): Bool = ListSet.isSubsetOf(ListSet.singleton(10), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

    @test
    def isSubsetOf14(): Bool = ListSet.isSubsetOf(ListSet.fromIterable(List#{9, 1}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

    @test
    def isSubsetOf15(): Bool = ListSet.isSubsetOf(ListSet.fromIterable(List#{6, 5, 8}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

    @test
    def isSubsetOf16(): Bool = ListSet.isSubsetOf(ListSet.fromIterable(List#{10, 2, 3, 6, -1, 4}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

    /////////////////////////////////////////////////////////////////////////////
    // isProperSubsetOf                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isProperSubsetOf01(): Bool = ListSet.isProperSubsetOf((ListSet.empty(): ListSet[Unit]), ListSet.empty()) == false

    @test
    def isProperSubsetOf02(): Bool = ListSet.isProperSubsetOf(ListSet.empty(), ListSet.singleton(1)) == true

    @test
    def isProperSubsetOf03(): Bool = ListSet.isProperSubsetOf(ListSet.singleton(1), ListSet.singleton(1)) == false

    @test
    def isProperSubsetOf04(): Bool = ListSet.isProperSubsetOf(ListSet.singleton(1), ListSet.singleton(2)) == false

    @test
    def isProperSubsetOf05(): Bool = ListSet.isProperSubsetOf(ListSet.empty(), ListSet.fromIterable(List#{1, 2})) == true

    @test
    def isProperSubsetOf06(): Bool = ListSet.isProperSubsetOf(ListSet.singleton(1), ListSet.fromIterable(List#{1, 2})) == true

    @test
    def isProperSubsetOf07(): Bool = ListSet.isProperSubsetOf(ListSet.singleton(2), ListSet.fromIterable(List#{1, 2})) == true

    @test
    def isProperSubsetOf08(): Bool = ListSet.isProperSubsetOf(ListSet.singleton(3), ListSet.fromIterable(List#{1, 2})) == false

    @test
    def isProperSubsetOf09(): Bool = ListSet.isProperSubsetOf(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{1, 2})) == false

    @test
    def isProperSubsetOf10(): Bool = ListSet.isProperSubsetOf(ListSet.fromIterable(List#{2, 1}), ListSet.fromIterable(List#{1, 2})) == false

    @test
    def isProperSubsetOf11(): Bool = ListSet.isProperSubsetOf(ListSet.fromIterable(List#{3, 1}), ListSet.fromIterable(List#{1, 2})) == false

    @test
    def isProperSubsetOf12(): Bool = ListSet.isProperSubsetOf(ListSet.fromIterable(List#{1, 2, 3}), ListSet.fromIterable(List#{1, 2})) == false

    @test
    def isProperSubsetOf13(): Bool = ListSet.isProperSubsetOf(ListSet.singleton(10), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

    @test
    def isProperSubsetOf14(): Bool = ListSet.isProperSubsetOf(ListSet.fromIterable(List#{9, 1}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

    @test
    def isProperSubsetOf15(): Bool = ListSet.isProperSubsetOf(ListSet.fromIterable(List#{6, 5, 8}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

    @test
    def isProperSubsetOf16(): Bool = ListSet.isProperSubsetOf(ListSet.fromIterable(List#{10, 2, 3, 6, -1, 4}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

    @test
    def isProperSubsetOf17(): Bool = ListSet.isProperSubsetOf(ListSet.fromIterable(List#{10, 2, 3, 6, 1, 4, 5, 9, 8, 7}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def count01(): Bool = ListSet.count(i -> i > 3, ListSet.empty()) == 0

    @test
    def count02(): Bool = ListSet.count(i -> i > 3, ListSet.singleton(1)) == 0

    @test
    def count03(): Bool = ListSet.count(i -> i > 3, ListSet.singleton(4)) == 1

    @test
    def count04(): Bool = ListSet.count(i -> i > 3, ListSet.fromIterable(List#{2, 1})) == 0

    @test
    def count05(): Bool = ListSet.count(i -> i > 3, ListSet.fromIterable(List#{8, 1})) == 1

    @test
    def count06(): Bool = ListSet.count(i -> i > 3, ListSet.fromIterable(List#{1, 8})) == 1

    @test
    def count07(): Bool = ListSet.count(i -> i > 3, ListSet.fromIterable(List#{7, 6})) == 2


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        ListSet.empty() |> ListSet.sumWith(x -> x + 1) == 0

    @test
    def sumWith02(): Bool =
        ListSet.singleton(1) |> ListSet.sumWith(x -> x + 1) == 2

    @test
    def sumWith03(): Bool =
        ListSet.fromIterable(List#{1, 2, 3}) |> ListSet.sumWith(x -> x + 1) == 9

    @test
    def sumWith04(): Bool =
        ListSet.fromIterable(List#{1, 2, 3, -3}) |> ListSet.sumWith(x -> x + 1) == 7

    @test
    def sumWith05(): Bool =
        ListSet.fromIterable(List#{-1, -2, -3, -4}) |> ListSet.sumWith(x -> x + 1) == -6

    @test
    def sumWith06(): Bool =
        ListSet.fromIterable(List#{10, -10}) |> ListSet.sumWith(x -> x + 1) == 2


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def flatten01(): Bool = (ListSet.flatten(ListSet.empty()): ListSet[Unit]) == ListSet.empty()

    @test
    def flatten02(): Bool = ListSet.flatten((ListSet.fromIterable(List#{ListSet.empty()}): ListSet[ListSet[Unit]])) == ListSet.empty()

    @test
    def flatten03(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1)})) == ListSet.singleton(1)

    @test
    def flatten04(): Bool = ListSet.flatten(ListSet.singleton(ListSet.fromIterable(List#{1, 2}))) == ListSet.fromIterable(List#{1, 2})

    @test
    def flatten05(): Bool = (ListSet.flatten(ListSet.fromIterable(List#{ListSet.empty(), ListSet.empty()})): ListSet[Unit]) == ListSet.empty()

    @test
    def flatten06(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.empty()})) == ListSet.singleton(1)

    @test
    def flatten07(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.empty(), ListSet.singleton(1)})) == ListSet.singleton(1)

    @test
    def flatten08(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.singleton(2)})) == ListSet.fromIterable(List#{1, 2})

    @test
    def flatten09(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.singleton(1)})) == ListSet.singleton(1)

    @test
    def flatten10(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 4, 5})})) == ListSet.fromIterable(List#{1, 2, 3, 4, 5})

    @test
    def flatten11(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 4, 3})})) == ListSet.fromIterable(List#{1, 2, 3, 4})

    @test
    def flatten12(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 4, 1})})) == ListSet.fromIterable(List#{1, 2, 3, 4})

    @test
    def flatten13(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.fromIterable(List#{2, 3}), ListSet.singleton(4)})) == ListSet.fromIterable(List#{1, 2, 3, 4})

    @test
    def flatten14(): Bool = ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.fromIterable(List#{2, 3}), ListSet.singleton(1)})) == ListSet.fromIterable(List#{1, 2, 3})

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def exists01(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.empty()) == false

    @test
    def exists02(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(5)) == false

    @test
    def exists03(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(7)) == true

    @test
    def exists04(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(15)) == true

    @test
    def exists05(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{1, 44})) == false

    @test
    def exists06(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, 71})) == true

    @test
    def exists07(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{71, 12})) == true

    @test
    def exists08(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{71, 79})) == true

    @test
    def exists09(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, -1, -14, -2, 84, 113})) == false

    @test
    def exists10(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, -1, 31, -14, -2, 84, 111})) == true

    @test
    def exists11(): Bool = ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, -1, -14, -2, 84, 111, 38})) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def forAll01(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.empty()) == true

    @test
    def forAll02(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(5)) == false

    @test
    def forAll03(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(7)) == true

    @test
    def forAll04(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(15)) == true

    @test
    def forAll05(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{1, 44})) == false

    @test
    def forAll06(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, 71})) == false

    @test
    def forAll07(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{71, 12})) == false

    @test
    def forAll08(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{71, 79})) == true

    @test
    def forAll09(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{7, 15, 23, 84, 111})) == false

    @test
    def forAll10(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{7, 15, 23, 111, 3})) == false

    @test
    def forAll11(): Bool = ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{7, 15, 23, 119, 111})) == true

    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def union01(): Bool = ListSet.union((ListSet.empty(): ListSet[Unit]), ListSet.empty()) == ListSet.empty()

    @test
    def union02(): Bool = ListSet.union(ListSet.singleton(1), ListSet.empty()) == ListSet.singleton(1)

    @test
    def union03(): Bool = ListSet.union(ListSet.empty(), ListSet.singleton(2)) == ListSet.singleton(2)

    @test
    def union04(): Bool = ListSet.union(ListSet.singleton(1), ListSet.singleton(1)) == ListSet.singleton(1)

    @test
    def union05(): Bool = ListSet.union(ListSet.singleton(1), ListSet.fromIterable(List#{-1})) == ListSet.fromIterable(List#{1, -1})

    @test
    def union06(): Bool = ListSet.union(ListSet.empty(), ListSet.fromIterable(List#{-1, 9})) == ListSet.fromIterable(List#{-1, 9})

    @test
    def union07(): Bool = ListSet.union(ListSet.singleton(9), ListSet.fromIterable(List#{-1, 9})) == ListSet.fromIterable(List#{-1, 9})

    @test
    def union08(): Bool = ListSet.union(ListSet.singleton(4), ListSet.fromIterable(List#{-1, 9})) == ListSet.fromIterable(List#{4, -1, 9})

    @test
    def union09(): Bool = ListSet.union(ListSet.fromIterable(List#{9, -1}), ListSet.fromIterable(List#{-1, 9})) == ListSet.fromIterable(List#{-1, 9})

    @test
    def union10(): Bool = ListSet.union(ListSet.fromIterable(List#{9, 5}), ListSet.fromIterable(List#{-1, 9})) == ListSet.fromIterable(List#{5, -1, 9})

    @test
    def union11(): Bool = ListSet.union(ListSet.fromIterable(List#{6, 5}), ListSet.fromIterable(List#{-1, 9})) == ListSet.fromIterable(List#{6, 5, -1, 9})

    @test
    def union12(): Bool = ListSet.union(ListSet.fromIterable(List#{6, -99}), ListSet.fromIterable(List#{6, 5, -1, 9, 43, 7, 8, -99})) == ListSet.fromIterable(List#{6, 5, -1, 9, 43, 7, 8, -99})

    @test
    def union13(): Bool = ListSet.union(ListSet.fromIterable(List#{6, -99, -1, 5, 22}), ListSet.fromIterable(List#{6, 5, -1, 9, 43, 7, 8, -99})) == ListSet.fromIterable(List#{22, 6, 5, -1, 9, 43, 7, 8, -99})

    @test
    def union14(): Bool = ListSet.union(ListSet.fromIterable(List#{-2, -3, -4}), ListSet.fromIterable(List#{6, 5, -1, 9, 43, 7, 8, -99})) == ListSet.fromIterable(List#{-2, -3, -4, 6, 5, -1, 9, 43, 7, 8, -99})

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def intersection01(): Bool = ListSet.intersection((ListSet.empty(): ListSet[Unit]), ListSet.empty()) == ListSet.empty()

    @test
    def intersection02(): Bool = ListSet.intersection(ListSet.singleton(1), ListSet.empty()) == ListSet.empty()

    @test
    def intersection03(): Bool = ListSet.intersection(ListSet.empty(), ListSet.singleton(2)) == ListSet.empty()

    @test
    def intersection04(): Bool = ListSet.intersection(ListSet.singleton(1), ListSet.singleton(2)) == ListSet.empty()

    @test
    def intersection05(): Bool = ListSet.intersection(ListSet.singleton(1), ListSet.singleton(1)) == ListSet.singleton(1)

    @test
    def intersection06(): Bool = ListSet.intersection(ListSet.empty(), ListSet.fromIterable(List#{1, 2})) == ListSet.empty()

    @test
    def intersection07(): Bool = ListSet.intersection(ListSet.fromIterable(List#{1, 2}), ListSet.empty()) == ListSet.empty()

    @test
    def intersection08(): Bool = ListSet.intersection(ListSet.singleton(2), ListSet.fromIterable(List#{1, 2})) == ListSet.singleton(2)

    @test
    def intersection09(): Bool = ListSet.intersection(ListSet.singleton(1), ListSet.fromIterable(List#{1, 2})) == ListSet.singleton(1)

    @test
    def intersection10(): Bool = ListSet.intersection(ListSet.fromIterable(List#{2, 1}), ListSet.fromIterable(List#{1, 2})) == ListSet.fromIterable(List#{2, 1})

    @test
    def intersection11(): Bool = ListSet.intersection(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{1, 2})) == ListSet.fromIterable(List#{1, 2})

    @test
    def intersection12(): Bool = ListSet.intersection(ListSet.fromIterable(List#{3, 2}), ListSet.fromIterable(List#{1, 2})) == ListSet.singleton(2)

    @test
    def intersection13(): Bool = ListSet.intersection(ListSet.fromIterable(List#{3, 55}), ListSet.fromIterable(List#{1, 2})) == ListSet.empty()

    @test
    def intersection14(): Bool = ListSet.intersection(ListSet.fromIterable(List#{3, 55, 11, 87, 22, 34, -87, 23}), ListSet.fromIterable(List#{1, 2, 84, -87, 87, 3, 44})) == ListSet.fromIterable(List#{3, 87, -87})

    @test
    def intersection15(): Bool = ListSet.intersection(ListSet.fromIterable(List#{3, 55, 11, 87, 22, 34, -87, 23}), ListSet.fromIterable(List#{23, 1, 2, 84, 87, 3})) == ListSet.fromIterable(List#{3, 87, 23})

    /////////////////////////////////////////////////////////////////////////////
    // difference                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def difference01(): Bool = ListSet.difference((ListSet.empty(): ListSet[Unit]), ListSet.empty()) == ListSet.empty()

    @test
    def difference02(): Bool = ListSet.difference(ListSet.empty(), ListSet.singleton(2)) == ListSet.empty()

    @test
    def difference03(): Bool = ListSet.difference(ListSet.singleton(1), ListSet.empty()) == ListSet.singleton(1)

    @test
    def difference04(): Bool = ListSet.difference(ListSet.singleton(1), ListSet.singleton(2)) == ListSet.singleton(1)

    @test
    def difference05(): Bool = ListSet.difference(ListSet.singleton(1), ListSet.singleton(1)) == ListSet.empty()

    @test
    def difference06(): Bool = ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.empty()) == ListSet.fromIterable(List#{1, 2})

    @test
    def difference07(): Bool = ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.singleton(1)) == ListSet.singleton(2)

    @test
    def difference08(): Bool = ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.singleton(2)) == ListSet.singleton(1)

    @test
    def difference09(): Bool = ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{8, 2, 4})) == ListSet.singleton(1)

    @test
    def difference10(): Bool = ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 1, 2, 4})) == ListSet.empty()

    @test
    def difference11(): Bool = ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 11, 21, 4})) == ListSet.fromIterable(List#{1, 2})

    @test
    def difference12(): Bool = ListSet.difference(ListSet.fromIterable(List#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}), ListSet.fromIterable(List#{-1, 92, 4, 5, 1, 2, 86})) == ListSet.fromIterable(List#{87, 6, 111})

    @test
    def difference13(): Bool = ListSet.difference(ListSet.fromIterable(List#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}), ListSet.fromIterable(List#{-1, 92, 4, 5, 1, 2, 86, 99, 6})) == ListSet.fromIterable(List#{87, 111})

    /////////////////////////////////////////////////////////////////////////////
    // subsets                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def subsets01(): Bool = ListSet.subsets((ListSet.empty(): ListSet[Unit])) == ListSet.fromIterable(List#{ListSet.empty()})

    @test
    def subsets02(): Bool = ListSet.subsets(ListSet.singleton(1)) == ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.empty()})

    @test
    def subsets03(): Bool = ListSet.subsets(ListSet.fromIterable(List#{1, 2})) == ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.singleton(1), ListSet.singleton(2), ListSet.empty()})

    @test
    def subsets04(): Bool = ListSet.subsets(ListSet.fromIterable(List#{1, 2, 3})) == ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2, 3}), ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{1, 3}), ListSet.singleton(1), ListSet.fromIterable(List#{2, 3}), ListSet.singleton(2), ListSet.singleton(3), ListSet.empty()})

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def filter01(): Bool = ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.empty()) == ListSet.empty()

    @test
    def filter02(): Bool = ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(1)) == ListSet.empty()

    @test
    def filter03(): Bool = ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(2)) == ListSet.singleton(2)

    @test
    def filter04(): Bool = ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, 3})) == ListSet.empty()

    @test
    def filter05(): Bool = ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{8, 3})) == ListSet.singleton(8)

    @test
    def filter06(): Bool = ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{-1, 32})) == ListSet.singleton(32)

    @test
    def filter07(): Bool = ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, 34})) == ListSet.fromIterable(List#{12, 34})

    @test
    def filter08(): Bool = ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{-33, -1, 12, 1, 34, 88, 7, 77, 31})) == ListSet.fromIterable(List#{12, 34, 88})

    @test
    def filter09(): Bool = ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{-33, -1, 12, 1, 34, 88, 7, 77, 31, 7, -92, 841})) == ListSet.fromIterable(List#{12, 34, 88, -92})

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def map01(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.empty()) == ListSet.empty()

    @test
    def map02(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(1)) == ListSet.fromIterable(List#{false})

    @test
    def map03(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(2)) == ListSet.fromIterable(List#{true})

    @test
    def map04(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, -1})) == ListSet.fromIterable(List#{false})

    @test
    def map05(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, -12})) == ListSet.fromIterable(List#{false, true})

    @test
    def map06(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{16, -1})) == ListSet.fromIterable(List#{true, false})

    @test
    def map07(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12})) == ListSet.fromIterable(List#{true})

    @test
    def map08(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12})) == ListSet.fromIterable(List#{true})

    @test
    def map09(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12, 1, 14})) == ListSet.fromIterable(List#{false, true})

    @test
    def map10(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12, 1, 14, 7, 88, -91})) == ListSet.fromIterable(List#{true, false})

    @test
    def map11(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12, 122, 14})) == ListSet.fromIterable(List#{true})

    @test
    def map12(): Bool = ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{123, -123, 1223, 141})) == ListSet.fromIterable(List#{false})

    @test
    def map13(): Bool = ListSet.map(x -> x `Int32.remainder` 9, ListSet.fromIterable(List#{11, 5, 16, 4})) == ListSet.fromIterable(List#{2, 5, 7, 4})

    @test
    def map14(): Bool = ListSet.map(x -> x `Int32.remainder` 9, ListSet.fromIterable(List#{0, 5, 1, -9, -8})) == ListSet.fromIterable(List#{5, 1, 0, -8})

    @test
    def map15(): Bool = ListSet.map(x -> x `Int32.remainder` 9, ListSet.fromIterable(List#{0, 5, 1, 10, 7, 19, 28, 2})) == ListSet.fromIterable(List#{0, 5, 7, 1, 2})


    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool =
        ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.empty()) == ListSet.empty()

    @test
    def filterMap02(): Bool =
        ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.singleton(1)) == ListSet.empty()

    @test
    def filterMap03(): Bool =
        ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.singleton(2)) == ListSet.singleton(1)

    @test
    def filterMap04(): Bool =
        ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.fromIterable(List#{1, 3})) == ListSet.empty()

    @test
    def filterMap05(): Bool =
        ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.fromIterable(List#{1, 4})) == ListSet.singleton(2)

    @test
    def filterMap06(): Bool =
        ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.fromIterable(List#{-1, 6})) == ListSet.singleton(3)

    @test
    def filterMap07(): Bool =
        ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.fromIterable(List#{6, 8})) == ListSet.fromIterable(List#{3, 4})

    @test
    def filterMap08(): Bool =
        let s1 = ListSet.fromIterable(List#{0, 1, 2, 3, 4, 5, 10});
        let s2 = ListSet.fromIterable(List#{0, 1, 2, 5});
        ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, s1) == s2

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def replace01(): Bool = ListSet.replace(src = 3, dst = 4, ListSet.empty()) == ListSet.empty()

    @test
    def replace02(): Bool = ListSet.replace(src = 3, dst = 4, ListSet.singleton(1)) == ListSet.singleton(1)

    @test
    def replace03(): Bool = ListSet.replace(src = 3, dst = 4, ListSet.singleton(3)) == ListSet.singleton(4)

    @test
    def replace04(): Bool = ListSet.replace(src = 3, dst = 4, ListSet.singleton(4)) == ListSet.singleton(4)

    @test
    def replace05(): Bool = ListSet.replace(src = 3, dst = 4, ListSet.fromIterable(List#{1, 2})) == ListSet.fromIterable(List#{1, 2})

    @test
    def replace06(): Bool = ListSet.replace(src = 3, dst = 4, ListSet.fromIterable(List#{1, 3})) == ListSet.fromIterable(List#{1, 4})

    @test
    def replace07(): Bool = ListSet.replace(src = 3, dst = 4, ListSet.fromIterable(List#{3, 2})) == ListSet.fromIterable(List#{4, 2})

    @test
    def replace08(): Bool = ListSet.replace(src = 3, dst = 4, ListSet.fromIterable(List#{3, 4})) == ListSet.singleton(4)

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def partition01(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.empty()) == (ListSet.empty(), ListSet.empty())

    @test
    def partition02(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(1)) == (ListSet.empty(), ListSet.singleton(1))

    @test
    def partition03(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(2)) == (ListSet.singleton(2), ListSet.empty())

    @test
    def partition04(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, 3})) == (ListSet.empty(), ListSet.fromIterable(List#{1, 3}))

    @test
    def partition05(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, 2})) == (ListSet.singleton(2), ListSet.singleton(1))

    @test
    def partition06(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{2, 1})) == (ListSet.singleton(2), ListSet.singleton(1))

    @test
    def partition07(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{2, -4})) == (ListSet.fromIterable(List#{2, -4}), ListSet.empty())

    @test
    def partition08(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{2, -11, 89, -4, 11, -6, 84})) == (ListSet.fromIterable(List#{2, -4, -6, 84}), ListSet.fromIterable(List#{-11, 89, 11}))

    @test
    def partition09(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{84, -6, 11, -4, 89, -11, 2})) == (ListSet.fromIterable(List#{84, -6, -4, 2}), ListSet.fromIterable(List#{11, 89, -11}))

    @test
    def partition10(): Bool = ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8})) == (ListSet.fromIterable(List#{2, 4, 6, 8}), ListSet.fromIterable(List#{1, 3, 5, 7}))

    /////////////////////////////////////////////////////////////////////////////
    // toOrderedSet                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toOrderedSet01(): Bool = ListSet.toOrderedSet(x -> x, (ListSet.empty(): ListSet[Unit])) == Set#{}

    @test
    def toOrderedSet02(): Bool = ListSet.toOrderedSet(x -> x, ListSet.singleton(1)) == Set#{1}

    @test
    def toOrderedSet03(): Bool = ListSet.toOrderedSet(x -> x, ListSet.fromIterable(List#{1, 2})) == Set#{1, 2}

    @test
    def toOrderedSet04(): Bool = ListSet.toOrderedSet(x -> x, ListSet.fromIterable(List#{1, 2, 3})) == Set#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def eq01(): Bool = (ListSet.empty(): ListSet[Unit]) == ListSet.empty()

    @test
    def eq02(): Bool = ListSet.singleton(1) != ListSet.empty()

    @test
    def eq03(): Bool = ListSet.empty() != ListSet.singleton(1)

    @test
    def eq04(): Bool = ListSet.fromIterable(List#{1, 2}) != ListSet.empty()

    @test
    def eq05(): Bool = ListSet.empty() != ListSet.fromIterable(List#{1, 2})

    @test
    def eq06(): Bool = ListSet.singleton(1) != ListSet.singleton(2)

    @test
    def eq07(): Bool = ListSet.singleton(1) == ListSet.singleton(1)

    @test
    def eq08(): Bool = ListSet.fromIterable(List#{1, 2, 3}) != ListSet.empty()

    @test
    def eq09(): Bool = ListSet.fromIterable(List#{1, 2}) != ListSet.singleton(1)

    @test
    def eq10(): Bool = ListSet.singleton(1) != ListSet.fromIterable(List#{1, 2})

    @test
    def eq11(): Bool = ListSet.empty() != ListSet.fromIterable(List#{1, 2, 3})

    @test
    def eq12(): Bool = ListSet.fromIterable(List#{1, 2}) != ListSet.fromIterable(List#{1, 3})

    @test
    def eq13(): Bool = ListSet.fromIterable(List#{1, 2}) == ListSet.fromIterable(List#{2, 1})

    @test
    def eq14(): Bool = ListSet.fromIterable(List#{1, 2}) == ListSet.fromIterable(List#{1, 2})

    @test
    def eq15(): Bool = ListSet.fromIterable(List#{1, 2, 3}) != ListSet.fromIterable(List#{1, 2, 4})

    @test
    def eq16(): Bool = ListSet.fromIterable(List#{1, 2, 3}) == ListSet.fromIterable(List#{1, 2, 3})

    @test
    def eq17(): Bool = ListSet.fromIterable(List#{1, 2, 3}) == ListSet.fromIterable(List#{2, 3, 1})

    @test
    def eq18(): Bool = ListSet.fromIterable(List#{1, 2, 3}) == ListSet.fromIterable(List#{3, 1, 2})

    @test
    def eq19(): Bool = ListSet.fromIterable(List#{1, 2, 3}) == ListSet.fromIterable(List#{2, 1, 3})

    /////////////////////////////////////////////////////////////////////////////
    // unfold                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unfold01(): Bool =
        ListSet.unfold(s -> if (true) None else Some((s + 48, s + 1)), 0) == ListSet.empty()

    @test
    def unfold02(): Bool =
        ListSet.unfold(s -> if (s > 0) None else Some((s + 48, s + 1)), 0) == ListSet.singleton(48)

    @test
    def unfold03(): Bool =
        ListSet.unfold(s -> if (s > 1) None else Some((s + 48, s + 1)), 0) == ListSet.fromIterable(List#{48, 49})

    @test
    def unfold04(): Bool =
        ListSet.unfold(s -> if (s >= 10) None else Some((s + 48, s + 1)), 0) == ListSet.fromIterable(List#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57})

    @test
    def unfold05(): Bool =
        ListSet.unfold(s -> if (s >= 10) None else Some((s + 48, s + 1)), 5) == ListSet.fromIterable(List#{53, 54, 55, 56, 57})

    @test
    def unfold06(): Bool =
        ListSet.unfold(s -> if (s >= 10) None else Some((s + 48, s + 2)), 0) == ListSet.fromIterable(List#{48, 50, 52, 54, 56})

    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithIter                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unfoldWithIter01(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (true)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        ListSet.unfoldWithIter(step) == ListSet.empty()
    }

    @test
    def unfoldWithIter02(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 0)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        ListSet.unfoldWithIter(step) == ListSet.singleton(48)
    }

    @test
    def unfoldWithIter03(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 1)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        ListSet.unfoldWithIter(step) == ListSet.fromIterable(List#{48, 49})
    }

    @test
    def unfoldWithIter04(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        ListSet.unfoldWithIter(step) == ListSet.fromIterable(List#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57})
    }

    @test
    def unfoldWithIter05(): Bool = region rc {
        let x = Ref.fresh(rc, 5);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        ListSet.unfoldWithIter(step) == ListSet.fromIterable(List#{53, 54, 55, 56, 57})
    }

    @test
    def unfoldWithIter06(): Bool = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 2, x);
                c
            };
        ListSet.unfoldWithIter(step) == ListSet.fromIterable(List#{48, 50, 52, 54, 56})
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        toString(ListSet.singleton(1)) == "ListSet(1)"

    @test
    def toString02(): Bool =
        toString(ListSet.fromIterable(List#{1, 2, 3})) == "ListSet(3, 2, 1)"

    @test
    def toString03(): Bool =
        toString(ListSet.fromIterable(List#{1, 2})) == "ListSet(2, 1)"

    @test
    def toString04(): Bool =
        toString(ListSet.fromIterable(List#{97, 2, 3, 4, 0})) == "ListSet(0, 4, 3, 2, 97)"

    @test
    def toString05(): Bool =
        toString(ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{4, 6})})) == "ListSet(ListSet(6, 4), ListSet(2, 1))"

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = ListSet.minimumBy((x, y) -> x <=> y, (ListSet.empty(): ListSet[Int32])) == None

    @test
    def minimumBy02(): Bool = ListSet.minimumBy((x, y) -> x <=> y, ListSet.singleton(1)) == Some(1)

    @test
    def minimumBy03(): Bool = ListSet.minimumBy((x, y) -> x <=> y, ListSet.fromIterable(List#{1, 2, 3, 0})) == Some(0)

    @test
    def minimumBy04(): Bool = ListSet.minimumBy((x, y) -> x <=> y, ListSet.range(5, 55)) == Some(5)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = ListSet.maximumBy((x, y) -> x <=> y, (ListSet.empty(): ListSet[Int32])) == None

    @test
    def maximumBy02(): Bool = ListSet.maximumBy((x, y) -> x <=> y, ListSet.singleton(1)) == Some(1)

    @test
    def maximumBy03(): Bool = ListSet.maximumBy((x, y) -> x <=> y, ListSet.fromIterable(List#{1, 2, 3, 0})) == Some(3)

    @test
    def maximumBy04(): Bool = ListSet.maximumBy((x, y) -> x <=> y, ListSet.range(5, 55)) == Some(54)

}
