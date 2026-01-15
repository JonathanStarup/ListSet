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
    @Test
    def collect01(): Unit \ Assert = region rc {
        Assert.assertEq(expected = ListSet.empty(), Collectable.collect((Iterator.empty(rc): Iterator[Int32, rc, rc])))
    }

    @Test
    def collect02(): Unit \ Assert = region rc {
        Assert.assertEq(expected = ListSet.singleton(1), Collectable.collect(Iterator.singleton(rc, 1)))
    }

    @Test
    def collect03(): Unit \ Assert = region rc {
        Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2}), Collectable.collect(Iterator.range(rc, 1, 3)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def size01(): Unit \ Assert = Assert.assertEq(expected = 0, ListSet.size((ListSet.empty(): ListSet[Int32])))

    @Test
    def size02(): Unit \ Assert = Assert.assertEq(expected = 1, ListSet.size(ListSet.singleton(1)))

    @Test
    def size03(): Unit \ Assert = Assert.assertEq(expected = 2, ListSet.size(ListSet.fromIterable(List#{1, 2})))

    @Test
    def size04(): Unit \ Assert = Assert.assertEq(expected = 3, ListSet.size(ListSet.fromIterable(List#{1, 2, 3})))

    @Test
    def size05(): Unit \ Assert = Assert.assertEq(expected = 13, ListSet.size(ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14})))

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def empty01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), (ListSet.empty(): ListSet[Unit]))

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def singleton01(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.singleton(1))

    @Test
    def singleton02(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(2), ListSet.singleton(2))

    @Test
    def singleton03(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{true}), ListSet.singleton(true))

    @Test
    def singleton04(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{false}), ListSet.singleton(false))

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def insert01(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.insert(1, ListSet.empty()))

    @Test
    def insert02(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.insert(1, ListSet.singleton(1)))

    @Test
    def insert03(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2}), ListSet.insert(1, ListSet.singleton(2)))

    @Test
    def insert04(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3}), ListSet.insert(1, ListSet.fromIterable(List#{2, 3})))

    @Test
    def insert05(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3}), ListSet.insert(1, ListSet.fromIterable(List#{1, 2, 3})))

    @Test
    def insert06(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{2, 1, 3}), ListSet.insert(1, ListSet.fromIterable(List#{2, 1, 3})))

    @Test
    def insert07(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{2, 1, 3, 7, 8, 9, 10}), ListSet.insert(1, ListSet.fromIterable(List#{2, 1, 3, 7, 8, 9, 10})))

    @Test
    def insert08(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{11, 2, 1, 3, 7, 8, 9, 10, 35}), ListSet.insert(11, ListSet.fromIterable(List#{2, 1, 3, 7, 8, 9, 10, 35})))

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def remove01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.remove(1, ListSet.empty()))

    @Test
    def remove02(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.remove(1, ListSet.singleton(1)))

    @Test
    def remove03(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(2), ListSet.remove(1, ListSet.singleton(2)))

    @Test
    def remove04(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(3), ListSet.remove(2, ListSet.fromIterable(List#{2, 3})))

    @Test
    def remove05(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(2), ListSet.remove(3, ListSet.fromIterable(List#{2, 3})))

    @Test
    def remove06(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{2, 3}), ListSet.remove(1, ListSet.fromIterable(List#{2, 3})))

    @Test
    def remove07(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7}), ListSet.remove(0, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})))

    @Test
    def remove08(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{2, 3, 4, 5, 6, 7}), ListSet.remove(1, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})))

    @Test
    def remove09(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 3, 4, 5, 6, 7}), ListSet.remove(2, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})))

    @Test
    def remove10(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3, 4, 5, 7}), ListSet.remove(6, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})))

    @Test
    def remove11(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6}), ListSet.remove(7, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})))

    @Test
    def remove12(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7}), ListSet.remove(8, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7})))

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isEmpty01(): Unit \ Assert = Assert.assertTrue(ListSet.isEmpty((ListSet.empty(): ListSet[Unit])))

    @Test
    def isEmpty02(): Unit \ Assert = Assert.assertFalse(ListSet.isEmpty(ListSet.singleton(1)))

    @Test
    def isEmpty03(): Unit \ Assert = Assert.assertFalse(ListSet.isEmpty(ListSet.fromIterable(List#{1, 2})))

    @Test
    def isEmpty04(): Unit \ Assert = Assert.assertFalse(ListSet.isEmpty(ListSet.fromIterable(List#{1, 2, 3})))

    @Test
    def isEmpty05(): Unit \ Assert = Assert.assertFalse(ListSet.isEmpty(ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8})))

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def nonEmpty01(): Unit \ Assert = Assert.assertFalse(ListSet.nonEmpty((ListSet.empty(): ListSet[Unit])))

    @Test
    def nonEmpty02(): Unit \ Assert = Assert.assertTrue(ListSet.nonEmpty(ListSet.singleton(1)))

    @Test
    def nonEmpty03(): Unit \ Assert = Assert.assertTrue(ListSet.nonEmpty(ListSet.fromIterable(List#{1, 2})))

    @Test
    def nonEmpty04(): Unit \ Assert = Assert.assertTrue(ListSet.nonEmpty(ListSet.fromIterable(List#{1, 2, 3})))

    @Test
    def nonEmpty05(): Unit \ Assert = Assert.assertTrue(ListSet.nonEmpty(ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8})))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def memberOf01(): Unit \ Assert = Assert.assertFalse(ListSet.memberOf(1, ListSet.empty()))

    @Test
    def memberOf02(): Unit \ Assert = Assert.assertFalse(ListSet.memberOf(1, ListSet.singleton(2)))

    @Test
    def memberOf03(): Unit \ Assert = Assert.assertTrue(ListSet.memberOf(1, ListSet.singleton(1)))

    @Test
    def memberOf04(): Unit \ Assert = Assert.assertTrue(ListSet.memberOf(1, ListSet.fromIterable(List#{1, 2})))

    @Test
    def memberOf05(): Unit \ Assert = Assert.assertTrue(ListSet.memberOf(2, ListSet.fromIterable(List#{1, 2})))

    @Test
    def memberOf06(): Unit \ Assert = Assert.assertFalse(ListSet.memberOf(0, ListSet.fromIterable(List#{1, 2})))

    @Test
    def memberOf07(): Unit \ Assert = Assert.assertFalse(ListSet.memberOf(3, ListSet.fromIterable(List#{1, 2})))

    @Test
    def memberOf08(): Unit \ Assert = Assert.assertFalse(ListSet.memberOf(0, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def memberOf09(): Unit \ Assert = Assert.assertTrue(ListSet.memberOf(1, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def memberOf10(): Unit \ Assert = Assert.assertTrue(ListSet.memberOf(2, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def memberOf11(): Unit \ Assert = Assert.assertTrue(ListSet.memberOf(10, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def memberOf12(): Unit \ Assert = Assert.assertFalse(ListSet.memberOf(12, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    /////////////////////////////////////////////////////////////////////////////
    // isSubsetOf                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isSubsetOf01(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf((ListSet.empty(): ListSet[Unit]), ListSet.empty()))

    @Test
    def isSubsetOf02(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.empty(), ListSet.singleton(1)))

    @Test
    def isSubsetOf03(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.singleton(1), ListSet.singleton(1)))

    @Test
    def isSubsetOf04(): Unit \ Assert = Assert.assertFalse(ListSet.isSubsetOf(ListSet.singleton(1), ListSet.singleton(2)))

    @Test
    def isSubsetOf05(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.empty(), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isSubsetOf06(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.singleton(1), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isSubsetOf07(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.singleton(2), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isSubsetOf08(): Unit \ Assert = Assert.assertFalse(ListSet.isSubsetOf(ListSet.singleton(3), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isSubsetOf09(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isSubsetOf10(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.fromIterable(List#{2, 1}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isSubsetOf11(): Unit \ Assert = Assert.assertFalse(ListSet.isSubsetOf(ListSet.fromIterable(List#{3, 1}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isSubsetOf12(): Unit \ Assert = Assert.assertFalse(ListSet.isSubsetOf(ListSet.fromIterable(List#{1, 2, 3}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isSubsetOf13(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.singleton(10), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def isSubsetOf14(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.fromIterable(List#{9, 1}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def isSubsetOf15(): Unit \ Assert = Assert.assertTrue(ListSet.isSubsetOf(ListSet.fromIterable(List#{6, 5, 8}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def isSubsetOf16(): Unit \ Assert = Assert.assertFalse(ListSet.isSubsetOf(ListSet.fromIterable(List#{10, 2, 3, 6, -1, 4}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    /////////////////////////////////////////////////////////////////////////////
    // isProperSubsetOf                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isProperSubsetOf01(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf((ListSet.empty(): ListSet[Unit]), ListSet.empty()))

    @Test
    def isProperSubsetOf02(): Unit \ Assert = Assert.assertTrue(ListSet.isProperSubsetOf(ListSet.empty(), ListSet.singleton(1)))

    @Test
    def isProperSubsetOf03(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf(ListSet.singleton(1), ListSet.singleton(1)))

    @Test
    def isProperSubsetOf04(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf(ListSet.singleton(1), ListSet.singleton(2)))

    @Test
    def isProperSubsetOf05(): Unit \ Assert = Assert.assertTrue(ListSet.isProperSubsetOf(ListSet.empty(), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isProperSubsetOf06(): Unit \ Assert = Assert.assertTrue(ListSet.isProperSubsetOf(ListSet.singleton(1), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isProperSubsetOf07(): Unit \ Assert = Assert.assertTrue(ListSet.isProperSubsetOf(ListSet.singleton(2), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isProperSubsetOf08(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf(ListSet.singleton(3), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isProperSubsetOf09(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isProperSubsetOf10(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf(ListSet.fromIterable(List#{2, 1}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isProperSubsetOf11(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf(ListSet.fromIterable(List#{3, 1}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isProperSubsetOf12(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf(ListSet.fromIterable(List#{1, 2, 3}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def isProperSubsetOf13(): Unit \ Assert = Assert.assertTrue(ListSet.isProperSubsetOf(ListSet.singleton(10), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def isProperSubsetOf14(): Unit \ Assert = Assert.assertTrue(ListSet.isProperSubsetOf(ListSet.fromIterable(List#{9, 1}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def isProperSubsetOf15(): Unit \ Assert = Assert.assertTrue(ListSet.isProperSubsetOf(ListSet.fromIterable(List#{6, 5, 8}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def isProperSubsetOf16(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf(ListSet.fromIterable(List#{10, 2, 3, 6, -1, 4}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    @Test
    def isProperSubsetOf17(): Unit \ Assert = Assert.assertFalse(ListSet.isProperSubsetOf(ListSet.fromIterable(List#{10, 2, 3, 6, 1, 4, 5, 9, 8, 7}), ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def count01(): Unit \ Assert = Assert.assertEq(expected = 0, ListSet.count(i -> i > 3, ListSet.empty()))

    @Test
    def count02(): Unit \ Assert = Assert.assertEq(expected = 0, ListSet.count(i -> i > 3, ListSet.singleton(1)))

    @Test
    def count03(): Unit \ Assert = Assert.assertEq(expected = 1, ListSet.count(i -> i > 3, ListSet.singleton(4)))

    @Test
    def count04(): Unit \ Assert = Assert.assertEq(expected = 0, ListSet.count(i -> i > 3, ListSet.fromIterable(List#{2, 1})))

    @Test
    def count05(): Unit \ Assert = Assert.assertEq(expected = 1, ListSet.count(i -> i > 3, ListSet.fromIterable(List#{8, 1})))

    @Test
    def count06(): Unit \ Assert = Assert.assertEq(expected = 1, ListSet.count(i -> i > 3, ListSet.fromIterable(List#{1, 8})))

    @Test
    def count07(): Unit \ Assert = Assert.assertEq(expected = 2, ListSet.count(i -> i > 3, ListSet.fromIterable(List#{7, 6})))


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        Assert.assertEq(expected = 0, ListSet.empty() |> ListSet.sumWith(x -> x + 1))

    @Test
    def sumWith02(): Unit \ Assert =
        Assert.assertEq(expected = 2, ListSet.singleton(1) |> ListSet.sumWith(x -> x + 1))

    @Test
    def sumWith03(): Unit \ Assert =
        Assert.assertEq(expected = 9, ListSet.fromIterable(List#{1, 2, 3}) |> ListSet.sumWith(x -> x + 1))

    @Test
    def sumWith04(): Unit \ Assert =
        Assert.assertEq(expected = 7, ListSet.fromIterable(List#{1, 2, 3, -3}) |> ListSet.sumWith(x -> x + 1))

    @Test
    def sumWith05(): Unit \ Assert =
        Assert.assertEq(expected = -6, ListSet.fromIterable(List#{-1, -2, -3, -4}) |> ListSet.sumWith(x -> x + 1))

    @Test
    def sumWith06(): Unit \ Assert =
        Assert.assertEq(expected = 2, ListSet.fromIterable(List#{10, -10}) |> ListSet.sumWith(x -> x + 1))


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flatten01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), (ListSet.flatten(ListSet.empty()): ListSet[Unit]))

    @Test
    def flatten02(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.flatten((ListSet.fromIterable(List#{ListSet.empty()}): ListSet[ListSet[Unit]])))

    @Test
    def flatten03(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1)})))

    @Test
    def flatten04(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2}), ListSet.flatten(ListSet.singleton(ListSet.fromIterable(List#{1, 2}))))

    @Test
    def flatten05(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), (ListSet.flatten(ListSet.fromIterable(List#{ListSet.empty(), ListSet.empty()})): ListSet[Unit]))

    @Test
    def flatten06(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.empty()})))

    @Test
    def flatten07(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.flatten(ListSet.fromIterable(List#{ListSet.empty(), ListSet.singleton(1)})))

    @Test
    def flatten08(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2}), ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.singleton(2)})))

    @Test
    def flatten09(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.singleton(1)})))

    @Test
    def flatten10(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3, 4, 5}), ListSet.flatten(ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 4, 5})})))

    @Test
    def flatten11(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3, 4}), ListSet.flatten(ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 4, 3})})))

    @Test
    def flatten12(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3, 4}), ListSet.flatten(ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 4, 1})})))

    @Test
    def flatten13(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3, 4}), ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.fromIterable(List#{2, 3}), ListSet.singleton(4)})))

    @Test
    def flatten14(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3}), ListSet.flatten(ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.fromIterable(List#{2, 3}), ListSet.singleton(1)})))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def exists01(): Unit \ Assert = Assert.assertFalse(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.empty()))

    @Test
    def exists02(): Unit \ Assert = Assert.assertFalse(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(5)))

    @Test
    def exists03(): Unit \ Assert = Assert.assertTrue(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(7)))

    @Test
    def exists04(): Unit \ Assert = Assert.assertTrue(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(15)))

    @Test
    def exists05(): Unit \ Assert = Assert.assertFalse(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{1, 44})))

    @Test
    def exists06(): Unit \ Assert = Assert.assertTrue(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, 71})))

    @Test
    def exists07(): Unit \ Assert = Assert.assertTrue(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{71, 12})))

    @Test
    def exists08(): Unit \ Assert = Assert.assertTrue(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{71, 79})))

    @Test
    def exists09(): Unit \ Assert = Assert.assertFalse(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, -1, -14, -2, 84, 113})))

    @Test
    def exists10(): Unit \ Assert = Assert.assertTrue(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, -1, 31, -14, -2, 84, 111})))

    @Test
    def exists11(): Unit \ Assert = Assert.assertTrue(ListSet.exists(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, -1, -14, -2, 84, 111, 38})))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def forAll01(): Unit \ Assert = Assert.assertTrue(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.empty()))

    @Test
    def forAll02(): Unit \ Assert = Assert.assertFalse(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(5)))

    @Test
    def forAll03(): Unit \ Assert = Assert.assertTrue(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(7)))

    @Test
    def forAll04(): Unit \ Assert = Assert.assertTrue(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.singleton(15)))

    @Test
    def forAll05(): Unit \ Assert = Assert.assertFalse(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{1, 44})))

    @Test
    def forAll06(): Unit \ Assert = Assert.assertFalse(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{11, 71})))

    @Test
    def forAll07(): Unit \ Assert = Assert.assertFalse(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{71, 12})))

    @Test
    def forAll08(): Unit \ Assert = Assert.assertTrue(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{71, 79})))

    @Test
    def forAll09(): Unit \ Assert = Assert.assertFalse(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{7, 15, 23, 84, 111})))

    @Test
    def forAll10(): Unit \ Assert = Assert.assertFalse(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{7, 15, 23, 111, 3})))

    @Test
    def forAll11(): Unit \ Assert = Assert.assertTrue(ListSet.forAll(x -> x `Int32.remainder` 8 == 7, ListSet.fromIterable(List#{7, 15, 23, 119, 111})))

    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def union01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.union((ListSet.empty(): ListSet[Unit]), ListSet.empty()))

    @Test
    def union02(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.union(ListSet.singleton(1), ListSet.empty()))

    @Test
    def union03(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(2), ListSet.union(ListSet.empty(), ListSet.singleton(2)))

    @Test
    def union04(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.union(ListSet.singleton(1), ListSet.singleton(1)))

    @Test
    def union05(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, -1}), ListSet.union(ListSet.singleton(1), ListSet.fromIterable(List#{-1})))

    @Test
    def union06(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{-1, 9}), ListSet.union(ListSet.empty(), ListSet.fromIterable(List#{-1, 9})))

    @Test
    def union07(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{-1, 9}), ListSet.union(ListSet.singleton(9), ListSet.fromIterable(List#{-1, 9})))

    @Test
    def union08(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{4, -1, 9}), ListSet.union(ListSet.singleton(4), ListSet.fromIterable(List#{-1, 9})))

    @Test
    def union09(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{-1, 9}), ListSet.union(ListSet.fromIterable(List#{9, -1}), ListSet.fromIterable(List#{-1, 9})))

    @Test
    def union10(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{5, -1, 9}), ListSet.union(ListSet.fromIterable(List#{9, 5}), ListSet.fromIterable(List#{-1, 9})))

    @Test
    def union11(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{6, 5, -1, 9}), ListSet.union(ListSet.fromIterable(List#{6, 5}), ListSet.fromIterable(List#{-1, 9})))

    @Test
    def union12(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{6, 5, -1, 9, 43, 7, 8, -99}), ListSet.union(ListSet.fromIterable(List#{6, -99}), ListSet.fromIterable(List#{6, 5, -1, 9, 43, 7, 8, -99})))

    @Test
    def union13(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{22, 6, 5, -1, 9, 43, 7, 8, -99}), ListSet.union(ListSet.fromIterable(List#{6, -99, -1, 5, 22}), ListSet.fromIterable(List#{6, 5, -1, 9, 43, 7, 8, -99})))

    @Test
    def union14(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{-2, -3, -4, 6, 5, -1, 9, 43, 7, 8, -99}), ListSet.union(ListSet.fromIterable(List#{-2, -3, -4}), ListSet.fromIterable(List#{6, 5, -1, 9, 43, 7, 8, -99})))

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def intersection01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.intersection((ListSet.empty(): ListSet[Unit]), ListSet.empty()))

    @Test
    def intersection02(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.intersection(ListSet.singleton(1), ListSet.empty()))

    @Test
    def intersection03(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.intersection(ListSet.empty(), ListSet.singleton(2)))

    @Test
    def intersection04(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.intersection(ListSet.singleton(1), ListSet.singleton(2)))

    @Test
    def intersection05(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.intersection(ListSet.singleton(1), ListSet.singleton(1)))

    @Test
    def intersection06(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.intersection(ListSet.empty(), ListSet.fromIterable(List#{1, 2})))

    @Test
    def intersection07(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.intersection(ListSet.fromIterable(List#{1, 2}), ListSet.empty()))

    @Test
    def intersection08(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(2), ListSet.intersection(ListSet.singleton(2), ListSet.fromIterable(List#{1, 2})))

    @Test
    def intersection09(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.intersection(ListSet.singleton(1), ListSet.fromIterable(List#{1, 2})))

    @Test
    def intersection10(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{2, 1}), ListSet.intersection(ListSet.fromIterable(List#{2, 1}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def intersection11(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2}), ListSet.intersection(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def intersection12(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(2), ListSet.intersection(ListSet.fromIterable(List#{3, 2}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def intersection13(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.intersection(ListSet.fromIterable(List#{3, 55}), ListSet.fromIterable(List#{1, 2})))

    @Test
    def intersection14(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{3, 87, -87}), ListSet.intersection(ListSet.fromIterable(List#{3, 55, 11, 87, 22, 34, -87, 23}), ListSet.fromIterable(List#{1, 2, 84, -87, 87, 3, 44})))

    @Test
    def intersection15(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{3, 87, 23}), ListSet.intersection(ListSet.fromIterable(List#{3, 55, 11, 87, 22, 34, -87, 23}), ListSet.fromIterable(List#{23, 1, 2, 84, 87, 3})))

    /////////////////////////////////////////////////////////////////////////////
    // difference                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def difference01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.difference((ListSet.empty(): ListSet[Unit]), ListSet.empty()))

    @Test
    def difference02(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.difference(ListSet.empty(), ListSet.singleton(2)))

    @Test
    def difference03(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.difference(ListSet.singleton(1), ListSet.empty()))

    @Test
    def difference04(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.difference(ListSet.singleton(1), ListSet.singleton(2)))

    @Test
    def difference05(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.difference(ListSet.singleton(1), ListSet.singleton(1)))

    @Test
    def difference06(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2}), ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.empty()))

    @Test
    def difference07(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(2), ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.singleton(1)))

    @Test
    def difference08(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.singleton(2)))

    @Test
    def difference09(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{8, 2, 4})))

    @Test
    def difference10(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 1, 2, 4})))

    @Test
    def difference11(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2}), ListSet.difference(ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{3, 11, 21, 4})))

    @Test
    def difference12(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{87, 6, 111}), ListSet.difference(ListSet.fromIterable(List#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}), ListSet.fromIterable(List#{-1, 92, 4, 5, 1, 2, 86})))

    @Test
    def difference13(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{87, 111}), ListSet.difference(ListSet.fromIterable(List#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}), ListSet.fromIterable(List#{-1, 92, 4, 5, 1, 2, 86, 99, 6})))

    /////////////////////////////////////////////////////////////////////////////
    // subsets                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def subsets01(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{ListSet.empty()}), ListSet.subsets((ListSet.empty(): ListSet[Unit])))

    @Test
    def subsets02(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{ListSet.singleton(1), ListSet.empty()}), ListSet.subsets(ListSet.singleton(1)))

    @Test
    def subsets03(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.singleton(1), ListSet.singleton(2), ListSet.empty()}), ListSet.subsets(ListSet.fromIterable(List#{1, 2})))

    @Test
    def subsets04(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2, 3}), ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{1, 3}), ListSet.singleton(1), ListSet.fromIterable(List#{2, 3}), ListSet.singleton(2), ListSet.singleton(3), ListSet.empty()}), ListSet.subsets(ListSet.fromIterable(List#{1, 2, 3})))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def filter01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.empty()))

    @Test
    def filter02(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(1)))

    @Test
    def filter03(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(2), ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(2)))

    @Test
    def filter04(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, 3})))

    @Test
    def filter05(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(8), ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{8, 3})))

    @Test
    def filter06(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(32), ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{-1, 32})))

    @Test
    def filter07(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{12, 34}), ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, 34})))

    @Test
    def filter08(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{12, 34, 88}), ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{-33, -1, 12, 1, 34, 88, 7, 77, 31})))

    @Test
    def filter09(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{12, 34, 88, -92}), ListSet.filter(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{-33, -1, 12, 1, 34, 88, 7, 77, 31, 7, -92, 841})))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def map01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.empty()))

    @Test
    def map02(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{false}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(1)))

    @Test
    def map03(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{true}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(2)))

    @Test
    def map04(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{false}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, -1})))

    @Test
    def map05(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{false, true}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, -12})))

    @Test
    def map06(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{true, false}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{16, -1})))

    @Test
    def map07(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{true}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12})))

    @Test
    def map08(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{true}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12})))

    @Test
    def map09(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{false, true}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12, 1, 14})))

    @Test
    def map10(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{true, false}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12, 1, 14, 7, 88, -91})))

    @Test
    def map11(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{true}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{12, -12, 122, 14})))

    @Test
    def map12(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{false}), ListSet.map(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{123, -123, 1223, 141})))

    @Test
    def map13(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{2, 5, 7, 4}), ListSet.map(x -> x `Int32.remainder` 9, ListSet.fromIterable(List#{11, 5, 16, 4})))

    @Test
    def map14(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{5, 1, 0, -8}), ListSet.map(x -> x `Int32.remainder` 9, ListSet.fromIterable(List#{0, 5, 1, -9, -8})))

    @Test
    def map15(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{0, 5, 7, 1, 2}), ListSet.map(x -> x `Int32.remainder` 9, ListSet.fromIterable(List#{0, 5, 1, 10, 7, 19, 28, 2})))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.empty(), ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.empty()))

    @Test
    def filterMap02(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.empty(), ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.singleton(1)))

    @Test
    def filterMap03(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.singleton(1), ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.singleton(2)))

    @Test
    def filterMap04(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.empty(), ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.fromIterable(List#{1, 3})))

    @Test
    def filterMap05(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.singleton(2), ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.fromIterable(List#{1, 4})))

    @Test
    def filterMap06(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.singleton(3), ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.fromIterable(List#{-1, 6})))

    @Test
    def filterMap07(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.fromIterable(List#{3, 4}), ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ListSet.fromIterable(List#{6, 8})))

    @Test
    def filterMap08(): Unit \ Assert =
        let s1 = ListSet.fromIterable(List#{0, 1, 2, 3, 4, 5, 10});
        let s2 = ListSet.fromIterable(List#{0, 1, 2, 5});
        Assert.assertEq(expected = s2, ListSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, s1))

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def replace01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), ListSet.replace(src = 3, dst = 4, ListSet.empty()))

    @Test
    def replace02(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.replace(src = 3, dst = 4, ListSet.singleton(1)))

    @Test
    def replace03(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(4), ListSet.replace(src = 3, dst = 4, ListSet.singleton(3)))

    @Test
    def replace04(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(4), ListSet.replace(src = 3, dst = 4, ListSet.singleton(4)))

    @Test
    def replace05(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2}), ListSet.replace(src = 3, dst = 4, ListSet.fromIterable(List#{1, 2})))

    @Test
    def replace06(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 4}), ListSet.replace(src = 3, dst = 4, ListSet.fromIterable(List#{1, 3})))

    @Test
    def replace07(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{4, 2}), ListSet.replace(src = 3, dst = 4, ListSet.fromIterable(List#{3, 2})))

    @Test
    def replace08(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(4), ListSet.replace(src = 3, dst = 4, ListSet.fromIterable(List#{3, 4})))

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def partition01(): Unit \ Assert = Assert.assertEq(expected = (ListSet.empty(), ListSet.empty()), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.empty()))

    @Test
    def partition02(): Unit \ Assert = Assert.assertEq(expected = (ListSet.empty(), ListSet.singleton(1)), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(1)))

    @Test
    def partition03(): Unit \ Assert = Assert.assertEq(expected = (ListSet.singleton(2), ListSet.empty()), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.singleton(2)))

    @Test
    def partition04(): Unit \ Assert = Assert.assertEq(expected = (ListSet.empty(), ListSet.fromIterable(List#{1, 3})), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, 3})))

    @Test
    def partition05(): Unit \ Assert = Assert.assertEq(expected = (ListSet.singleton(2), ListSet.singleton(1)), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, 2})))

    @Test
    def partition06(): Unit \ Assert = Assert.assertEq(expected = (ListSet.singleton(2), ListSet.singleton(1)), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{2, 1})))

    @Test
    def partition07(): Unit \ Assert = Assert.assertEq(expected = (ListSet.fromIterable(List#{2, -4}), ListSet.empty()), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{2, -4})))

    @Test
    def partition08(): Unit \ Assert = Assert.assertEq(expected = (ListSet.fromIterable(List#{2, -4, -6, 84}), ListSet.fromIterable(List#{-11, 89, 11})), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{2, -11, 89, -4, 11, -6, 84})))

    @Test
    def partition09(): Unit \ Assert = Assert.assertEq(expected = (ListSet.fromIterable(List#{84, -6, -4, 2}), ListSet.fromIterable(List#{11, 89, -11})), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{84, -6, 11, -4, 89, -11, 2})))

    @Test
    def partition10(): Unit \ Assert = Assert.assertEq(expected = (ListSet.fromIterable(List#{2, 4, 6, 8}), ListSet.fromIterable(List#{1, 3, 5, 7})), ListSet.partition(x -> x `Int32.remainder` 2 == 0, ListSet.fromIterable(List#{1, 2, 3, 4, 5, 6, 7, 8})))

    /////////////////////////////////////////////////////////////////////////////
    // toOrderedSet                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toOrderedSet01(): Unit \ Assert = Assert.assertEq(expected = Set#{}, ListSet.toOrderedSet(x -> x, (ListSet.empty(): ListSet[Unit])))

    @Test
    def toOrderedSet02(): Unit \ Assert = Assert.assertEq(expected = Set#{1}, ListSet.toOrderedSet(x -> x, ListSet.singleton(1)))

    @Test
    def toOrderedSet03(): Unit \ Assert = Assert.assertEq(expected = Set#{1, 2}, ListSet.toOrderedSet(x -> x, ListSet.fromIterable(List#{1, 2})))

    @Test
    def toOrderedSet04(): Unit \ Assert = Assert.assertEq(expected = Set#{1, 2, 3}, ListSet.toOrderedSet(x -> x, ListSet.fromIterable(List#{1, 2, 3})))

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def eq01(): Unit \ Assert = Assert.assertEq(expected = ListSet.empty(), (ListSet.empty(): ListSet[Unit]))

    @Test
    def eq02(): Unit \ Assert = Assert.assertFalse(ListSet.singleton(1) == ListSet.empty())

    @Test
    def eq03(): Unit \ Assert = Assert.assertFalse(ListSet.empty() == ListSet.singleton(1))

    @Test
    def eq04(): Unit \ Assert = Assert.assertFalse(ListSet.fromIterable(List#{1, 2}) == ListSet.empty())

    @Test
    def eq05(): Unit \ Assert = Assert.assertFalse(ListSet.empty() == ListSet.fromIterable(List#{1, 2}))

    @Test
    def eq06(): Unit \ Assert = Assert.assertFalse(ListSet.singleton(1) == ListSet.singleton(2))

    @Test
    def eq07(): Unit \ Assert = Assert.assertEq(expected = ListSet.singleton(1), ListSet.singleton(1))

    @Test
    def eq08(): Unit \ Assert = Assert.assertFalse(ListSet.fromIterable(List#{1, 2, 3}) == ListSet.empty())

    @Test
    def eq09(): Unit \ Assert = Assert.assertFalse(ListSet.fromIterable(List#{1, 2}) == ListSet.singleton(1))

    @Test
    def eq10(): Unit \ Assert = Assert.assertFalse(ListSet.singleton(1) == ListSet.fromIterable(List#{1, 2}))

    @Test
    def eq11(): Unit \ Assert = Assert.assertFalse(ListSet.empty() == ListSet.fromIterable(List#{1, 2, 3}))

    @Test
    def eq12(): Unit \ Assert = Assert.assertFalse(ListSet.fromIterable(List#{1, 2}) == ListSet.fromIterable(List#{1, 3}))

    @Test
    def eq13(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{2, 1}), ListSet.fromIterable(List#{1, 2}))

    @Test
    def eq14(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{1, 2}))

    @Test
    def eq15(): Unit \ Assert = Assert.assertFalse(ListSet.fromIterable(List#{1, 2, 3}) == ListSet.fromIterable(List#{1, 2, 4}))

    @Test
    def eq16(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{1, 2, 3}), ListSet.fromIterable(List#{1, 2, 3}))

    @Test
    def eq17(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{2, 3, 1}), ListSet.fromIterable(List#{1, 2, 3}))

    @Test
    def eq18(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{3, 1, 2}), ListSet.fromIterable(List#{1, 2, 3}))

    @Test
    def eq19(): Unit \ Assert = Assert.assertEq(expected = ListSet.fromIterable(List#{2, 1, 3}), ListSet.fromIterable(List#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // unfold                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfold01(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.empty(), ListSet.unfold(s -> if (true) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold02(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.singleton(48), ListSet.unfold(s -> if (s > 0) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold03(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.fromIterable(List#{48, 49}), ListSet.unfold(s -> if (s > 1) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold04(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.fromIterable(List#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57}), ListSet.unfold(s -> if (s >= 10) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold05(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.fromIterable(List#{53, 54, 55, 56, 57}), ListSet.unfold(s -> if (s >= 10) None else Some((s + 48, s + 1)), 5))

    @Test
    def unfold06(): Unit \ Assert =
        Assert.assertEq(expected = ListSet.fromIterable(List#{48, 50, 52, 54, 56}), ListSet.unfold(s -> if (s >= 10) None else Some((s + 48, s + 2)), 0))

    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithIter                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfoldWithIter01(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (true)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        Assert.assertEq(expected = ListSet.empty(), ListSet.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter02(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 0)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        Assert.assertEq(expected = ListSet.singleton(48), ListSet.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter03(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 1)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        Assert.assertEq(expected = ListSet.fromIterable(List#{48, 49}), ListSet.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter04(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        Assert.assertEq(expected = ListSet.fromIterable(List#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57}), ListSet.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter05(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 5);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        Assert.assertEq(expected = ListSet.fromIterable(List#{53, 54, 55, 56, 57}), ListSet.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter06(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 2, x);
                c
            };
        Assert.assertEq(expected = ListSet.fromIterable(List#{48, 50, 52, 54, 56}), ListSet.unfoldWithIter(step))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert =
        Assert.assertEq(expected = "ListSet(1)", toString(ListSet.singleton(1)))

    @Test
    def toString02(): Unit \ Assert =
        Assert.assertEq(expected = "ListSet(3, 2, 1)", toString(ListSet.fromIterable(List#{1, 2, 3})))

    @Test
    def toString03(): Unit \ Assert =
        Assert.assertEq(expected = "ListSet(2, 1)", toString(ListSet.fromIterable(List#{1, 2})))

    @Test
    def toString04(): Unit \ Assert =
        Assert.assertEq(expected = "ListSet(0, 4, 3, 2, 97)", toString(ListSet.fromIterable(List#{97, 2, 3, 4, 0})))

    @Test
    def toString05(): Unit \ Assert =
        Assert.assertEq(expected = "ListSet(ListSet(6, 4), ListSet(2, 1))", toString(ListSet.fromIterable(List#{ListSet.fromIterable(List#{1, 2}), ListSet.fromIterable(List#{4, 6})})))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert = Assert.assertEq(expected = None, ListSet.minimumBy((x, y) -> x <=> y, (ListSet.empty(): ListSet[Int32])))

    @Test
    def minimumBy02(): Unit \ Assert = Assert.assertEq(expected = Some(1), ListSet.minimumBy((x, y) -> x <=> y, ListSet.singleton(1)))

    @Test
    def minimumBy03(): Unit \ Assert = Assert.assertEq(expected = Some(0), ListSet.minimumBy((x, y) -> x <=> y, ListSet.fromIterable(List#{1, 2, 3, 0})))

    @Test
    def minimumBy04(): Unit \ Assert = Assert.assertEq(expected = Some(5), ListSet.minimumBy((x, y) -> x <=> y, ListSet.range(5, 55)))

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert = Assert.assertEq(expected = None, ListSet.maximumBy((x, y) -> x <=> y, (ListSet.empty(): ListSet[Int32])))

    @Test
    def maximumBy02(): Unit \ Assert = Assert.assertEq(expected = Some(1), ListSet.maximumBy((x, y) -> x <=> y, ListSet.singleton(1)))

    @Test
    def maximumBy03(): Unit \ Assert = Assert.assertEq(expected = Some(3), ListSet.maximumBy((x, y) -> x <=> y, ListSet.fromIterable(List#{1, 2, 3, 0})))

    @Test
    def maximumBy04(): Unit \ Assert = Assert.assertEq(expected = Some(54), ListSet.maximumBy((x, y) -> x <=> y, ListSet.range(5, 55)))

}
