# ListSet

    A set that just requires equality.

`JonathanStarup.ListSet` implements the set interface used in `Set` of the
standard library without requiring `Order` or `Hash`. This set inefficient but
might be useful for small sets.

# Package Structure
- `JonathanStarup.ListSet`
    - The main module of this package, defining the list-based set.
- `JonathanStarup.ListOps`
    - Because of limited function hiding in Flix, here are some functions that
      operate on plain lists not in the standard library.
- `JonathanStarup.UnorderedList`
    - Because of limited function hiding in Flix, here are some functions that
      operate on lists without respecting the order of the list.

# License
See [LICENSE.md](./LICENSE.md).

## Copyright
Copyright 2024 Jonathan Lindegaard Starup

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.
