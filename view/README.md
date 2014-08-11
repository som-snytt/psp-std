[![Build Status](https://travis-ci.org/paulp/psp-view.png)](https://travis-ci.org/paulp/psp-view)

psp collect
===========

This is an alternative to the standard scala collections, which are unwieldy.
All operations are lazy. Computation is deferred for as long as possible.
It's still under development. Please don't use it unless you understand what that means.
I'm working on integrating it into something more comprehensive: see paulp/psp-std.

Explanation of test output, reformatted to reduce width:

```
Basis sequence was 1 to 1000
<xs> map *3  dropR 11  slice [7,41)
 Linear   Sized  Direct  <EAGER>  ListV  Stream StreamV  RangeV VectorV
 ------   -----  ------  -------  -----  ------ -------  ------ -------
     21      10       3     1000   1010      21    1010    1010    1010  //  24, 27, 30
```

The "basis sequence" defines the initial contents of each sequence (Linear, Sized, etc. explained below.)
Each test output line begins with a list of operations to be performed successively on each sequence type,
with ```<xs>``` standing in for the initial sequence. In this case the operations are to multiply each element by 3,
drop 11 elements from the right, and slice indices [7, 41) from what remains. Finally we take three elements
(if available, or whatever remains) from the front of the sequence and display it at the end of the line.

```
// In other words
1 to 1000 map (_ * 3) dropRight 11 slice (7, 41) take 3 mkString ", "
```

All the sequence varieties, ours and scala, must arrive at the same answer or it is printed
with angry exclamation points to indicate there is a bug somewhere.

- **Linear** is a sequential access sequence, similar to scala's List. Due to that access pattern,
if elements must be taken from the right side of the list it will fare as poorly as an eager
collection. This operation requires 21 steps because it must walk enough of the stream to
establish that the dropRight 11 had no effect on the slice. The elements it needs are within
the first 10, but it must traverse 10 + 11 before this fact is known.

- **Sized** is Linear but with an additional wrapper containing its precise size. This sometimes
allows work to be avoided, as it does here. Sized required 11 fewer steps than Linear,
because with knowledge that the sequence contains 1000 elements, it can infer that dropRight 11
followed by slice(7, 41) is the same as slice(7, 41), and ignore the drop.

- **Direct** is a random access sequence, similar to scala's Vector or an Array. It should never
require more steps than Sized; it will often require many fewer. In this case, as is common,
it requires exactly three, which means it is optimal in terms of operations performed. This is
possible because psp's map is deterministic - the input collection and output collection must
have the same size - and dropRight and slice have a deterministic effect on the result's indices.
The dropRight is safely ignored and after slice, the first three elements are indices 7, 8, 9.
In the original sequence those indices contains the numbers 8, 9, 10, but as that sequence was
mapped 3x the final result is 24, 27, 30.

- **&lt;EAGER&gt;** is scala's List, but it could be any scala collection other than Stream.
It will always be 1000 because scala will traverse the original 1000 elements one time
in the course of making its first eager collection, and then it will never consult the
original collection again.

- **Stream** is scala's Stream, and **StreamV**, **RangeV**, and **VectorV** are the scala
views of those collections. In this case the only one to avoid traversing the entire sequence
and then some is Stream, which performs the same amount of work as does Linear.

Performance
===========

As dramatic as the test result numbers are, they severely understate the difference.
Since we only count steps which take place on the original collection, we count
all of ours but only a portion of theirs. In particular the column <EAGER> which
represents a scala eager collection should say 3000, not 1000, because 1000 steps
are performed on three different collections.

Unnecessary allocations damage performance even more than the unnecessary work,
and this too is not shown in these statistics. Scala's eager approach creates full-sized
intermediate collections which are often immediately discarded. Here's an example of
what that can mean in practice.
```
scala> val xs = (1 to 1e7.toInt).toArray
xs: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, ...)

// Import scala's array operations implicit (we usually shadow it)
scala> import Predef.intArrayOps
import Predef.intArrayOps

// Typically about 1000ms to complete.
scala> timed(xs map (_ + 1) map (_ + 1) map (_ + 1) take 3 mkString ", ")
Elapsed: 859.385 ms
res0: String = 4, 5, 6

scala> timed(xs map (_ + 1) map (_ + 1) map (_ + 1) take 3 mkString ", ")
Elapsed: 1016.358 ms
res1: String = 4, 5, 6

// Shadow it so we use psp collections.
scala> import psp.core.intArrayOps
import psp.core.intArrayOps

// Typically about 1ms to complete.
scala> timed(xs map (_ + 1) map (_ + 1) map (_ + 1) take 3 mk_s ", ")
Elapsed: 1.406 ms
res2: String = 4, 5, 6

scala> timed(xs map (_ + 1) map (_ + 1) map (_ + 1) take 3 mk_s ", ")
Elapsed: 1.342 ms
res3: String = 4, 5, 6
```

Of course pervasive laziness offers many other benefits as well. Sometimes we succeed where scala would throw an unnecessary exception:

```
scala> val xs = Array(0, 0, 0, 0, 0, 1)
xs: Array[Int] = Array(0, 0, 0, 0, 0, 1)

// In scala, failure
scala> xs map (5 / _) takeRight 1 mkString ""
java.lang.ArithmeticException: / by zero
  at $anonfun$1.apply$mcII$sp(<console>:23)

// In psp, success
scala> xs map (5 / _) takeRight 1 join ""
res0: String = 5
```

Other times we succeed where scala opts for non-termination.

```
scala> val xs = Foreach from BigInt(1)
xs: psp.core.Foreach[BigInt] = unfold from 1

// We drop 1000 elements off the right side of infinity, then take the first three.
scala> xs dropRight 1000 take 3 join ", "
res0: String = 1, 2, 3

// Try it with a Stream view (it does terminate with Stream.)
scala> val xs = (Stream from 1).view
xs: scala.collection.immutable.StreamView[Int,scala.collection.immutable.Stream[Int]] = StreamView(...)

// Alas poor StreamView... I knew him well.
scala> xs dropRight 1000 take 3 mkString ", "
[... time passes ...]
java.lang.OutOfMemoryError: Java heap space
  at scala.collection.immutable.Stream$.from(Stream.scala:1142)
  at scala.collection.immutable.Stream$$anonfun$from$1.apply(Stream.scala:1142)
```
