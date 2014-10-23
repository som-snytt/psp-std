package psp
package dmz

import scala._
import scala.{ collection => sc }
import sc.{ generic => scg, mutable => scm, immutable => sci }
import java.nio.{ file => jnf }
import psp.std.api._

/** Building a default namespace consciously rather than accretively.
 */
trait ScalaLibrary extends Any with ApiAliases {
  // Semi-privileged types which might be avoidable with unreasonable effort.
  type List[+A]         = sci.List[A]
  type Option[+A]       = scala.Option[A]
  type Product2[+A, +B] = scala.Product2[A, B]
  type Some[+A]         = scala.Some[A]
  type Tuple2[+A, +B]   = scala.Tuple2[A, B]
  type ControlThrowable = scala.util.control.ControlThrowable

  // Unrenamed scala types.
  type Try[+A]                     = scala.util.Try[A]
  type Success[+A]                 = scala.util.Success[A]
  type Failure[+A]                 = scala.util.Failure[A]

  type Seq[+A] = sc.Seq[A]    // XXX

  type ArrayBuffer[A]                  = scm.ArrayBuffer[A]
  type Builder[-Elem, +To]             = scm.Builder[Elem, To]
  type ListBuffer[A]                   = scm.ListBuffer[A]

  type BigDecimal                      = scala.math.BigDecimal
  type BigInt                          = scala.math.BigInt
  type Numeric[A]                      = scala.math.Numeric[A]
  type Ordered[A]                      = scala.math.Ordered[A]
  type Ordering[A]                     = scala.math.Ordering[A]

  type CanBuildFrom[-From, -Elem, +To] = scg.CanBuildFrom[From, Elem, To]
  type ClassManifest[A]                = scala.reflect.ClassManifest[A]
  type ClassTag[A]                     = scala.reflect.ClassTag[A]
  type Codec                           = scala.io.Codec
  type Function0[+R]                   = scala.Function0[R]
  type Function1[-T, +R]               = scala.Function1[T, R]
  type Function2[-T1, -T2, +R]         = scala.Function2[T1, T2, R]
  type Function3[-T1, -T2, -T3, +R]    = scala.Function3[T1, T2, T3, R]
  type Manifest[A]                     = scala.reflect.Manifest[A]
  type StringBuilder                   = scala.StringBuilder

  // common annotations
  type inline      = scala.inline
  type specialized = scala.specialized
  type switch      = scala.annotation.switch
  type tailrec     = scala.annotation.tailrec
  type uV          = scala.annotation.unchecked.uncheckedVariance

  // scala types which I won't let win.
  type sCollection[+A]       = sc.GenTraversable[A]
  type scIndexedSeq[+A]      = sc.IndexedSeq[A]
  type scIterable[+A]        = sc.Iterable[A]
  type scIterator[+A]        = sc.Iterator[A]
  type scLinearSeq[+A]       = sc.LinearSeq[A]
  type scMap[K, +V]          = sc.Map[K, V]
  type scSeq[+A]             = sc.Seq[A]
  type scSet[A]              = sc.Set[A]
  type scSortedMap[K, +V]    = sc.SortedMap[K, V]
  type scTraversableOnce[+A] = sc.TraversableOnce[A]
  type scTraversable[+A]     = sc.Traversable[A]
  type sciBitSet             = sci.BitSet
  type sciIndexedSeq[+A]     = sci.IndexedSeq[A]
  type sciLinearSeq[+A]      = sci.LinearSeq[A]
  type sciList[+A]           = sci.List[A]
  type sciMap[K, +V]         = sci.Map[K, V]
  type sciRange              = sci.Range
  type sciSeq[+A]            = sci.Seq[A]
  type sciSet[A]             = sci.Set[A]
  type sciStream[+A]         = sci.Stream[A]
  type sciTraversable[+A]    = sci.Traversable[A]
  type sciVector[+A]         = sci.Vector[A]
  type scmMap[K, V]          = scm.Map[K, V]
  type scmSeq[A]             = scm.Seq[A]
  type scmSet[A]             = scm.Set[A]
  type scmWrappedArray[A]    = scm.WrappedArray[A]

  // original convenience aliases.
  type Array2[A]                 = Array[Array[A]]
  type Array3[A]                 = Array[Array[Array[A]]]
  type Array4[A]                 = Array[Array[Array[Array[A]]]]
  type Array5[A]                 = Array[Array[Array[Array[Array[A]]]]]
  type Bytes                     = Array[Byte]
  type CTag[A]                   = scala.reflect.ClassTag[A]
  type CanBuildSelf[-Elem, Self] = scg.CanBuildFrom[Self, Elem, Self]
  type CanBuild[-Elem, +To]      = scg.CanBuildFrom[_, Elem, To]
  type Chars                     = Array[Char]
  type GTOnce[+A]                = sc.GenTraversableOnce[A]
  type PathDirStream             = jnf.DirectoryStream[jnf.Path]
  type PathPredicate             = jnf.Path => Boolean
  type Predicate2[-A1, -A2]      = (A1, A2) => Boolean
  type UShort                    = Char
  type Unary[A]                  = A => A
}
