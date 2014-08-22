package psp
package std

/** Experiments with a type class basis.
 */
object TypeClasses {
  trait Basis[-R] {
    def sizeInfo(xs: R): SizeInfo
  }
  trait Intensional[-R, -A] extends Basis[R] {
    def isMember(xs: R): Predicate[A]
  }
  trait Extensional[-R, +A] extends Basis[R] {
    def members(xs: R): Suspended[A]
  }
  trait Bothtensional[-R, A] extends Intensional[R, A] with Extensional[R, A]

  trait Indexed[-R, +A] extends Seq[R, A] {
    def size(xs: R): Size
    def indexed(xs: R): Index => A
  }
  trait Linear[R, +A] extends Seq[R, A] {
    def head(xs: R): A
    def tail(xs: R): R
  }

  trait Seq[-R, +A] extends Extensional[R, A]
  trait Set[-R, A] extends Bothtensional[R, A]
  trait Map[-R, K, +V] extends Bothtensional[R, K] { def apply(xs: R)(k: K): V }

  trait IndexedLeaf[-R, A] extends Indexed[R, A] with Bothtensional[R, A]

  trait Empty[CC[X]]                { def empty[A] : CC[A]                            }
  trait Filter[R, +A]               { def filter(xs: R)(p: Predicate[A]): R           }
  trait Mapped[CC[X], A]            { def map[B](xs: CC[A])(f: A => B): CC[B]         }
  trait FlatMapped[CC[X], A, FM[X]] { def flatMap[B](xs: CC[A])(f: A => FM[B]): CC[B] }
}

trait TypeClassOps {
  import TypeClasses._

  implicit final class BasisClassOps[R](val xs: R)(implicit v: Basis[R]) {
    def sizeInfo: SizeInfo = v.sizeInfo(xs)
  }
  implicit final class MapClassOps[R, K, V](val xs: R)(implicit v: Map[R, K, V]) {
    def apply(k: K): V                   = v(xs)(k)
    def members: Suspended[K]            = v.members(xs)
    def foreachKey(f: K => Unit): Unit   = members(f)
    def foreachValue(f: V => Unit): Unit = members(x => f(apply(x)))
  }

  implicit final class IndexedClassOps[R, A](val xs: R)(implicit v: Indexed[R, A]) {
    def elemAt(index: Index): A         = v.indexed(xs)(index)
    def size                            = v.size(xs)
    def exclusiveEnd: Index             = size.toIndex
    def hasIndex(index: Index): Boolean = indexRange contains index
    def indexRange: IndexRange          = size.toIndexRange
    def index(elem: A): Index           = indexRange find (i => elemAt(i) == elem)
    def lastIndex(elem: A): Index       = indexRange findReverse (i => elemAt(i) == elem)
  }
}

trait ScalaTypeClasses {
  import scala.collection.immutable.{ Traversable, Set, Seq, Map }

  class ForeachClass[A] extends TypeClasses.Extensional[Traversable[A], A] {
    def members(xs: Traversable[A])            = xs.foreach
    def sizeInfo(xs: Traversable[A]): SizeInfo = if (xs.isEmpty) SizeInfo.Empty else SizeInfo.NonEmpty
  }
  class ArrayClass[A] extends TypeClasses.Indexed[Array[A], A] {
    def size(xs: Array[A])               = Size(xs.length)
    def indexed(xs: Array[A])            = xs apply _.value
    def members(xs: Array[A])            = xs.foreach
    def sizeInfo(xs: Array[A]): SizeInfo = size(xs)
  }
  class LinearClass[A] extends TypeClasses.Linear[LinearSeq[A], A] {
    def head(xs: LinearSeq[A]): A            = xs.head
    def tail(xs: LinearSeq[A]): LinearSeq[A] = xs.tail
    def members(xs: LinearSeq[A])            = xs.foreach
    def sizeInfo(xs: LinearSeq[A]): SizeInfo = if (xs.isEmpty) SizeInfo.Empty else SizeInfo.NonEmpty
  }
  class StreamClass[A] extends TypeClasses.Linear[Stream[A], A] {
    def head(xs: Stream[A]): A            = xs.head
    def tail(xs: Stream[A]): Stream[A]    = xs.tail
    def members(xs: Stream[A])            = xs.foreach
    def sizeInfo(xs: Stream[A]): SizeInfo = if (xs.isEmpty) SizeInfo.Empty else if (xs.hasDefiniteSize) SizeInfo(xs.size) else SizeInfo.NonEmpty
  }
  class IndexedClass[A] extends TypeClasses.Indexed[IndexedSeq[A], A] {
    def size(xs: IndexedSeq[A]): Size         = Size(xs.size)
    def indexed(xs: IndexedSeq[A])            = xs apply _.value
    def members(xs: IndexedSeq[A])            = xs.foreach
    def sizeInfo(xs: IndexedSeq[A]): SizeInfo = size(xs)
  }
  class SetClass[A] extends TypeClasses.Set[Set[A], A] {
    def members(xs: Set[A])            = xs foreach _
    def isMember(xs: Set[A])           = xs
    def sizeInfo(xs: Set[A]): SizeInfo = SizeInfo(xs.size)
  }
  class MapClass[K, V] extends TypeClasses.Map[Map[K, V], K, V] {
    def members(xs: Map[K, V])            = xs.keys.foreach
    def apply(xs: Map[K, V])(k: K): V     = xs(k)
    def isMember(xs: Map[K, V])           = xs.isDefinedAt
    def sizeInfo(xs: Map[K, V]): SizeInfo = SizeInfo(xs.size)
  }
  class StringClass extends TypeClasses.Indexed[String, Char] {
    def size(xs: String): Size         = Size(xs.length)
    def indexed(xs: String)            = xs charAt _.value
    def members(xs: String)            = xs.toCharArray.foreach
    def sizeInfo(xs: String): SizeInfo = size(xs)
  }
}

object ScalaTypeClasses extends ScalaTypeClasses with TypeClassOps {
  implicit def foreachClass[A] = new ForeachClass[A]
  implicit def linearClass[A]  = new LinearClass[A]
  implicit def streamClass[A]  = new StreamClass[A]
  implicit def indexedClass[A] = new IndexedClass[A]
  implicit def arrayClass[A]   = new ArrayClass[A]
  implicit def setClass[A]     = new SetClass[A]
  implicit def mapClass[K, V]  = new MapClass[K, V]
  implicit def stringClass     = new StringClass
}
