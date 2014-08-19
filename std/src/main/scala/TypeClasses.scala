package psp
package std

/** Experiments with a type class basis.
 */
object TypeClasses {
  trait Basis[-R] {
    def sizeInfo(xs: R): SizeInfo
  }
  trait Intensional[-R, -A] extends Basis[R] {
    def membership(xs: R): Predicate[A]
  }
  trait Extensional[-R, +A] extends Basis[R] {
    def enumerate(xs: R): Suspended[A]
  }
  trait Indexed[-R, +A] extends Extensional[R, A] {
    def size(xs: R): Size
    def indexed(xs: R): Index => A
  }
  trait Linear[R, +A] extends Extensional[R, A] {
    def head(xs: R): A
    def tail(xs: R): R
  }
  trait Set[-R, A] extends Extensional[R, A] with Intensional[R, A]
  trait Map[-R, K, +V] extends Set[R, K] { def apply(xs: R)(k: K): V }
  trait IndexedLeaf[-R, A] extends Indexed[R, A] with Intensional[R, A]

  trait Packaged[-CC[X], -R] extends Basis[R]
  trait Mapped[CC[X], A] extends Packaged[CC, CC[A]] { def map[B](xs: CC[A])(f: A => B): CC[B] }
}

trait TypeClassOps {
  import TypeClasses._

  implicit final class BasisClassOps[R](val xs: R)(implicit v: Basis[R]) {
    def sizeInfo: SizeInfo = v.sizeInfo(xs)
  }
  implicit final class MapClassOps[R, K, V](val xs: R)(implicit v: Map[R, K, V]) {
    def apply(k: K): V                   = v.apply(xs)(k)
    def foreachKey(f: K => Unit): Unit   = v.enumerate(xs)(f)
    def foreachValue(f: V => Unit): Unit = v.enumerate(xs)(x => f(apply(x)))
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
  import scala.collection.{ immutable => imm }

  class ForeachClass[A] extends TypeClasses.Extensional[imm.Traversable[A], A] {
    def enumerate(xs: imm.Traversable[A])          = xs foreach _
    def sizeInfo(xs: imm.Traversable[A]): SizeInfo = if (xs.isEmpty) SizeInfo.Empty else SizeInfo.NonEmpty
  }
  class ArrayClass[A] extends TypeClasses.Indexed[Array[A], A] {
    def size(xs: Array[A])               = Size(xs.length)
    def indexed(xs: Array[A])            = xs apply _.value
    def enumerate(xs: Array[A])          = xs foreach _
    def sizeInfo(xs: Array[A]): SizeInfo = size(xs)
  }
  class LinearClass[A] extends TypeClasses.Linear[imm.LinearSeq[A], A] {
    def head(xs: imm.LinearSeq[A]): A                = xs.head
    def tail(xs: imm.LinearSeq[A]): imm.LinearSeq[A] = xs.tail
    def enumerate(xs: imm.LinearSeq[A])              = xs foreach _
    def sizeInfo(xs: imm.LinearSeq[A]): SizeInfo     = if (xs.isEmpty) SizeInfo.Empty else SizeInfo.NonEmpty
  }
  class StreamClass[A] extends TypeClasses.Linear[imm.Stream[A], A] {
    def head(xs: imm.Stream[A]): A             = xs.head
    def tail(xs: imm.Stream[A]): imm.Stream[A] = xs.tail
    def enumerate(xs: imm.Stream[A])           = xs foreach _
    def sizeInfo(xs: imm.Stream[A]): SizeInfo  = if (xs.isEmpty) SizeInfo.Empty else if (xs.hasDefiniteSize) SizeInfo(xs.size) else SizeInfo.NonEmpty
  }
  class IndexedClass[A] extends TypeClasses.Indexed[imm.IndexedSeq[A], A] {
    def size(xs: imm.IndexedSeq[A]): Size         = Size(xs.size)
    def indexed(xs: imm.IndexedSeq[A])            = xs apply _.value
    def enumerate(xs: imm.IndexedSeq[A])          = xs foreach _
    def sizeInfo(xs: imm.IndexedSeq[A]): SizeInfo = size(xs)
  }
  class SetClass[A] extends TypeClasses.Set[Set[A], A] {
    def enumerate(xs: Set[A])          = xs foreach _
    def membership(xs: Set[A])         = xs
    def sizeInfo(xs: Set[A]): SizeInfo = SizeInfo(xs.size)
  }
  class MapClass[K, V] extends TypeClasses.Map[Map[K, V], K, V] {
    def enumerate(xs: Map[K, V])          = xs.keys foreach _
    def apply(xs: Map[K, V])(k: K): V     = xs(k)
    def membership(xs: Map[K, V])         = xs isDefinedAt _
    def sizeInfo(xs: Map[K, V]): SizeInfo = SizeInfo(xs.size)
  }
  class StringClass extends TypeClasses.Indexed[String, Char] {
    def size(xs: String): Size         = Size(xs.length)
    def indexed(xs: String)            = xs charAt _.value
    def enumerate(xs: String)          = xs.toCharArray foreach _
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
