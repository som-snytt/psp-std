package psp
package std

import api._

object Linear {
  final val Empty: Linear[Nothing] = WrapList(Nil)

  final case class WrapList[A](xs: sciList[A]) extends AnyVal with Linear[A] {
    def ::(x: A): WrapList[A] = WrapList(x :: xs)

    def isEmpty           = xs.isEmpty
    def size: Size        = if (isEmpty) Size.Empty else Size.NonEmpty
    def head: A           = xs.head
    def tail: WrapList[A] = WrapList(xs.tail)
    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }

  def fromScala[A](xs: scLinearSeq[A]): Linear[A] = WrapList(xs.toList)
  def fromJava[A](xs: jIterable[A]): Linear[A]    = WrapList(xs.m.toScalaList)

  def builder[A] : Builds[A, WrapList[A]]      = Builds[A, sciList[A]](_.toScalaList) map (x => WrapList(x))
  def empty[A] : Linear[A]                     = Empty
  def fill[A](n: Int)(body: => A): WrapList[A] = WrapList[A](sciList.fill(n)(body))
  def apply[A](xs: A*): WrapList[A]            = WrapList[A](xs.toList)
}

  //   def +:(x: A): WrapVector[A]             = WrapVector(x +: xs)
  //   def :+(x: A): WrapVector[A]             = WrapVector(xs :+ x)
  //   def ++(ys: Direct[A]): WrapVector[A]    = WrapVector(xs ++ ys.seq)
  //   def size: Precise                       = Precise(xs.size)
  //   def elemAt(i: Index): A                 = xs(i.safeInt)
  //   @inline def foreach(f: A => Unit): Unit = xs foreach f
  // }
  // final case class WrapArray[A](xs: Array[_]) extends AnyVal with DirectImpl[A] {
  //   def size                                = Precise(xs.length)
  //   def elemAt(i: Index): A                 = xs(i.safeInt).castTo[A]
  //   @inline def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
  // }
  // final case class WrapJava[A](xs: jList[A]) extends AnyVal with DirectImpl[A] {
  //   def size                                = Precise(xs.size)
  //   def elemAt(i: Index)                    = xs get i.safeInt
  //   @inline def foreach(f: A => Unit): Unit = xs foreach f
  // }
  // final case class WrapString(xs: String) extends AnyVal with DirectImpl[Char] {
  //   def size                                   = Precise(xs.length)
  //   def elemAt(i: Index)                       = xs charAt i.safeInt
  //   @inline def foreach(f: Char => Unit): Unit = size.indices foreach (i => f(xs elemAt i))
  // }
  // final case class Pure[A](size: Precise, elem: Index => A) extends DirectImpl[A] {
  //   def elemAt(i: Index)                    = elem(i)
  //   @inline def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elem(i)))
  // }
  // final case class Reversed[A](xs: Direct[A]) extends AnyVal with DirectImpl[A] {
  //   def size                = xs.size
  //   def elemAt(i: Index): A = xs(size.lastIndex - i.sizeExcluding.get)
  //   @inline def foreach(f: A => Unit): Unit = size.indices foreachReverse (i => f(xs(i)))
  // }


// }

// sealed trait PolicyList[A] extends Linear[A]
// final case object pNil extends PolicyList[Nothing] {
//   def foreach(f: Nothing => Unit): Unit = ()
//   def size = Size.Empty
//   def isEmpty  = true
//   def head     = abort("pNil.head")
//   def tail     = abort("pNil.tail")
//   override def toString = "nil"
// }
// final case class pCons[A](head: A, tail: PolicyList[A]) extends PolicyList[A] {
//   def foreach(f: A => Unit): Unit = { f(head) ; tail foreach f }
//   def size = Size.NonEmpty
//   def isEmpty  = false
//   override def toString = s"$head :: $tail"
// }

// final class pChunked[A](private val chunks: Linear[Linear[A]]) extends Linear[A] {
//   private def nonEmptyHead: pChunked[A] = if (chunks.isEmpty || chunks.head.nonEmpty) this else new pChunked(chunks.tail).nonEmptyHead

//   def isEmpty  = nonEmptyHead.chunks.isEmpty
//   def head     = nonEmptyHead.head
//   def tail     = nonEmptyHead.tail
//   def size = if (isEmpty) Size.Empty else Size.NonEmpty
//   def foreach(f: A => Unit): Unit = { f(head) ; tail foreach f }
// }

// object PolicyList {
//   implicit class PolicyListOps[A](val xs: PolicyList[A]) extends AnyVal {
//     def ::(x: A): pCons[A] = new pCons(x, xs)
//   }
//   /** FIXME - This should be a value class but in 2.10 it's our friend Mr. Bug again.
//       [error] (run-main-0) java.lang.ClassCastException: psp.std.PolicyList$FromScala cannot be cast to scala.collection.immutable.LinearSeq
//       java.lang.ClassCastException: psp.std.PolicyList$FromScala cannot be cast to scala.collection.immutable.LinearSeq
//         at psp.std.package$.fromScala(package.scala:210)
//         at psp.std.package$.fromElems(package.scala:221)
//         at psp.std.PolicySet$.elems(PolicySet.scala:13)
//    */
//    final class FromScala[A](val xs: sciLinearSeq[A]) extends AnyRef with Linear[A] {
//     def size    = Size(xs)
//     def isEmpty = xs.isEmpty
//     def head    = xs.head
//     def tail    = new FromScala(xs.tail)
//     def foreach(f: A => Unit) = xs foreach f
//   }

//   def builder[A] : Builds[A, PolicyList[A]]      = Builds(xs => xs.foldr(empty[A])(_ :: _))
//   def empty[A] : PolicyList[A]                   = pNil.castTo
//   def fill[A](n: Int)(body: => A): PolicyList[A] = if (n <= 0) empty[A] else body :: fill(n - 1)(body)

//   // Length check necessary to avoid infinite recursion in foldr, which may utilize this class
//   def apply[A](xs: A*): PolicyList[A] = if (xs.length == 0) empty[A] else xs.m.foldr(empty[A])(_ :: _)
// }

// /** A wrapper so a fixed-size sequence can linearly be decomposed with a bit less
//  *  allocation overhead.
//  */
//  final class Linearized[A](xs: Direct[A], startIndex: Index) extends Linear[A] with HasPreciseSize {
//   def foreach(f: A => Unit): Unit = xs.indices drop startIndex.indexValue.size foreach (i => f(xs(i)))

//   def size    = xs.size - startIndex.indexValue.size
//   def isEmpty = startIndex.isUndefined || xs.lastIndex < startIndex
//   def head    = xs(startIndex)
//   def tail    = new Linearized(xs, startIndex.next)
// }
