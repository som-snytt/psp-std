package psp
package dmz

import scala._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder

/** The dmz package is working around the flailing which takes place
 *  when you try to alias scala's core types. We need to be completely
 *  clear of the psp.std package object else scala will find a way
 *  to encounter a cycle, which will be handled by issuing no error
 *  and then failing to find identifiers in the near future.
 */
final object Array {
  def apply[A: ClassTag](xs: A*): Array[A]            = scala.Array.apply[A](xs: _*)
  def apply(x: Boolean, xs: Boolean*): Array[Boolean] = scala.Array.apply(x, xs: _*)
  def apply(x: Byte, xs: Byte*): Array[Byte]          = scala.Array.apply(x, xs: _*)
  def apply(x: Short, xs: Short*): Array[Short]       = scala.Array.apply(x, xs: _*)
  def apply(x: Char, xs: Char*): Array[Char]          = scala.Array.apply(x, xs: _*)
  def apply(x: Int, xs: Int*): Array[Int]             = scala.Array.apply(x, xs: _*)
  def apply(x: Long, xs: Long*): Array[Long]          = scala.Array.apply(x, xs: _*)
  def apply(x: Float, xs: Float*): Array[Float]       = scala.Array.apply(x, xs: _*)
  def apply(x: Double, xs: Double*): Array[Double]    = scala.Array.apply(x, xs: _*)
  def apply(x: Unit, xs: Unit*): Array[Unit]          = scala.Array.apply(x, xs: _*)

  def empty[A: ClassTag]: Array[A]                                  = new Array[A](0)
  def newBuilder[A: ClassTag] : ArrayBuilder[A]                     = ArrayBuilder.make[A]()
  def range(start: Int, end: Int): Array[Int]                       = scala.Array.range(start, end)
  def range(start: Int, end: Int, step: Int): Array[Int]            = scala.Array.range(start, end, step)
  def iterate[T: ClassTag](start: T, len: Int)(f: T => T): Array[T] = scala.Array.iterate[T](start, len)(f)
  def unapplySeq[T](x: Array[T]): Option[IndexedSeq[T]]             = scala.Array.unapplySeq[T](x)

  @inline def fill[T: ClassTag](n: Int)(elem: => T): Array[T]                                                                  = scala.Array.fill(n)(elem)
  @inline def fill[T: ClassTag](n1: Int, n2: Int)(elem: => T): Array[Array[T]]                                                 = scala.Array.fill(n1, n2)(elem)
  @inline def fill[T: ClassTag](n1: Int, n2: Int, n3: Int)(elem: => T): Array[Array[Array[T]]]                                 = scala.Array.fill(n1, n2, n3)(elem)
  @inline def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => T): Array[Array[Array[Array[T]]]]                 = scala.Array.fill(n1, n2, n3, n4)(elem)
  @inline def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => T): Array[Array[Array[Array[Array[T]]]]] = scala.Array.fill(n1, n2, n3, n4, n5)(elem)

  final object hashCmp extends java.util.Comparator[AnyRef] {
    private def hash(x: AnyRef): Int = java.lang.System identityHashCode x
    def compare(x: AnyRef, y: AnyRef): Int = hash(x) - hash(y)
  }

  def sortInPlace[A](xs: Array[A]): Array[A] = {
    xs match {
      case xs: Array[Byte]   => java.util.Arrays.sort(xs)
      case xs: Array[Char]   => java.util.Arrays.sort(xs)
      case xs: Array[Short]  => java.util.Arrays.sort(xs)
      case xs: Array[Int]    => java.util.Arrays.sort(xs)
      case xs: Array[Long]   => java.util.Arrays.sort(xs)
      case xs: Array[Double] => java.util.Arrays.sort(xs)
      case xs: Array[Float]  => java.util.Arrays.sort(xs)
      case xs: Array[AnyRef] => java.util.Arrays.sort[AnyRef](xs, Array.hashCmp)
      case _                 =>
    }
    xs
  }
}
