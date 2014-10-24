package psp
package std

import java.{ lang => jl }
import scala.{ collection => sc }
import api._
import psp.dmz.PolicyDmz

/** Yes I know all about implicit classes.
 *  There's no way to write an implicit value class which doesn't hardcode
 *  its location into an object. Separating the implicit conversion from
 *  the class allows clients to build their own package object.
 *
 *  This is all a consequence of scala offering no means for managing namespaces,
 *  so namespace management has become hopelessly entangled with unrelated concerns
 *  like inheritance, specificity, method dispatch, and so forth.
 */
abstract class StdPackage
      extends OrderInstances
         with StdZipped
         with StdProperties
         with AlgebraInstances
         with GlobalShow
         with StdGateways
         with StdArrowAssoc
         with PolicyDmz {

  implicit class ApiOrderOps[A](val ord: Order[A]) {
    def reverse: Order[A]          = Order[A]((x, y) => ord.compare(x, y).flip)
    def on[B](f: B => A): Order[B] = Order[B]((x, y) => ord.compare(f(x), f(y)))
  }
  implicit class BuildsOps[Elem, To](z: Builds[Elem, To]) {
    def comap[Prev](f: Prev => Elem): Builds[Prev, To] = Builds(xs => z build (xs map f))
    def map[Next](f: To => Next): Builds[Elem, Next]   = Builds(xs => f(z build xs))
    def direct: Suspended[Elem] => To                  = mf => z build Foreach(mf)
    def scalaBuilder: scmBuilder[Elem, To]                = sciVector.newBuilder[Elem] mapResult (xs => z build xs)
  }
  implicit class JavaEnumerationOps[A](it: jEnumeration[A]) {
    def toIterator = BiIterator enumeration it
  }
  implicit class TupleViewOps[A, B](val xs: View[(A, B)]) {
    def filterLeft(p: Predicate[A])  = xs withFilter (x => p(x._1))
    def filterRight(p: Predicate[B]) = xs withFilter (x => p(x._2))
    def lefts: View[A]               = xs map (_._1)
    def rights: View[B]              = xs map (_._2)
  }
  implicit class Tuple2Ops[A, B](val lhs: (A, B)) {
    def fold[C, D](rhs: (A, B))(f: (A, A) => C, g: (B, B) => C)(h: (C, C) => D): D =
      h(f(lhs._1, rhs._1), g(lhs._2, rhs._2))
  }
  implicit class AnyTargetSeqOps[A: HashEq](root: A) {
    def transitiveDepth(maxDepth: Int, expand: A => pSeq[A]): pSeq[A] = {
      var seen = PolicySet.elems[A]()
      def loop(depth: Int, root: A, f: A => Unit): Unit = if (depth < maxDepth && !seen(root)) {
        seen = seen union exSet(root)
        f(root)
        expand(root) |> (xs => if (xs != null) xs foreach (x => loop(depth + 1, x, f)))
      }
      Foreach(f => loop(0, root, f))
    }
    def transitiveClosure(expand: A => pSeq[A]): pSeq[A] = {
      var seen = PolicySet.elems[A]()
      def loop(root: A, f: A => Unit): Unit = if (!seen(root)) {
        seen = seen union exSet(root)
        f(root)
        expand(root) |> (xs => if (xs != null) xs foreach (x => loop(x, f)))
      }
      Foreach(f => loop(root, f))
    }
  }
  implicit def booleanToPredicate(value: Boolean): Predicate[Any] = if (value) ConstantTrue else ConstantFalse
  implicit def jClassToPolicyClass(x: jClass): PolicyClass        = new PolicyClass(x)
  implicit def intToPreciseSize(n: Int): IntSize                  = Precise(n)

  implicit def viewifyString(x: String): View[Char]          = x.m
  implicit def viewifyArray[A](x: Array[A]): View[A]         = x.m[DirectAccess] // must give this type argument explicitly.
  implicit def unViewifyString(x: View[Char]): String        = x.force[String]
  implicit def unViewifyArray[A: CTag](x: View[A]): Array[A] = x.force[Array[A]]

  implicit def convertPolicySeq[A, B](xs: pSeq[A])(implicit conversion: A => B): pSeq[B] = xs map (x => conversion(x))
  implicit def scalaSeqToPSeq[A](x: scSeq[A]): pVector[A] = x.pvec

  implicit def conforms[A] : (A <:< A) = new conformance[A]
}
