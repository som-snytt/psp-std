package psp
package std

import Nat._

trait LowPriorityNatList {
  self: NatList.type =>

  implicit class AmbiguousEmptyListOps[A](xs: NatList[_0, A]) {
    def head: Nothing                   = ???
    def tail: NatList[Nothing, Nothing] = ???
  }
}

sealed trait NatList[N <: Nat, A] extends Foreach[A] with api.HasStaticSize[N] {
  def isEmpty: Boolean
  def unsafeHead: A
  def unsafeTail: NatList[N#Prev, A]

  private def asTail[A] = this.castTo[NatList[N#Succ#Prev, A]]

  def ::[A1 >: A](hd: A1) = new NatList.::(hd, asTail[A1])

  def forgetSize: NatList[??, A]  = asExpected(this)
  def size: Size                  = if (isEmpty) Size(0) else unsafeTail.size + Size(1)
  def sizeInfo                    = Precise(size)
  def foreach(f: A => Unit): Unit = if (!isEmpty) { f(unsafeHead) ; unsafeTail foreach f }
}

object NatList extends LowPriorityNatList with GeneralizedApply {
  trait Poly2[A, B] {
    def apply[R](f: (A, B) => R): Foreach[R]
  }
  trait Poly3[A, B, C] {
    def apply[R](f: (A, B, C) => R): Foreach[R]
  }

  def map2[N <: Nat, A, B, C](xs: NatList[N, A], ys: NatList[N, B])(f: (A, B) => C): NatList[N, C] = (xs, ys) match {
    case (x :: xs, y :: ys) => new ::(f(x, y), map2(xs, ys)(f))
    case (Nil, Nil)         => empty
    case _                  => ???
  }
  def map3[N <: Nat, A, B, C, D](xs: NatList[N, A], ys: NatList[N, B], zs: NatList[N, C])(f: (A, B, C) => D): NatList[N, D] = (xs, ys, zs) match {
    case (x :: xs, y :: ys, z :: zs) => new ::(f(x, y, z), map3(xs, ys, zs)(f))
    case (Nil, Nil, Nil)             => empty
    case _                           => ???
  }

  class SafeZipped2[N <: Nat, A, B](xs: NatList[N, A], ys: NatList[N, B]) {
    def map = new Poly2[A, B] { def apply[R](f: (A, B) => R): Foreach[R] = map2(xs, ys)(f) }
    def zip[C](zs: NatList[N, C]): SafeZipped3[N, A, B, C] = new SafeZipped3(xs, ys, zs)
  }

  class SafeZipped3[N <: Nat, A, B, C](xs: NatList[N, A], ys: NatList[N, B], zs: NatList[N, C]) {
    def map = new Poly3[A, B, C] { def apply[R](f: (A, B, C) => R): Foreach[R] = map3(xs, ys, zs)(f) }
  }

  implicit class NatListOps[N <: Nat, A](xs: NatList[N, A]) {
    def head: A                                         = xs.unsafeHead
    def tail: NatList[N#Prev, A]                        = xs.unsafeTail
    def zip[B](ys: NatList[N, B]): SafeZipped2[N, A, B] = new SafeZipped2(xs, ys)
  }
  final case class ::[N <: Nat, A](unsafeHead: A, unsafeTail: NatList[N#Prev, A]) extends NatList[N, A] {
    def isEmpty = false
    override def toString = s"$unsafeHead :: $unsafeTail"
  }
  final case object Nil extends NatList[_0, Nothing] {
    def isEmpty = true
    def unsafeHead = ???
    def unsafeTail = ???
    override def toString = "Nil"
  }

  def empty[A]   = asExpected[NatList[_0, A]](Nil)
  def unknown[A] = asExpected[NatList[??, A]](Nil)

  object arity extends ArityApplies {
    type Coll[N <: Nat, A] = NatList[N, A]

    def apply[A](): Coll[_0, A]                                                                      = asExpected(empty[A])
    def apply[A](p1: A): Coll[_1, A]                                                                 = asExpected(p1 :: empty[A])
    def apply[A](p1: A, p2: A): Coll[_2, A]                                                          = asExpected(p1 :: p2 :: empty[A])
    def apply[A](p1: A, p2: A, p3: A): Coll[_3, A]                                                   = asExpected(p1 :: p2 :: p3 :: empty[A])
    def apply[A](p1: A, p2: A, p3: A, p4: A): Coll[_4, A]                                            = asExpected(p1 :: p2 :: p3 :: p4 :: empty[A])
    def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A): Coll[_5, A]                                     = asExpected(p1 :: p2 :: p3 :: p4 :: p5 :: empty[A])
    def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A): Coll[_6, A]                              = asExpected(p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: empty[A])
    def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A): Coll[_7, A]                       = asExpected(p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: empty[A])
    def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A): Coll[_8, A]                = asExpected(p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: empty[A])
    def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A): Coll[_9, A]         = asExpected(p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: empty[A])
    def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, ps: A*): Coll[??, A] = asExpected(ps.foldRight(apply(p1, p2, p3, p4, p5, p6, p7, p8, p9).forgetSize)(_ :: _ forgetSize))
  }
}
