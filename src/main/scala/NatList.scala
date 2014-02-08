package psp
package core

import Nat._
import NatList._

trait LowPriorityNatList {
  self: NatList.type =>

  implicit class AmbiguousEmptyListOps[A](xs: NatList[_0, A]) {
    def head: Nothing                    = ???
    def tail: NatList[Nothing, Nothing] = ???
  }
}

sealed trait NatList[N <: Nat, +A] extends Foreach[A] with HasStaticSize[N] {
  def isEmpty: Boolean
  def unsafeHead: A
  def unsafeTail: NatList[N#Prev, A]

  private[this] def asTail[A1] = this.asInstanceOf[NatList[N#Succ#Prev, A1]]
  def ::[A1 >: A](hd: A1): NatList.::[N#Succ, A1] = new NatList.::[N#Succ, A1](hd, asTail[A1])

  def size: Size = if (isEmpty) Size(0) else unsafeTail.size + Size(1)
  def sizeInfo = Precise(size)
  def foreach(f: A => Unit): Unit = if (!isEmpty) { f(unsafeHead) ; unsafeTail foreach f }
}

object NatList extends LowPriorityNatList {
  type L0[+A] = NatList[_0, A]
  type L1[+A] = NatList[_1, A]
  type L2[+A] = NatList[_2, A]
  type L3[+A] = NatList[_3, A]
  type L4[+A] = NatList[_4, A]
  // type L5[+A] = NatList[_5, A]
  // type L6[+A] = NatList[_6, A]
  // type L7[+A] = NatList[_7, A]
  // type L8[+A] = NatList[_8, A]
  // type L9[+A] = NatList[_9, A]

  def map2[N <: Nat, A, B, C](xs: NatList[N, A], ys: NatList[N, B])(f: (A, B) => C): NatList[N, C] = (xs, ys) match {
    case (x :: xs, y :: ys) => new ::(f(x, y), map2(xs, ys)(f))
    case (Nil, Nil)         => Nil
    case _                  => ???
  }
  def map3[N <: Nat, A, B, C, D](xs: NatList[N, A], ys: NatList[N, B], zs: NatList[N, C])(f: (A, B, C) => D): NatList[N, D] = (xs, ys, zs) match {
    case (x :: xs, y :: ys, z :: zs) => new ::(f(x, y, z), map3(xs, ys, zs)(f))
    case (Nil, Nil, Nil)             => Nil
    case _                           => ???
  }

  def demo = NatList(1, 2, 3, 4) zip NatList[Double](1, 2, 3, 4)

  class SafeZipped2[N <: Nat, A, B](xs: NatList[N, A], ys: NatList[N, B]) {
    def map[R](f: (A, B) => R): Foreach[R]                  = map2(xs, ys)(f)
    def zip[C](zs: NatList[N, C]): SafeZipped3[N, A, B, C] = new SafeZipped3(xs, ys, zs)
  }

  class SafeZipped3[N <: Nat, A, B, C](xs: NatList[N, A], ys: NatList[N, B], zs: NatList[N, C]) {
    def map[R](f: (A, B, C) => R): Foreach[R] = map3(xs, ys, zs)(f)
  }

  implicit class NatListOps[N <: Nat, A](xs: NatList[N, A]) {
    def head: A                                         = xs.unsafeHead
    def tail: NatList[N#Prev, A]                        = xs.unsafeTail
    def zip[B](ys: NatList[N, B]): SafeZipped2[N, A, B] = new SafeZipped2(xs, ys)
  }

  def apply[A](): L0[A]                                                          = Nil
  def apply[A](x1: A): L1[A]                                                     = x1 :: Nil
  def apply[A](x1: A, x2: A): L2[A]                                              = x1 :: x2 :: Nil
  def apply[A](x1: A, x2: A, x3: A): L3[A]                                       = x1 :: x2 :: x3 :: Nil
  def apply[A](x1: A, x2: A, x3: A, x4: A): L4[A]                                = x1 :: x2 :: x3 :: x4 :: Nil
  // def apply[A](x1: A, x2: A, x3: A, x4: A, x5: A)                             = x1 :: x2 :: x3 :: x4 :: x5 :: Nil
  // def apply[A](x1: A, x2: A, x3: A, x4: A, x5: A, x6: A)                      = x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: Nil
  // def apply[A](x1: A, x2: A, x3: A, x4: A, x5: A, x6: A, x7: A)               = x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: Nil
  // def apply[A](x1: A, x2: A, x3: A, x4: A, x5: A, x6: A, x7: A, x8: A)        = x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: Nil
  // def apply[A](x1: A, x2: A, x3: A, x4: A, x5: A, x6: A, x7: A, x8: A, x9: A) = x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: Nil

  final case class ::[N <: Nat, +A](unsafeHead: A, unsafeTail: NatList[N#Prev, A]) extends NatList[N, A] {
    def isEmpty = false
    override def toString = s"$unsafeHead :: $unsafeTail"
  }
  final case object Nil extends NatList[_0, Nothing] {
    def isEmpty = true
    def unsafeHead = ???
    def unsafeTail = ???
    override def toString = "Nil"
  }
}
