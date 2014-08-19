package psp

import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import psp.core.impl._
import psp.std._

package core {
  trait PackageLevel extends PspTypes with PspHighPriority with PspUtilityMethods
}

package object core extends psp.core.PackageLevel {
  def unknownSize = SizeInfo.Unknown

  // Suicide is painless... I'm sure I'll move or remove these.
  // implicit def convertTuple2[A](p: Tuple2[A, A])                      = NatList(p._1, p._2)
  // implicit def convertTuple3[A](p: Tuple3[A, A, A])                   = NatList(p._1, p._2, p._3)
  // implicit def convertTuple4[A](p: Tuple4[A, A, A, A])                = NatList(p._1, p._2, p._3, p._4)
  // implicit def convertTuple5[A](p: Tuple5[A, A, A, A, A])             = NatList(p._1, p._2, p._3, p._4, p._5)
  // implicit def convertTuple6[A](p: Tuple6[A, A, A, A, A, A])          = NatList(p._1, p._2, p._3, p._4, p._5, p._6)
  // implicit def convertTuple7[A](p: Tuple7[A, A, A, A, A, A, A])       = NatList(p._1, p._2, p._3, p._4, p._5, p._6, p._7)
  // implicit def convertTuple8[A](p: Tuple8[A, A, A, A, A, A, A, A])    = NatList(p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8)
  // implicit def convertTuple9[A](p: Tuple9[A, A, A, A, A, A, A, A, A]) = NatList(p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8, p._9)
  //
  // Bizarrely, the mere presence of the implicits induces a crash when
  // creating a NatList, even if the implicits are not touched.
  //
  // java.lang.AssertionError: assertion failed: N#Succ
  //   at scala.reflect.internal.tpe.TypeMaps$adaptToNewRunMap$.adaptToNewRun(TypeMaps.scala:1077)
  //   at scala.reflect.internal.tpe.TypeMaps$adaptToNewRunMap$.apply(TypeMaps.scala:1121)
  //   at scala.reflect.internal.tpe.TypeMaps$adaptToNewRunMap$.apply(TypeMaps.scala:1050)

  implicit class TraversableToPsp[A](xs: GenTraversableOnce[A]) {
    def toPsp: Foreach[A] = Foreach traversable xs
  }

  implicit class JavaIteratorToPsp[A](xs: jIterator[A]) {
    def toScalaIterator = new scala.Iterator[A] {
      def next    = xs.next
      def hasNext = xs.hasNext
    }
    def toPsp: Foreach[A] = toScalaIterator.toTraversable.toPsp
  }

  implicit class JavaCollectionToPsp[A](xs: jAbstractCollection[A]) {
    def toPsp: Foreach[A] = toTraversable.toPsp
    def toTraversable: Traversable[A] = toScalaIterator.toTraversable
    def toScalaIterator = new scala.Iterator[A] {
      val it = xs.iterator
      def next    = it.next
      def hasNext = it.hasNext
    }
  }
}
