package psp

import psp.std._

package core {
  trait PackageLevel extends PspHighPriority {
    type Done               = Boolean
    type Ref[+A]            = A with AnyRef
    type Predicate2[-A, -B] = (A, B) => Boolean

    // With type aliases like these which include a type selection,
    // sometimes substitution fails and you get messages like
    // "found: Int, required: tc.A." It is bug, bug, bug city.
    // It can be worked a little bit by expanding the type
    // manually at the call sites where the bug hits (it's SI-8223).
    type AtomicView[Repr, W <: WalkableTypes]  = Env[Repr, W]#AtomicView
    type IndexedView[Repr, W <: WalkableTypes] = Env[Repr, W]#IndexedView
    type LinearView[Repr, W <: WalkableTypes]  = Env[Repr, W]#LinearView

    type Env[Repr, W <: WalkableTypes] = ViewEnvironment[W#A, Repr, W#CC]

    type ForeachableType[A0, Repr, CC0[X]] = Foreachable[Repr] {
      type A = A0
      type CC[B] = CC0[B]
    }
    type SequentialAccessType[A0, Repr, CC0[X]] = SequentialAccess[Repr] {
      type A = A0
      type CC[B] = CC0[B]
    }
    type DirectAccessType[A0, Repr, CC0[X]] = DirectAccess[Repr] {
      type A = A0
      type CC[B] = CC0[B]
    }
  }
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
