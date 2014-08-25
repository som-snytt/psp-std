package psp
package std

// import psp.std._

package core {
  trait CollectionLow {
    // aka AtomicView[Repr, tc.type] but SI-8223. Similarly for the analogous implicits.
    implicit def raiseAtomicView[Repr](repr: Repr)(implicit tc: Foreachable[Repr]): Env[Repr, tc.type]#AtomicView = tc wrap repr
  }
  trait CollectionMid extends CollectionLow {
    implicit def raiseSequentialAccessView[Repr](repr: Repr)(implicit tc: SequentialAccess[Repr]): Env[Repr, tc.type]#LinearView = tc wrap repr
  }
  trait CollectionHigh extends CollectionMid {
    implicit def raiseIndexedView[Repr](repr: Repr)(implicit tc: DirectAccess[Repr]): Env[Repr, tc.type]#IndexedView = tc wrap repr
  }
  trait PackageLevel extends CollectionHigh {
    type Done               = Boolean
    type Ref[+A]            = A with AnyRef
    type Predicate2[-A, -B] = (A, B) => Boolean

    implicit def raiseFunction1Ops[T, R](f: T => R): Function1Ops[T, R] = new Function1Ops[T, R](f)

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

package object core extends psp.std.core.PackageLevel {
  def unknownSize = SizeInfo.Unknown

  implicit def directBuilder[A] : Builds[A, Direct[A]] = Builds((xs: Foreach[A]) =>
    xs match {
      case xs: Direct[A] => xs
      case _             => Direct.elems(xs.toSeq: _*)
    }
  )

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

  implicit def implicitForeachOps[A](xs: Foreach[A]): ForeachOperations[A] = new ForeachOperations(xs)

  implicit final class IndexedExtensionOps[A](val xs: Indexed[A]) extends AnyVal {
    def apply(index: Index): A = xs elemAt index

    def zip[B](that: Indexed[B]): Indexed[(A, B)]                                                   = zipWith(that)(_ -> _)
    def zipWith[A1, B](that: Indexed[A1])(f: (A, A1) => B): Indexed[B]                              = new ZippedIndexed2(xs, that, f)
    def zipWith[A1, A2, B](that1: Indexed[A1], that2: Indexed[A2])(f: (A, A1, A2) => B): Indexed[B] = new ZippedIndexed3(xs, that1, that2, f)
  }

  implicit final class DirectExtensionOps[A](val xs: Direct[A]) extends AnyVal {
    def ++(ys: Direct[A]): Direct[A] = Direct.join(xs, ys)
  }

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
