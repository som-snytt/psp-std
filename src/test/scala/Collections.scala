package psp
package tests

import scala.collection.immutable
import org.scalacheck._, Prop._, Gen._
import psp.std._, psp.std.core._

class Collections extends Bundle {
  def checkResult[T: ClassTag](result: Object) = assert(classTag[T].runtimeClass isAssignableFrom result.getClass)

  val xs = immutable.BitSet(1, 2, 3)

  implicit object StringIsCharSequence extends impl.DirectAccessImpl[Char, String, psp.std.core.Direct] {
    def length(repr: String): Size               = Size(repr.length)
    def elemAt(repr: String)(index: Index): Char = repr charAt index.value
  }

  def run(): Boolean = {
    checkResult[immutable.BitSet](xs.m map (_.toString.length) native)
    checkResult[immutable.BitSet](xs.m map (_.toString) map (_.length) native)
    checkResult[immutable.BitSet](xs.m map (x => Seq(x)) map (_.size) native)
    checkResult[immutable.BitSet](xs.m map (x => Seq(x).size) native)
    checkResult[String]("abc".m map (_.toInt.toChar) native)
    checkResult[String]("abc".m flatMap (_.toString * 3 m) native)
    checkResult[Array[Int]](Array[Int](1, 2, 3).m.native)
    checkResult[Array[Int]](Array[Int](1, 2, 3).m flatMap (x => Foreach elems x) native)
    checkResult[immutable.Map[_, _]](immutable.Map("a" -> 1, "b" -> 2, "c" -> 3).m.native)
    checkResult[immutable.Map[_, _]](immutable.Map("a" -> 1, "b" -> 2, "c" -> 3).m map (x => x) native)
    checkResult[Seq[_]](Seq("a" -> 1, "b" -> 2, "c" -> 3).m.native)
    checkResult[Seq[_]]((Seq("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x))).force[Seq[_]])
    checkResult[Vector[_]](Vector("a" -> 1, "b" -> 2, "c" -> 3).m.native)
    checkResult[Vector[_]]((Vector("a" -> 1, "b" -> 2, "c" -> 3).m map (_._1) map (x => (x, x))).force[Vector[_]])
    finish()
  }
}

class CollectionEquivalents(max: Size) extends ScalacheckBundle {
  def ints = Gen.choose(1, max.value)
  def preds = Gen.oneOf[Int => Boolean](
    (_: Int) % 3 == 0,
    (_: Int) => true,
    (_: Int) => false,
    (_: Int) < max.value
  )

  // def genSize: Gen[Size]         = chooseNum(1, Int.MaxValue / 2) map (n => Size(n))

  // def containerOfN[C[_],T](n: Int, g: Gen[T])(implicit
  //   evb: Buildable[T,C], evt: C[T] => Traversable[T]
  // ): Gen[C[T]] =
  //   sequence[C,T](Traversable.fill(n)(g)) suchThat { c =>
  //     // TODO: Can we guarantee c.size == n (See issue #89)?
  //     c.forall(g.sieveCopy)
  //   }

  def bundle = "CollectionEquivalents"

  def gen[A: Arbitrary] : Gen[A] = ?[Arbitrary[A]].arbitrary

  // class GenContainer {
  //   type CC[X] <: Traversable[X]
  //   type A
  //   def apply(implicit z: Builds[A, CC[A]], tag: ClassTag[CC[A]]): Gen[CC[A]] = {
  //     Gen.containerOfN[CC, A](max.value, gen[A])
  //   }
  // }

  case class NamedGen[+A](label: String, gen: Gen[A]) {
    def map[B](f: String => String, g: A => B): NamedGen[B] = NamedGen(f(label), gen map g)
  }

  def genContainer[DD[X] <: Traversable[X], CC[X] <: DD[X], A: Arbitrary](implicit z: Builds[A, CC[A]], tag: ClassTag[CC[A]]): NamedGen[DD[A]] =
    NamedGen[DD[A]](tag.toString, Gen.containerOfN[CC, A](max.value, gen[A]))

  // new GenContainer {
  //   type CC[X] = CC0[X]
  //   type A = A0

  //   def apply(implicit z: Builds[A, CC[A]], tag: ClassTag[CC[A]]): Gen[CC[A]] = {
  //     Gen.containerOfN[CC, A](max.value, gen[A])
  //   }

  // }

  // implicit def arbVector[A: Arbitrary] = genContainer[Vector](Size(100))

  // immutable/BitSet.scala
  // immutable/HashMap.scala
  // immutable/HashSet.scala
  // immutable/IndexedSeq.scala
  // immutable/IntMap.scala
  // immutable/Iterable.scala
  // immutable/LinearSeq.scala
  // immutable/List.scala
  // immutable/ListMap.scala
  // immutable/ListSet.scala
  // immutable/LongMap.scala
  // immutable/Map.scala
  // immutable/NumericRange.scala
  // immutable/Queue.scala
  // immutable/Range.scala
  // immutable/Seq.scala
  // immutable/Set.scala
  // immutable/SortedMap.scala
  // immutable/SortedSet.scala
  // immutable/Stack.scala
  // immutable/Stream.scala
  // immutable/StreamView.scala
  // immutable/Traversable.scala
  // immutable/TreeMap.scala
  // immutable/TreeSet.scala
  // immutable/Vector.scala
  // immutable/WrappedString.scala
  // IterableView.scala
  // SeqView.scala
  // TraversableView.scala

  def allSequences[A: Arbitrary]() = List[NamedGen[immutable.Seq[A]]](
    genContainer[immutable.Seq, immutable.IndexedSeq, A],
    genContainer[immutable.Seq, immutable.LinearSeq, A],
    genContainer[immutable.Seq, immutable.List, A],
    genContainer[immutable.Seq, immutable.Queue, A],
    genContainer[immutable.Seq, immutable.Seq, A],
    genContainer[immutable.Seq, immutable.Stack, A],
    genContainer[immutable.Seq, immutable.Stream, A],
    genContainer[immutable.Seq, immutable.Vector, A]
  )
  def allSets[A: Arbitrary]() = List[NamedGen[immutable.Set[A]]](
    genContainer[immutable.Set, immutable.HashSet, A],
    genContainer[immutable.Set, immutable.ListSet, A],
    genContainer[immutable.Set, immutable.Set, A]
    // genSequence[immutable.Set, immutable.SortedSet, A],
    // genSequence[immutable.Set, immutable.TreeSet, A]
  )
  def allViews[A: Arbitrary](): List[NamedGen[collection.SeqView[A, Seq[A]]]] =
    allSequences[A] map (_ map (_ + ".view", _.view))


    // (ng =>  mapSecond (_ map (_.view)))

  def allSeqsAndViews[A: Arbitrary](): List[NamedGen[scala.collection.Seq[A]]]    = (
       allSequences[A]()
    ++ allViews[A]()
    ++ {
      for (NamedGen(d1, gxs) <- allSequences[A]() ; NamedGen(d2, gys) <- allViews[A]()) yield {
        NamedGen("%s ++ %s".format(d1, d2), gxs flatMap (xs => gys map (ys => xs ++ ys)))
      }
    }
    ++ {
      for (NamedGen(d1, gxs) <- allViews[A]() ; NamedGen(d2, gys) <- allSequences[A]()) yield {
        NamedGen("%s ++ %s".format(d1, d2), gxs flatMap (xs => gys map (ys => xs ++ ys)))
      }
    }
  )

  def allContainers[A: Arbitrary](): List[NamedGen[scala.collection.Iterable[A]]] = allSeqsAndViews[A]() ++ allSets[A]()

  def newProps[CC[X] <: Traversable[X], A : Arbitrary : ClassTag](gens: Seq[NamedGen[CC[A]]])(desc: String, f: Gen[CC[A]] => Prop) = {
    val xs = gens map (ng => NamedProp(ng.label, f(ng.gen)))
    NamedProp(s"--- $desc", Prop.passed) +: xs :+ NamedProp("---", Prop.passed)
  }

  //   genSequence[immutable.IndexedSeq, A],
  //   genSequence[immutable.LinearSeq, A],
  //   genSequence[immutable.List, A],
  //   genSequence[immutable.Seq, A],
  //   genSequence[immutable.Stack, A],
  //   genSequence[immutable.Stream, A],
  //   genSequence[immutable.Vector, A]
  // )



  // genContainer[collection.SeqView][A],
  // genContainer[immutable.StreamView][A],
  // def allGenerators[A: Arbitrary] = allContainers map (c => c(Size(100), gen[A]))

  // def containerProps[A : Arbitrary : ClassTag](desc: String, f: Gen[collection.Iterable[A]] => Prop) = {
  //   def props = allContainers[A]() flatMap (ng =>
  //     Seq(
  //       NamedProp(ng.label, f(ng.gen))
  //     )
  //   )


  //   //   Seq[NamedProp](
  //   //     s ->
  //   //   )
  //   // }
  //   NamedProp(s"--- $desc", Prop.passed) +: props :+ NamedProp("---", Prop.passed)
  // }

  type cIterable[+A] = scala.collection.Iterable[A]
  type cSeq[+A]      = scala.collection.Seq[A]

  def true1[CC[X] <: cIterable[X], A](xs: CC[A], n: Int)          = (xs take n) == (xs splitAt n)._1
  def true2[CC[X] <: cIterable[X], A](xs: CC[A], n: Int)          = (xs drop n) == (xs splitAt n)._2
  def true3[CC[X] <: cSeq[X], A](xs: CC[A], n: Int, m: Int)       = (xs drop n take m) == xs.slice(n, n + m)
  def true4[CC[X] <: cIterable[X], A](xs: CC[A], p: A => Boolean) = (xs filter p) == (xs partition p)._1
  def true5[CC[X] <: cIterable[X], A](xs: CC[A], p: A => Boolean) = (xs filterNot p) == (xs partition p)._2
  def true6[CC[X] <: cIterable[X], A](xs: CC[A], p: A => Boolean) = (xs takeWhile p) == (xs span p)._1
  def true7[CC[X] <: cIterable[X], A](xs: CC[A], p: A => Boolean) = (xs dropWhile p) == (xs span p)._2
  def true8[CC[X] <: cSeq[X], A](xs: CC[A])                       = xs.reverse.reverse == xs

  val props = (
       newProps[cIterable, Int](allContainers[Int]())("(xs take n) == (xs splitAt n)._1", forAll(_, ints)(true1))
    ++ newProps[cIterable, Int](allContainers[Int]())("(xs drop n) == (xs splitAt n)._2", forAll(_, ints)(true2))
    ++ newProps[cSeq, Int](allSeqsAndViews[Int]())("(xs drop n take m) == xs.slice(n, n + m)", forAll(_, ints, ints)(true3))
    ++ newProps[cIterable, Int](allContainers[Int]())("(xs filter p) == (xs partition p)._1", forAll(_, preds)(true4))
    ++ newProps[cIterable, Int](allContainers[Int]())("(xs filterNot p) == (xs partition p)._2", forAll(_, preds)(true5))
    ++ newProps[cIterable, Int](allContainers[Int]())("(xs takeWhile p) == (xs span p)._1", forAll(_, preds)(true6))
    ++ newProps[cIterable, Int](allContainers[Int]())("(xs dropWhile p) == (xs span p)._2", forAll(_, preds)(true7))
    ++ newProps[cSeq, Int](allSeqsAndViews[Int]())("xs.reverse.reverse == xs", forAll(_)(true8))
  )




  //      containerProps("(xs take n) == (xs splitAt n)._1" , forAll(_: Gen[collection.Iterable[Int]], genIntToMax)((xs, n) => (xs take n) == (xs splitAt n)._1))
  //   ++ containerProps("(xs drop n take m) == xs.slice(n, n + m)" , forAll(_: Gen[collection.Iterable[Int]], genIntToMax, genIntToMax)((xs, n, m) => (xs drop n take m) == xs.slice(n, n + m)))
  // )

  // allContainers[Int]() flatMap {
  //   case (s, g) =>
  //     Seq[NamedProp](
  //       "(xs take n) == (xs splitAt n)._1" -> s, forAll(g, genInt)((xs, n) => (xs take n) == (xs splitAt n)._1)))
  //     )

  //      (for ((s, g) <- allContainers[Int]()) yield NamedProp(s, forAll(g, genInt)((xs, n) => (xs take n) == (xs splitAt n)._1)))
  //   // ++ (allSequences[Int]() map (g => (g.toString -> forAll(g, genInt)((xs, n) => (xs drop n) == (xs splitAt n)._2): NamedProp)))
  // )


  // Seq[NamedProp](
  //   "take == splitAt._1" -> forAll((xs: Vector[Int], n: Int) => (xs take n) == (xs splitAt n)._1)
  // )

  // def genCollection[CC[X]](max: Size) = new {
  //   def apply[A: Gen](implicit z: Builds[A, CC[A]]): Gen[CC[A]] = {
  //     val sizegen = chooseNum(0, max) map (n => Size(n))

  //   }
  // }

  // drop n take m == slice(n, n + m)
  // splitAt n == (take n, drop n)
  // span p == (takeWhile p, dropWhile p)
  // partition p == (filter p, filterNot p)

}
