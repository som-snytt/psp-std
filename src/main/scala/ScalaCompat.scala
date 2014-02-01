package psp
package core

import scala.{ collection => sc }
import sc.generic.{ CanBuildFrom => CBF }
import impl._

object ToScala {
  def apply[A](xs: Foreach[A]): Traversable[A] = new ForeachAsTraversable[A](xs)
}
object FromScala {
  def apply[A](xs: sc.IndexedSeq[A]): Indexed[A] = new ScalaIndexedSeqAsIndexed[A](xs)
  def apply[A](xs: Traversable[A]): Foreach[A] = xs match {
    case xs: sc.IndexedSeq[A] => apply(xs)
    case _                    => new TraversableAsForeach[A](xs)
  }
}

object ScalaCompat {
  def apply[Repr](xs: Repr)(implicit tc: Foreachable[Repr]): CompatSeq[tc.A, Repr, tc.CC] = new CompatSeqImpl[tc.A, Repr, tc.CC](xs)(tc)
}

final class CompatSeqImpl[A, Repr, CC[X]](val repr: Repr)(implicit val tc: ForeachableType[A, Repr, CC]) extends CompatSeq[A, Repr, CC] {
  protected[this] def newBuilder: Builder[A, Repr]    = ???//tc.nativeBuilder
  protected[this] def thisCollection: CC[A]           = toCollection(repr)
  protected[this] def toCollection(repr: Repr): CC[A] = ???
}

trait CompatSeq[A, Repr, CC[X]] extends sc.GenSeqLike[A, Repr] {
  def repr: Repr

  val tc: ForeachableType[A, Repr, CC]

  protected[this] def newBuilder: Builder[A, Repr]
  private[this] def forxs[A1 >: A]: Foreach[A1]                      = tc wrap repr
  private[this] def wrap(xs: Repr): AtomicView[A, Repr, CC]          = tc wrap xs
  private[this] def wrapOp(f: api.View[A] => Foreach[A]): Repr      = f(wrap(repr)).foldl(newBuilder)(_ += _).result
  private[this] def wrapMap[B](f: api.View[A] => Foreach[B]): CC[B] = ???

  // Members declared in scala.Equals
  def canEqual(that: Any): Boolean = ???

  // Members declared in sc.GenIterableLike
  def iterator: Iterator[A]                                                                                                 = toIterable.toIterator
  def sameElements[A1 >: A](that: sc.GenIterable[A1]): Boolean                                                              = ???
  def zip[A1 >: A, B, That](that: sc.GenIterable[B])(implicit bf: CBF[Repr,(A1, B),That]): That                             = ???
  def zipAll[B, A1 >: A, That](that: sc.GenIterable[B],thisElem: A1,thatElem: B)(implicit bf: CBF[Repr,(A1, B),That]): That = ???
  def zipWithIndex[A1 >: A, That](implicit bf: CBF[Repr,(A1, Int),That]): That                                              = ???

  // Members declared in sc.GenSeqLike
  def :+[B >: A, That](elem: B)(implicit bf: CBF[Repr,B,That]): That                                        = ???
  def +:[B >: A, That](elem: B)(implicit bf: CBF[Repr,B,That]): That                                        = ???
  def apply(idx: Int): A                                                                                    = ???
  def corresponds[B](that: sc.GenSeq[B])(p: (A, B) => Boolean): Boolean                                     = ???
  def diff[B >: A](that: sc.GenSeq[B]): Repr                                                                = ???
  def distinct: Repr                                                                                        = ???
  def endsWith[B](that: sc.GenSeq[B]): Boolean                                                              = ???
  def indexWhere(p: Predicate[A],from: Int): Int                                                            = { forxs.foreachWithIndex((x, i) => if (i >= from && p(x)) return i else false) ; NoIndex }
  def intersect[B >: A](that: sc.GenSeq[B]): Repr                                                           = ???
  def lastIndexWhere(p: Predicate[A],end: Int): Int                                                         = ???
  def length: Int                                                                                           = ???
  def padTo[B >: A, That](len: Int,elem: B)(implicit bf: CBF[Repr,B,That]): That                            = ???
  def patch[B >: A, That](from: Int,patch: sc.GenSeq[B],replaced: Int)(implicit bf: CBF[Repr,B,That]): That = ???
  def reverse: Repr                                                                                         = ???
  def reverseMap[B, That](f: A => B)(implicit bf: CBF[Repr,B,That]): That                                   = ???
  def segmentLength(p: Predicate[A],from: Int): Int                                                         = ???
  def seq: Seq[A]                                                                                           = toSeq
  def startsWith[B](that: sc.GenSeq[B],offset: Int): Boolean                                                = ???
  def toSeq: Seq[A]                                                                                         = toList
  def updated[B >: A, That](index: Int,elem: B)(implicit bf: CBF[Repr,B,That]): That                        = ???

  // Members declared in sc.GenTraversableLike
  def ++[B >: A, That](that: sc.GenTraversableOnce[B])(implicit bf: CBF[Repr,B,That]): That   = bf() ++= forxs.trav ++= that.seq result
  def collect[B, That](pf: PartialFunction[A,B])(implicit bf: CBF[Repr,B,That]): That         = bf() ++= (forxs collect pf toTraversable) result
  def drop(n: Int): Repr                                                                      = wrapOp(_ drop n)
  def dropWhile(pred: Predicate[A]): Repr                                                     = wrapOp(_ dropWhile pred)
  def filter(pred: Predicate[A]): Repr                                                        = wrapOp(_ filter pred)
  def filterNot(pred: Predicate[A]): Repr                                                     = wrapOp(_ filterNot pred)
  def flatMap[B, That](f: A => sc.GenTraversableOnce[B])(implicit bf: CBF[Repr,B,That]): That = forxs.foldl(bf())((res, x) => res ++= f(x).seq).result
  def foreach[U](f: A => U): Unit                                                             = forxs foreach (x => f(x))
  def groupBy[K](f: A => K): sc.GenMap[K,Repr]                                                = ???
  def head: A                                                                                 = { foreach(return _) ; sys.error("empty.head") }
  def headOption: Option[A]                                                                   = if (isEmpty) None else Some(head)
  def init: Repr                                                                              = ???
  def isTraversableAgain: Boolean                                                             = true
  def last: A                                                                                 = ???
  def lastOption: Option[A]                                                                   = if (isEmpty) None else Some(last)
  def map[B, That](f: A => B)(implicit bf: CBF[Repr,B,That]): That                            = forxs.foldl(bf())((res, x) => res += f(x)).result
  def partition(pred: Predicate[A]): (Repr, Repr)                                             = ???
  def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CBF[Repr,B,That]): That         = scanLeft(z)(op)
  def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CBF[Repr,B,That]): That           = (bf() ++= forxs.scanl(z)(op).trav).result
  def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CBF[Repr,B,That]): That          = ???
  def size: Int                                                                               = forxs.sizeInfo.precisely getOrElse Int.MaxValue
  def slice(unc_from: Int,unc_until: Int): Repr                                               = wrapOp(_ slice (unc_from, unc_until))
  def span(pred: Predicate[A]): (Repr, Repr)                                                  = takeWhile(pred) -> dropWhile(pred)
  def splitAt(n: Int): (Repr, Repr)                                                           = take(n) -> drop(n)
  def stringPrefix: String                                                                    = "Compat"
  def tail: Repr                                                                              = if (isEmpty) sys.error("empty.tail") else drop(1)
  def take(n: Int): Repr                                                                      = wrapOp(_ take n)
  def takeWhile(pred: Predicate[A]): Repr                                                     = wrapOp(_ takeWhile pred)

  // Members declared in sc.GenTraversableOnce
  def :\[B](z: B)(op: (A, B) => B): B                                              = forxs.foldr(z)(op)
  def /:[B](z: B)(op: (B, A) => B): B                                              = forxs.foldl(z)(op)
  def aggregate[B](z: => B)(seqop: (B, A) => B,combop: (B, B) => B): B             = forxs.foldl(z)(seqop)
  def copyToArray[B >: A](xs: Array[B],start: Int,len: Int): Unit                  = forxs.foreachWithIndex((x, i) => { xs(start + i) = x ; i >= len })
  def copyToArray[B >: A](xs: Array[B],start: Int): Unit                           = copyToArray[B](xs, start, xs.length - start)
  def copyToArray[B >: A](xs: Array[B]): Unit                                      = copyToArray[B](xs, 0, xs.length)
  def count(p: Predicate[A]): Int                                                  = forxs.foldl(0)((res, x) => if (p(x)) res + 1 else res)
  def exists(pred: Predicate[A]): Boolean                                          = forxs exists pred
  def find(pred: Predicate[A]): Option[A]                                          = forxs find pred
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1                                 = forxs.foldl(z)(op)
  def foldLeft[B](z: B)(op: (B, A) => B): B                                        = forxs.foldl(z)(op)
  def foldRight[B](z: B)(op: (A, B) => B): B                                       = forxs.foldr(z)(op)
  def forall(pred: Predicate[A]): Boolean                                          = forxs forall pred
  def hasDefiniteSize: Boolean                                                     = forxs.sizeInfo.isFinite
  def isEmpty: Boolean                                                             = forxs.sizeInfo.isZero
  def max[A1 >: A](implicit ord: Ordering[A1]): A                                  = forxs[A1].max.castTo[A]
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A                            = forxs.max(implicitly[Order[B]] by f)
  def min[A1 >: A](implicit ord: Ordering[A1]): A                                  = forxs[A1].min.castTo[A]
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A                            = forxs.min(implicitly[Order[B]] by f)
  def mkString: String                                                             = mkString("")
  def mkString(sep: String): String                                                = forxs mk_s sep
  def mkString(start: String,sep: String,end: String): String                      = s"""$start${mkString("")}$end"""
  def nonEmpty: Boolean                                                            = !isEmpty
  def product[A1 >: A](implicit num: Numeric[A1]): A1                              = forxs[A1].product
  def reduce[A1 >: A](op: (A1, A1) => A1): A1                                      = forxs reduce op
  def reduceLeft[B >: A](op: (B, A) => B): B                                       = forxs reduce op
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]                         = Some(reduceLeft(op))
  def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1]                        = Some(reduce(op))
  def reduceRight[B >: A](op: (A, B) => B): B                                      = forxs[A].reverse.reduce[B]((x, y) => op(y, x))
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B]                        = Some(reduceRight(op))
  def sum[A1 >: A](implicit num: Numeric[A1]): A1                                  = forxs[A1].sum
  def to[Col[_]](implicit cbf: CBF[Nothing,A,Col[A @uV]]): Col[A @uV]              = forxs.to[Col]
  def toArray[A1 >: A](implicit evidence$1: scala.reflect.ClassTag[A1]): Array[A1] = forxs.toIterable.toArray
  def toBuffer[A1 >: A]: sc.mutable.Buffer[A1]                                     = forxs.toIterable.toBuffer
  def toIndexedSeq: sc.immutable.IndexedSeq[A]                                     = forxs.to[sc.immutable.IndexedSeq]
  def toIterable: sc.GenIterable[A]                                                = forxs.to[sc.GenIterable]
  def toIterator: Iterator[A]                                                      = forxs.to[Iterator]
  def toList: List[A]                                                              = forxs.toList
  def toMap[K, V](implicit ev: A <:< (K, V)): sc.GenMap[K,V]                       = toIterable.toMap
  def toSet[A1 >: A]: sc.GenSet[A1]                                                = toIterable.toSet
  def toStream: Stream[A]                                                          = forxs.toStream
  def toTraversable: sc.Traversable[A]                                             = forxs.trav
  def toVector: Vector[A]                                                          = forxs.toVector

  // Members declared in sc.Parallelizable
  protected[this] def parCombiner: sc.parallel.Combiner[A,sc.parallel.ParSeq[A]] = ???
}
