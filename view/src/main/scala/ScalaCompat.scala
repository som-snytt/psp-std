package psp
package core

import scala.{ collection => sc }
import sc.{ GenIterable => GIterable, GenSeq => GSeq }
import impl._
import psp.std._

object ToScala {
  def apply[A](xs: Foreach[A]): Traversable[A] = new ForeachAsTraversable[A](xs)
}
object FromScala {
  def apply[A](xs: sc.IndexedSeq[A]): Direct[A] = new ScalaIndexedSeqAsIndexed[A](xs)
  def apply[A](xs: GenTraversableOnce[A]): Foreach[A] = xs match {
    case xs: sc.IndexedSeq[A] => apply(xs)
    case _                    => new TraversableAsForeach[A](xs.toTraversable.seq)
  }
}

/*****

object ScalaCompat {
  def apply[Repr](xs: Repr)(implicit tc: DirectAccess[Repr]): CompatSeq[tc.A, Repr, tc.CC] = new CompatSeq[tc.A, Repr, tc.CC](xs)(tc)
}

trait SeqBuilderMethods[Repr] {
  val tc: DirectAccess[Repr]
  import tc.A

  def repr: Repr
  def reverse: Repr
  def iterator: Iterator[A]

  protected[this] type CBF[B, That]                                                                    = CanBuildFrom[Repr, B, That]
  protected[this] def xs: Direct[A]                                                                   = tc wrap repr
  protected[this] def pspBuild[B, That](cbf: CBF[B, That]): Builds[B, That]                            = Builds wrap cbf
  protected[this] def buildThat[B, That](xs: Foreach[B])(implicit bf: CBF[B, That]): That              = pspBuild(bf) build xs
  protected[this] def buildThat[B, That](xss: GenTraversableOnce[B]*)(implicit bf: CBF[B, That]): That = pspBuild(bf).build(xss: _*)

  def :+[B >: A, That](elem: B)(implicit bf: CBF[B,That]): That                           = buildThat(iterator, Iterator(elem))
  def +:[B >: A, That](elem: B)(implicit bf: CBF[B,That]): That                           = buildThat(Iterator(elem), iterator)
  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CBF[B, That]): That         = buildThat(xs.size.toIndexed map { case `index` => elem ; case idx => (tc elemAt repr)(idx) })
  def reverseMap[B, That](f: A => B)(implicit bf: CBF[B, That]): That                     = buildThat(xs.reverse map f)
  def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CBF[B, That]): That      = buildThat(xs ++ that.toPsp)
  def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CBF[B, That]): That        = xs collect pf force
  def flatMap[B, That](f: A => sc.GenTraversableOnce[B])(implicit bf: CBF[B, That]): That = buildThat(xs.toSeq map f: _*)
  def map[B, That](f: A => B)(implicit bf: CBF[B, That]): That                            = buildThat(xs.toTraversable map f)
  def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CBF[B, That]): That         = scanLeft(z)(op)
  def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CBF[B, That]): That           = buildThat(xs.scanl(z)(op))
  def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CBF[B, That]): That          = buildThat(xs.reverse.scanl(z)(op.swap).reverse)
}

final class CompatSeq[A, Repr, CC[X]](val repr: Repr)(implicit val tc: DirectAccessType[A, Repr, CC]) extends sc.GenSeqLike[A, Repr] with SeqBuilderMethods[Repr] {
  protected[this] def newBuilder: Builder[A, Repr]         = tc.nativeBuilder
  protected[this] def genericBuilder[B]: Builder[B, CC[B]] = tc.genericBuilder[B]
  protected[this] def thisCollection: CC[A]                = toCollection(repr)
  protected[this] def thisView: api.View[A]                = xs.m
  protected[this] def toCollection(repr: Repr): CC[A]      = tc.genericBuilder[A] ++= wrap(repr).toSeq result
  private[this] def fail(msg: String): Nothing             = sys.error(msg)

  private[this] def pspSize: Size                                       = tc length repr
  private[this] def intSize: Int                                        = pspSize.value
  private[this] def asIndexed: Direct[A]                               = Direct.pure(pspSize, idx => tc.elemAt(repr)(idx))
  private[this] def xsUp[A1 >: A] : Direct[A1]                         = xs
  private[this] implicit def wrap(xs: Repr): IndexedView[Repr, tc.type] = tc wrap xs
  private[this] def wrapOp(f: api.View[A] => Foreach[A]): Repr          = buildNative(f(xs.m))
  private[this] def wrapGen(f: api.View[A] => Foreach[A]): Vector[A]    = Vector.newBuilder[A] ++= f(xs.m).trav result
  private[this] def buildNative(xs: Foreach[A]): Repr                   = newBuilder ++= xs.toTraversable result

  // Members declared in scala.Equals
  def canEqual(that: Any): Boolean = that.isInstanceOf[GSeq[_]]

  // Members declared in sc.GenIterableLike
  def reverseIterator: Iterator[A]                                                    = xs.size.reverseInterval.scalaIterator map apply
  def iterator: Iterator[A]                                                           = xs.scalaIterator
  def sameElements[A1 >: A](that: GIterable[A1]): Boolean                             = (iterator corresponds that.iterator)(_ == _)
  def zip[A1 >: A, B, That](that: GIterable[B])(implicit bf: CBF[(A1, B),That]): That = buildThat(iterator zip that.iterator)
  def zipAll[B, A1 >: A, That](that: GIterable[B], thisElem: A1, thatElem: B)(implicit bf: CBF[(A1, B), That]): That = {
    val it1 = this.iterator
    val it2 = that.iterator
    val buf = bf()
    while (it1.hasNext || it2.hasNext) {
      val x = if (it1.hasNext) it1.next else thisElem
      val y = if (it2.hasNext) it2.next else thatElem
      buf += ((x, y))
    }
    buf.result
  }
  def zipWithIndex[A1 >: A, That](implicit bf: CBF[(A1, Int),That]): That                            = buildThat(iterator zip (Iterator from 0))
  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CBF[B, That]): That                        = (Builds wrap bf) build (iterator, Iterator.fill(len - intSize)(elem))
  def patch[B >: A, That](from: Int, patch: GSeq[B], replaced: Int)(implicit bf: CBF[B, That]): That = buildThat(wrapGen(_ take from), patch.seq, wrapGen(_ drop from + replaced))

  // Members declared in sc.GenSeqLike
  def apply(idx: Int): A                                           = (tc elemAt repr)(idx)
  def corresponds[B](that: GSeq[B])(p: (A, B) => Boolean): Boolean = (iterator corresponds that.iterator)(p)
  def diff[B >: A](that: GSeq[B]): Repr                            = this filterNot that.toSet

  def distinct: Repr = {
    val seen =  new jHashSet[A]
    foldLeft(newBuilder)((buf, x) => if (seen contains x) buf else try buf +=x finally seen add x).result
  }
  def groupBy[K](f: A => K): sc.immutable.Map[K, Repr] = {
    val m = sc.mutable.Map[K, Builder[A, Repr]]() withDefault (_ => newBuilder)
    this foreach (x => m(f(x)) += x)
    m.toMap map { case (k, v) => (k, v.result) }
  }

  def endsWith[B](that: GSeq[B]): Boolean               = (this.size >= that.size) && ((xs takeRight that.size).toSeq sameElements that)
  def indexWhere(p: Predicate[A],from: Int): Int        = xs.size.toIndexed drop from find (idx => p(apply(idx))) getOrElse NoIndex
  def intersect[B >: A](that: GSeq[B]): Repr            = buildNative(filter(that.toSet))
  def lastIndexWhere(p: Predicate[A],end: Int): Int     = xs.size.reverseInterval drop (intSize - end) find (idx => p(apply(idx))) getOrElse NoIndex
  def segmentLength(p: Predicate[A], from: Int): Int    = (this drop from takeWhile p).sizeInfo.preciseIntSize
  def startsWith[B](that: GSeq[B],offset: Int): Boolean = (this drop offset take that.size).trav.toIterator sameElements that.iterator

  def head: A                     = if (isEmpty) failEmpty("head") else apply(0)
  def last: A                     = if (isEmpty) failEmpty("last") else apply(pspSize.lastIndex)
  def tail: Repr                  = if (isEmpty) failEmpty("tail") else drop(1)
  def init: Repr                  = if (isEmpty) failEmpty("init") else dropRight(1)
  def headOption: Option[A]       = if (isEmpty) None else Some(head)
  def lastOption: Option[A]       = if (isEmpty) None else Some(last)
  def isTraversableAgain: Boolean = true
  def length: Int                 = intSize
  def seq: Seq[A]                 = toSeq
  def size: Int                   = length
  def stringPrefix: String        = "Compat"
  def hasDefiniteSize: Boolean    = xs.sizeInfo.isFinite
  def isEmpty: Boolean            = xs.sizeInfo.isZero
  def nonEmpty: Boolean           = !isEmpty


  def foreach[U](f: A => U): Unit = xs foreach (x => f(x))

  def partition(pred: Predicate[A]): (Repr, Repr) = filter(pred)    -> filterNot(pred)
  def span(pred: Predicate[A]): (Repr, Repr)      = takeWhile(pred) -> dropWhile(pred)
  def splitAt(n: Int): (Repr, Repr)               = take(n)         -> drop(n)

  def drop(n: Int): Repr                  = wrapOp(_ drop n)
  def dropRight(n: Int): Repr             = wrapOp(_ dropRight n)
  def dropWhile(pred: Predicate[A]): Repr = wrapOp(_ dropWhile pred)
  def filter(pred: Predicate[A]): Repr    = wrapOp(_ filter pred)
  def filterNot(pred: Predicate[A]): Repr = wrapOp(_ filterNot pred)
  def slice(start: Int, end: Int): Repr   = wrapOp(_ slice indexRange(start, end))
  def take(n: Int): Repr                  = wrapOp(_ take n)
  def takeRight(n: Int): Repr             = wrapOp(_ takeRight n)
  def takeWhile(pred: Predicate[A]): Repr = wrapOp(_ takeWhile pred)
  def reverse: Repr                       = wrapOp(_.reverse)
  def withFilter(p: Predicate[A]): Repr   = wrapOp(_ withFilter p)

  def :\[B](z: B)(op: (A, B) => B): B                                          = xs.foldr(z)(op)
  def /:[B](z: B)(op: (B, A) => B): B                                          = xs.foldl(z)(op)
  def aggregate[B](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B        = xs.foldl(z)(seqop)
  def copyToArray[B >: A](xs: Array[B],start: Int,len: Int): Unit              = this.xs.foreachWithIndex((x, i) => { xs(start + i) = x ; i >= len })
  def copyToArray[B >: A](xs: Array[B],start: Int): Unit                       = copyToArray[B](xs, start, xs.length - start)
  def copyToArray[B >: A](xs: Array[B]): Unit                                  = copyToArray[B](xs, 0, xs.length)
  def count(p: Predicate[A]): Int                                              = filter(p).sizeInfo.preciseIntSize
  def exists(pred: Predicate[A]): Boolean                                      = xs exists pred
  def find(pred: Predicate[A]): Option[A]                                      = xs find pred
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1                             = xs.foldl(z)(op)
  def foldLeft[B](z: B)(op: (B, A) => B): B                                    = xs.foldl(z)(op)
  def foldRight[B](z: B)(op: (A, B) => B): B                                   = xs.foldr(z)(op)
  def forall(pred: Predicate[A]): Boolean                                      = xs forall pred
  def max[A1 >: A](implicit ord: Ordering[A1]): A                              = xsUp[A1].max.castTo[A]
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A                        = xs max (Order[B] by f)
  def min[A1 >: A](implicit ord: Ordering[A1]): A                              = xsUp[A1].min.castTo[A]
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A                        = xs min (Order[B] by f)
  def mkString: String                                                         = xs mk_s ""
  def mkString(sep: String): String                                            = xs mk_s sep
  def mkString(start: String, sep: String, end: String): String                = pp"$start$mkString$end"
  def product[A1 >: A](implicit num: Numeric[A1]): A1                          = xsUp[A1].product
  def reduce[A1 >: A](op: (A1, A1) => A1): A1                                  = xs reduce op
  def reduceLeft[B >: A](op: (B, A) => B): B                                   = xs reduce op
  def reduceRight[B >: A](op: (A, B) => B): B                                  = xs.reverse reduceLeft[B] op.swap
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]                     = Some(reduceLeft(op))
  def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1]                    = Some(reduce(op))
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B]                    = Some(reduceRight(op))
  def sum[A1 >: A](implicit num: Numeric[A1]): A1                              = xsUp[A1].sum
  def to[Col[_]](implicit cbf: CanBuildFrom[Nothing,A,Col[A @uV]]): Col[A @uV] = xs.to[Col]
  def toArray[A1 >: A](implicit ev: ClassTag[A1]): Array[A1]                   = toIterable.toArray
  def toBuffer[A1 >: A]: sc.mutable.Buffer[A1]                                 = toIterable.toBuffer
  def toIndexedSeq: sc.immutable.IndexedSeq[A]                                 = toVector
  def toIterable: GIterable[A]                                                 = to[GIterable](Iterable.canBuildFrom)
  def toIterator: Iterator[A]                                                  = iterator
  def toList: List[A]                                                          = xs.toList
  def toMap[K, V](implicit ev: A <:< (K, V)): sc.GenMap[K,V]                   = toIterable.toMap
  def toSeq: Seq[A]                                                            = toList
  def toSet[A1 >: A]: sc.GenSet[A1]                                            = toIterable.toSet
  def toStream: Stream[A]                                                      = xs.toStream
  def toTraversable: sc.Traversable[A]                                         = xs.trav
  def toVector: Vector[A]                                                      = xs.toVector

  // Members declared in sc.Parallelizable
  protected[this] def parCombiner: sc.parallel.Combiner[A,sc.parallel.ParSeq[A]] = sc.parallel.ParSeq.newCombiner[A]

  override def toString = pp"CompatSeq($repr)"
}

*****/
