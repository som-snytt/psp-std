package psp
package core

class ScalaCompat[+A, CC[+X]] extends scala.collection.GenSeqLike[A, CC[A]] {
  import scala.{ collection => sc }

  // Members declared in scala.Equals
  def canEqual(that: Any): Boolean = ???

  // Members declared in sc.GenIterableLike
  def iterator: Iterator[A]                                                                                                  = ???
  def sameElements[A1 >: A](that: sc.GenIterable[A1]): Boolean                                                               = ???
  def zip[A1 >: A, B, That](that: sc.GenIterable[B])(implicit bf: CBF[CC[A],(A1, B),That]): That                             = ???
  def zipAll[B, A1 >: A, That](that: sc.GenIterable[B],thisElem: A1,thatElem: B)(implicit bf: CBF[CC[A],(A1, B),That]): That = ???
  def zipWithIndex[A1 >: A, That](implicit bf: CBF[CC[A],(A1, Int),That]): That                                              = ???

  // Members declared in sc.GenSeqLike
  def :+[B >: A, That](elem: B)(implicit bf: CBF[CC[A],B,That]): That                                        = ???
  def +:[B >: A, That](elem: B)(implicit bf: CBF[CC[A],B,That]): That                                        = ???
  def apply(idx: Int): A                                                                                     = ???
  def corresponds[B](that: sc.GenSeq[B])(p: (A, B) => Boolean): Boolean                                      = ???
  def diff[B >: A](that: sc.GenSeq[B]): CC[A]                                                                = ???
  def distinct: CC[A]                                                                                        = ???
  def endsWith[B](that: sc.GenSeq[B]): Boolean                                                               = ???
  def indexWhere(p: A => Boolean,from: Int): Int                                                             = ???
  def intersect[B >: A](that: sc.GenSeq[B]): CC[A]                                                           = ???
  def lastIndexWhere(p: A => Boolean,end: Int): Int                                                          = ???
  def length: Int                                                                                            = ???
  def padTo[B >: A, That](len: Int,elem: B)(implicit bf: CBF[CC[A],B,That]): That                            = ???
  def patch[B >: A, That](from: Int,patch: sc.GenSeq[B],replaced: Int)(implicit bf: CBF[CC[A],B,That]): That = ???
  def reverse: CC[A]                                                                                         = ???
  def reverseMap[B, That](f: A => B)(implicit bf: CBF[CC[A],B,That]): That                                   = ???
  def segmentLength(p: A => Boolean,from: Int): Int                                                          = ???
  def seq: Seq[A]                                                                                            = ???
  def startsWith[B](that: sc.GenSeq[B],offset: Int): Boolean                                                 = ???
  def toSeq: sc.GenSeq[A]                                                                                    = ???
  def updated[B >: A, That](index: Int,elem: B)(implicit bf: CBF[CC[A],B,That]): That                        = ???

  // Members declared in sc.GenTraversableLike
  def ++[B >: A, That](that: sc.GenTraversableOnce[B])(implicit bf: CBF[CC[A],B,That]): That   = ???
  def collect[B, That](pf: PartialFunction[A,B])(implicit bf: CBF[CC[A],B,That]): That         = ???
  def drop(n: Int): CC[A]                                                                      = ???
  def dropWhile(pred: A => Boolean): CC[A]                                                     = ???
  def filter(pred: A => Boolean): CC[A]                                                        = ???
  def filterNot(pred: A => Boolean): CC[A]                                                     = ???
  def flatMap[B, That](f: A => sc.GenTraversableOnce[B])(implicit bf: CBF[CC[A],B,That]): That = ???
  def foreach[U](f: A => U): Unit                                                              = ???
  def groupBy[K](f: A => K): sc.GenMap[K,CC[A]]                                                = ???
  def head: A                                                                                  = ???
  def headOption: Option[A]                                                                    = ???
  def init: CC[A]                                                                              = ???
  def isTraversableAgain: Boolean                                                              = ???
  def last: A                                                                                  = ???
  def lastOption: Option[A]                                                                    = ???
  def map[B, That](f: A => B)(implicit bf: CBF[CC[A],B,That]): That                            = ???
  def partition(pred: A => Boolean): (CC[A], CC[A])                                            = ???
  def repr: CC[A]                                                                              = ???
  def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CBF[CC[A],B,That]): That         = ???
  def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CBF[CC[A],B,That]): That           = ???
  def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CBF[CC[A],B,That]): That          = ???
  def size: Int                                                                                = ???
  def slice(unc_from: Int,unc_until: Int): CC[A]                                               = ???
  def span(pred: A => Boolean): (CC[A], CC[A])                                                 = ???
  def splitAt(n: Int): (CC[A], CC[A])                                                          = ???
  def stringPrefix: String                                                                     = ???
  def tail: CC[A]                                                                              = ???
  def take(n: Int): CC[A]                                                                      = ???
  def takeWhile(pred: A => Boolean): CC[A]                                                     = ???

  // Members declared in sc.GenTraversableOnce
  def :\[B](z: B)(op: (A, B) => B): B                                              = ???
  def /:[B](z: B)(op: (B, A) => B): B                                              = ???
  def aggregate[B](z: => B)(seqop: (B, A) => B,combop: (B, B) => B): B             = ???
  def copyToArray[B >: A](xs: Array[B],start: Int,len: Int): Unit                  = ???
  def copyToArray[B >: A](xs: Array[B],start: Int): Unit                           = ???
  def copyToArray[B >: A](xs: Array[B]): Unit                                      = ???
  def count(p: A => Boolean): Int                                                  = ???
  def exists(pred: A => Boolean): Boolean                                          = ???
  def find(pred: A => Boolean): Option[A]                                          = ???
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1                                 = ???
  def foldLeft[B](z: B)(op: (B, A) => B): B                                        = ???
  def foldRight[B](z: B)(op: (A, B) => B): B                                       = ???
  def forall(pred: A => Boolean): Boolean                                          = ???
  def hasDefiniteSize: Boolean                                                     = ???
  def isEmpty: Boolean                                                             = ???
  def max[A1 >: A](implicit ord: Ordering[A1]): A                                  = ???
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A                            = ???
  def min[A1 >: A](implicit ord: Ordering[A1]): A                                  = ???
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A                            = ???
  def mkString: String                                                             = ???
  def mkString(sep: String): String                                                = ???
  def mkString(start: String,sep: String,end: String): String                      = ???
  def nonEmpty: Boolean                                                            = ???
  def product[A1 >: A](implicit num: Numeric[A1]): A1                              = ???
  def reduce[A1 >: A](op: (A1, A1) => A1): A1                                      = ???
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]                         = ???
  def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1]                        = ???
  def reduceRight[B >: A](op: (A, B) => B): B                                      = ???
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B]                        = ???
  def sum[A1 >: A](implicit num: Numeric[A1]): A1                                  = ???
  def to[Col[_]](implicit cbf: CBF[Nothing,A,Col[A @uV]]): Col[A @uV]              = ???
  def toArray[A1 >: A](implicit evidence$1: scala.reflect.ClassTag[A1]): Array[A1] = ???
  def toBuffer[A1 >: A]: sc.mutable.Buffer[A1]                                     = ???
  def toIndexedSeq: sc.immutable.IndexedSeq[A]                                     = ???
  def toIterable: sc.GenIterable[A]                                                = ???
  def toIterator: Iterator[A]                                                      = ???
  def toList: List[A]                                                              = ???
  def toMap[K, V](implicit ev: <:<[A,(K, V)]): sc.GenMap[K,V]                      = ???
  def toSet[A1 >: A]: sc.GenSet[A1]                                                = ???
  def toStream: Stream[A]                                                          = ???
  def toTraversable: sc.GenTraversable[A]                                          = ???
  def toVector: Vector[A]                                                          = ???

  // Members declared in sc.Parallelizable
  protected[this] def parCombiner: sc.parallel.Combiner[A,sc.parallel.ParSeq[A]] = ???
}
