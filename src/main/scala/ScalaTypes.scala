package psp
package core

import scala.{ collection => sc }
import sc.{ mutable => scm, generic => scg }

trait JavaTypes {
  type jHashSet[A]            = java.util.HashSet[A]
  type LinkedBlockingQueue[A] = java.util.concurrent.LinkedBlockingQueue[A]
  type BlockingQueue[A]       = java.util.concurrent.BlockingQueue[A]
  type SynchronousQueue[A]    = java.util.concurrent.SynchronousQueue[A]
}

/** It's kind of funny I guess.
 */
trait ScalaShadowImplicits {
  val wrapByteArray, wrapShortArray, wrapCharArray, wrapIntArray, wrapLongArray, wrapFloatArray, wrapDoubleArray = null
  val byteArrayOps, shortArrayOps, charArrayOps, intArrayOps, longArrayOps, floatArrayOps, doubleArrayOps        = null
  val byteWrapper, shortWrapper, charWrapper, intWrapper, longWrapper, floatWrapper, doubleWrapper               = null
  val wrapString, unwrapString, augmentString, unaugmentString                                                   = null
  val StringAdd, ArrowAssoc                                                                                      = null
  val genericArrayOps, genericWrapArray                                                                          = null
}

trait ScalaTypes {
  type tailrec      = scala.annotation.tailrec
  type uV           = scala.annotation.unchecked.uncheckedVariance
  type IdFun[A]     = A => A
  type =?> [-A, +B] = PartialFunction[A, B]

  type Traversable[+A]                                             = sc.Traversable[A]
  type GenTraversableOnce[+A]                                      = sc.GenTraversableOnce[A]
  type GenTraversable[+A]                                          = sc.GenTraversable[A]
  type GenIterable[+A]                                             = sc.GenIterable[A]
  type GenTraversableLike[+A, +Repr]                               = sc.GenTraversableLike[A, Repr]
  type TraversableLike[+A, +Repr]                                  = sc.TraversableLike[A, Repr]
  type IterableLike[+A, +Repr]                                     = sc.IterableLike[A, Repr]
  type SeqLike[+A, +Repr]                                          = sc.SeqLike[A, Repr]
  type Builder[-Elem, +To]                                         = scm.Builder[Elem, To]
  type ArrayBuilder[T]                                             = scm.ArrayBuilder[T]
  type WrappedArray[A]                                             = scm.WrappedArray[A]
  type ArrayBuffer[A]                                              = scm.ArrayBuffer[A]
  type IsSeqLike[Repr]                                             = scg.IsSeqLike[Repr]
  type IsTraversableLike[Repr]                                     = scg.IsTraversableLike[Repr]
  type GenericCompanion[+CC[X] <: GenTraversable[X]]               = scg.GenericCompanion[CC]
  type GenericTraversableTemplate[+A, +CC[X] <: GenTraversable[X]] = scg.GenericTraversableTemplate[A, CC]
  type FilterMonadic[+A, +Repr]                                    = scg.FilterMonadic[A, Repr]
  type CanBuildFrom[-From, -Elem, +To]                             = scg.CanBuildFrom[From, Elem, To]
  type ScalaNumber                                                 = scala.math.ScalaNumber
  type ClassTag[A]                                                 = scala.reflect.ClassTag[A]
}
