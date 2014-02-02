package psp
package core

import scala.{ collection => sc }
import sc.{ mutable => scm, generic => scg }

/** A thin abstraction over some questionable assumptions. */
trait PspTypes extends PspJavaTypes with PspScalaTypes {
  type Index              = Int
  type Done               = Boolean
  type Suspended[+A]      = (A => Unit) => Unit
  type Ref[+A]            = A with AnyRef
  type Predicate[-A]      = A => Boolean
  type Predicate2[-A, -B] = (A, B) => Boolean

  val MaxIndex = Int.MaxValue
  val NoIndex  = -1
  val EOL      = sys.props.getOrElse("line.separator", "\n")

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

trait PspJavaTypes {
  type jPath                  = java.nio.file.Path
  type jUri                   = java.net.URI
  type jUrl                   = java.net.URL
  type jFile                  = java.io.File
  type jClass[A]              = java.lang.Class[A]
  type jHashSet[A]            = java.util.HashSet[A]
  type LinkedBlockingQueue[A] = java.util.concurrent.LinkedBlockingQueue[A]
  type BlockingQueue[A]       = java.util.concurrent.BlockingQueue[A]
  type SynchronousQueue[A]    = java.util.concurrent.SynchronousQueue[A]
}

trait PspScalaTypes {
  type tailrec      = scala.annotation.tailrec
  type uV           = scala.annotation.unchecked.uncheckedVariance
  type IdFun[A]     = A => A
  type =?> [-A, +B] = PartialFunction[A, B]

  type GenTraversableOnce[+A]          = sc.GenTraversableOnce[A]
  type Builder[-Elem, +To]             = scm.Builder[Elem, To]
  type WrappedArray[A]                 = scm.WrappedArray[A]
  type ArrayBuffer[A]                  = scm.ArrayBuffer[A]
  type CanBuildFrom[-From, -Elem, +To] = scg.CanBuildFrom[From, Elem, To]
  type ScalaNumber                     = scala.math.ScalaNumber
  type ClassTag[A]                     = scala.reflect.ClassTag[A]
}
