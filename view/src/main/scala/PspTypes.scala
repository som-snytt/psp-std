package psp
package core

import scala.{ collection => sc }
import sc.{ mutable => scm, generic => scg }

/** A thin abstraction over some questionable assumptions. */
trait PspTypes {
  type Done               = Boolean
  type Suspended[+A]      = (A => Unit) => Unit
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
