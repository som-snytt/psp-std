package psp
package std

import api._
import StdShow._

package repl {
  trait TargetCommon[A] {
    def target: Direct[A]
    def >(implicit z: Show[A]): Direct[A]     = target doto (_ map z.show foreach println)
    def >>(implicit z: TryShow[A]): Direct[A] = target doto (_ map z.show foreach println)
    def !>(implicit z: Show[A]): Direct[A]    = target doto (xs => (xs map z.show).sorted.m foreach println)
  }

  trait ReplImportLow {
    implicit final class ReplOps[A](val target: A) {
      def >(implicit z: Show[A]): A     = target doto (x => println(z show x))
      def >>(implicit z: TryShow[A]): A = target doto (x => println(z show x))
    }
    implicit final class ReplJavaOps[A](val xs: jCollection[A])        extends TargetCommon[A] { def target = xs.m.pvec }
  }
  object ReplImport extends ReplImportLow {
    implicit final class ReplForeachOps[A](val xs: Each[A])         extends TargetCommon[A] { def target = xs.m.pvec }
    implicit final class ReplArrayOps[A](val xs: Array[A])             extends TargetCommon[A] { def target = xs.m.pvec }
    implicit final class ReplTraversableOps[A](val xs: sCollection[A]) extends TargetCommon[A] { def target = xs.m.pvec }

    implicit final class ReplMapOps[K, V](val target: ExMap[K, V]) {
      def >(implicit z1: Show[K], z2: Show[V]): ExMap[K, V]        = target doto (m => println(show"$m"))
      def >>(implicit z1: TryShow[K], z2: TryShow[V]): ExMap[K, V] = target doto (m => println(pp"$m"))
      def !>(implicit z1: Show[K], z2: Show[V]): ExMap[K, V]       = target doto (m => m !>)
    }
  }
}
