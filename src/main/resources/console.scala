import scala.collection.{ mutable => scm, immutable => sci }
import java.nio.{ file => jnf }
import psp.std._, ansi._, api._

implicit final class ReplForeachOps[A](val target: Foreach[A]) {
  def !> : Unit                         = println(target mkString EOL)
  def  >(implicit shows: Show[A]): Unit = println(target.joinLines)
}

implicit final class ReplOps[A](val target: A) {
  def !> : Unit                        = println(target)
  def >(implicit shows: Show[A]): Unit = println(target.to_s)
}

import spire.algebra._, spire.math._, com.google.common.collect._
