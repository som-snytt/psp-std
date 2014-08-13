package psp
package core
package impl

import psp.std._

trait PspUtility {
  def join(sep: String)(xs: Any*): String = xs mkString sep
  def andTrue(x: Unit): Boolean           = true
  def andFalse(x: Unit): Boolean          = false
  def nullAs[A] : A                       = (null: AnyRef).castTo[A]
  def decodeName(s: String): String       = scala.reflect.NameTransformer.decode(s)
  def log(msg: String): Unit              = Console.err println msg

  def timed[A](body: => A): A = {
    val start = System.nanoTime
    try body finally log("Elapsed: %.3f ms" format (System.nanoTime - start) / 1e6)
  }
}
