package psp
package demo

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

/** Self-contained **/

final class Pipeline[+A](val mf: (A => Unit) => Unit) extends AnyVal {
  def foreach(f: A => Unit): Unit                        = mf(f)
  def filter(p: A => Boolean): Pipeline[A]               = withFilter(p)
  def withFilter(p: A => Boolean): Pipeline[A]           = Pipeline(g => foreach(x => if (p(x)) g(x)))
  def map[B](f: A => B): Pipeline[B]                     = Pipeline(g => foreach(x => g(f(x))))
  def flatMap[B](f: A => Pipeline[B]): Pipeline[B]       = Pipeline(g => foreach(x => f(x) foreach g))
  def collect[B](pf: PartialFunction[A, B]): Pipeline[B] = Pipeline(g => foreach(x => if (pf isDefinedAt x) g(pf(x))))
  def take(n: Int): Pipeline[A]                          = Pipeline(g => Pipeline.applyN(this, g, n))
  def drop(n: Int): Pipeline[A]                          = Pipeline(g => Pipeline.skipN(this, g, n))
  def mkString(sep: String): String                      = Pipeline.makeString(this, sep)
  override def toString = "<foreach>"
}

object Pipeline {
  def apply[A](xs: Traversable[A]): Pipeline[A]      = new Pipeline[A](xs foreach _)
  def apply[A](mf: (A => Unit) => Unit): Pipeline[A] = new Pipeline[A](mf)

  private def makeString[A](pipe: Pipeline[A], sep: String): String = {
    var first = true
    val sb = new StringBuilder
    pipe foreach { x =>
      if (first) try sb append x finally first = false
      else sb append sep append x
    }
    sb.toString
  }
  private def applyN[A](pipe: Pipeline[A], f: A => Unit, n: Int): Unit = {
    if (n > 0) {
      var count = n
      pipe foreach { x => f(x) ; count -= 1; if (count <= 0) return }
    }
  }
  private def skipN[A](pipe: Pipeline[A], f: A => Unit, n: Int): Unit = {
    var count = n
    pipe foreach (x => if (count <= 0) f(x) else count -= 1)
  }
}
