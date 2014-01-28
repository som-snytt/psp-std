package psp
package core

import java.{ lang => jl }
import impl._

final class MemoIndexed[+A](xs: Foreach[A]) extends OpenIndexed[A] {
  @volatile private[this] var seen = 0
  @volatile private[this] var done = false

  private[this] val memo    = new ArrayBuffer[A]()
  private[this] val handoff = new LinkedBlockingQueue[A](1)
  private[this] lazy val thread  = spawn({ xs foreach handoff.put ; done = true })
  private[this] def spawn[A](body: => A): Unit = (new Thread() { override def run(): Unit = body }).start()
  private[this] def next(): A = handoff.poll match {
    case null => nullAs[A]
    case elem => memo += elem ; seen += 1 ; elem
  }
  private[this] def hasNext: Boolean = !done && (handoff.peek match {
    case null => Thread.`yield` ; hasNext
    case _    => true
  })

  def isDefinedAt(i: Int): Boolean = (i >= 0) && {
    thread
    while (i >= seen && hasNext) next()
    i < seen
  }

  def foreach(f: A => Unit): Unit = {
    def loop(i: Int): Unit = {
      if (isDefinedAt(i)) {
        f(elemAt(i))
        loop(i + 1)
      }
    }
    loop(0)
  }
  def apply(index: Int): A = elemAt(index)
  def elemAt(index: Int): A = if (isDefinedAt(index)) memo(index) else sys.error(s"Out of range: $index")
  def sizeInfo: SizeInfo = if (done) precise(seen) else precise(seen).atLeast
  override def toString = "<memo>"
}

final class ZippedIndexed[+A, +B](left: OpenIndexed[A], right: OpenIndexed[B]) extends OpenIndexed[(A, B)] {
  def foreach(f: ((A, B)) => Unit): Unit = {
    var i = 0
    while (isDefinedAt(i)) { f(apply(i)) ; i += 1 }
  }
  def isDefinedAt(index: Int): Boolean = (left isDefinedAt index) && (right isDefinedAt index)
  def apply(index: Int): (A, B)        = elemAt(index)
  def elemAt(index: Int): (A, B)       = (left elemAt index) -> (right elemAt index)
  def sizeInfo: SizeInfo               = left.sizeInfo min right.sizeInfo
}
