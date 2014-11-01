package psp
package std

import api._

/** Indexed is somewhere between Each and Direct.
 *  There's an apply(index) method, but its size may not be known and may be infinite.
 *  We can memoize an Each into an Indexed.
 */
object Indexed {
  final class Memo[+A](xs: Each[A]) extends Indexed[A] {
    @volatile private[this] var done = false
    @volatile private[this] var memo = sciVector[A]()
    private[this] val handoff = new LinkedBlockingQueue[A](1)

    private[this] lazy val thread: Unit = spawn({ xs foreach handoff.put ; done = true })
    private[this] def seen: Precise = memo.length
    private[this] def next(): A = handoff.poll match {
      case null => nullAs[A]
      case elem => elem doto (memo :+= _)
    }
    private[this] def hasNext: Boolean = !done && (handoff.peek match {
      case null => threadYield() ; hasNext
      case _    => true
    })

    def isDefinedAt(i: Index): Boolean = !i.isUndefined && {
      thread
      while (!seen.containsIndex(i) && hasNext) next()
      seen containsIndex i
    }

    def foreach(f: A => Unit): Unit = {
      def loop(i: Index): Unit = {
        if (isDefinedAt(i)) {
          f(elemAt(i))
          loop(i.next)
        }
      }
      loop(Index(0))
    }
    def apply(index: Index): A  = elemAt(index)
    def elemAt(index: Index): A = if (isDefinedAt(index)) memo(index.safeToInt) else abort(s"Out of range: $index")
    def size: Size              = if (done) seen else seen.atLeast
  }
}
