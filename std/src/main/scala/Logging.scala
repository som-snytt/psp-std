package psp
package std

import api._
import java.text.DateFormat._

class DelayedLog {
  val counter = Counter(1)
  private[this] var toLog: List[Message[_]] = Nil

  def timeString(timeStyle: Int = MEDIUM): String = getTimeInstance(timeStyle) format new jDate()
  def stackString(frames: Int   = 50): String = (new Throwable).getStackTrace drop 3 take frames mkString EOL

  private class Message[T](bodyFn: => T, pf: T ?=> String) {
    val id    = counter.next()
    val stamp = timeString()
    lazy val value = bodyFn
    lazy val message = value.matchOr("")(pf)

    override def toString = message
  }

  def purge() = try dump() finally toLog = Nil

  def batch[T](body: => T): T = {
    val saved = toLog
    toLog = Nil
    try body finally {
      try dump() finally toLog = saved
    }
  }
  private def dump() = {
    println("dump() disabled")
    // val entries = toLog.reverse filterNot (_.message.isEmpty)
    // val freqs   = entries.map(_.value).ascendingFrequency
    // implicit val eqs: Eq[Any] = Eq.natural[Any]()
    // for (entry <- entries.m distinctBy (_.value)) {
    //   import entry._
    //   val count_s = freqs(value) match {
    //     case 1 => ""
    //     case n => s" ($n times)"
    //   }
    //   println(f"[$stamp] $id%6s $message$count_s")
    // }
  }

  def record[A: Show](body: => A): Unit               = recordIf(body) { case x => x.to_s }
  def recordIf[A](body: => A)(pf: A ?=> String): Unit = toLog ::= new Message[A](body, pf)
}
