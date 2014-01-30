package psp
package core
package impl

import org.slf4j.LoggerFactory
import ch.qos.logback.core.util.StatusPrinter
import ch.qos.logback.classic.LoggerContext

trait PspUtility extends PspLogging {
  def join(sep: String)(xs: Any*): String = xs mkString sep
  def andTrue(x: Unit): Boolean           = true
  def andFalse(x: Unit): Boolean          = false
  def nullAs[A] : A                       = (null: AnyRef).castTo[A]
  def decodeName(s: String): String       = scala.reflect.NameTransformer.decode(s)

  def timed[A](body: => A): A = {
    val start = System.nanoTime
    try body finally log("Elapsed: %.3f ms" format (System.nanoTime - start) / 1e6)
  }
}

trait PspLogging {
  private def logger  = LoggerFactory.getLogger(this.getClass)
  private def context = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

  // The following line prints startup information from logback if anything should go amiss
  StatusPrinter print context

  def logInfo(msg: String) = logger info msg
  def logWarn(msg: String) = logger warn msg

  def log(msg: String): Unit = Console.err println msg
  def logResult[A](msg: String)(x: A): A  = try x finally log(pp"$msg: $x")
  def logResult_![A](msg: String)(body: => A): A  = {
    log(msg)
    logResult(" \\-->")(body)
  }
}
