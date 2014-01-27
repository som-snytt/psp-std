package psp
package core
package impl

import org.slf4j.LoggerFactory
import ch.qos.logback.core.util.StatusPrinter
import ch.qos.logback.classic.LoggerContext

trait PspLogging {
  private def logger  = LoggerFactory.getLogger(this.getClass)
  private def context = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

  // The following line prints startup information from logback if anything should go amiss
  StatusPrinter print context

  def logInfo(msg: String) = logger info msg
  def logWarn(msg: String) = logger warn msg

  def log(msg: String): Unit = Console.err println msg
  def eagerPrintResult[A](msg: String)(x: => A): A  = {
    Console.err.println(pp"$msg")
    val res = x
    Console.err.println(pp""" \\--> $res""")
    res
  }
  def printResult[A](msg: String)(x: A): A  = try x finally Console.err.println(pp"$msg: $x")
  def logResult[A](x: A): A  = try x finally log(pp"$x")
  def logError[A](msg: String)(x: A): A = {
    (new Throwable).getStackTrace take 10 foreach println
    try x finally log(pp"$msg: $x")
  }
}
