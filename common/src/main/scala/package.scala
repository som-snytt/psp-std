package psp

import org.slf4j.LoggerFactory
import ch.qos.logback.core.util.StatusPrinter
import ch.qos.logback.classic.LoggerContext

trait PspCommon {
  private def logger  = LoggerFactory.getLogger(this.getClass)
  private def context = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

  // The following line prints startup information from logback if anything should go amiss
  StatusPrinter print context

  def logInfo(msg: String) = logger info msg
  def logWarn(msg: String) = logger warn msg
}

package object common extends PspCommon
