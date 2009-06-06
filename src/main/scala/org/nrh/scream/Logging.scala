package org.nrh.scream
import org.slf4j.{Logger,LoggerFactory}

trait Logging {
  final protected val logger = LoggerFactory.getLogger(this.getClass)
  def trace[A](msg:String)(fn: => A):A = {
    logger.debug(msg)
    val result = fn
    logger.debug("Result = " + result)
    result
  }
}
