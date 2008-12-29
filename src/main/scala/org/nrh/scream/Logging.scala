package org.nrh.scream
import org.slf4j.{Logger,LoggerFactory}

trait Logging {
  final protected val logger = LoggerFactory.getLogger(this.getClass)
}
