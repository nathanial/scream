package org.nrh.scream
import java.util.Formatter

object Debug {
  def debug(msg: String, obj:AnyRef*) {
    System.out.format(msg + "\n", obj:_*)
  }
}
