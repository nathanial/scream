package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.DomainImplicits._
import org.nrh.scream.Interval._
import org.nrh.scream.IntervalImplicits._
import org.scalatest._

object ProblemTest {
  def main(args: Array[String]){
    (new ProblemTest).execute()
  }
}

class ProblemTest extends Suite with Logging {
  
/*  def testNode1 { 
    val p = new Problem
    val a = p.newVar("a",domain(0 upto 3))
    val n = new Node(p.state)
    logger.info("TestNode1")
    n.successors.foreach(s => logger.info(s.toString))
  }

  def testNode2 {
    val p = new Problem
    val a = p.newVar("a", domain(0 upto 2))
    val b = p.newVar("b", domain(10 upto 13))
    val n = new Node(p.state)
    logger.info("TestNode2")
    logger.info("Root")
    logger.info(n.toString)
    logger.info("1st level")
    val s1 = n.successors 
    s1.foreach(s => logger.info(s.toString))
    val s2 = s1.flatMap(_.successors)
    logger.info("2nd level")
    s2.foreach(s => logger.info(s.toString))
    logger.info("s2.length = " + s2.length)
    assert(s2.length == 12)
  }

  def testNode3 {
    val p = new Problem
    val a = p.newVar("a",0)
    val n = new Node(p.state)
    logger.info("TestNode3")
    logger.info("Root")
    logger.info(n.toString)
    logger.info("1st level")
    val s1 = n.successors
    s1.foreach(s => logger.info(s.toString))
    assert(s1.length == 0)
  }
  */  
}
