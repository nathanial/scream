package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.Range._
import org.scalatest._

object ProblemTest {
  def main(args: Array[String]){
    (new ProblemTest).execute()
  }
}

class ProblemTest extends Suite with Logging {
  
  def testNode1 { 
    val d1 = domain(0 upto 3)
    val n = new Node(new VarState(0,"d1", d1) :: Nil)
    logger.info("TestNode1")
    n.successors.foreach(s => logger.info(s.toString))
  }

  def testNode2 {
    val d1 = domain(0 upto 2)
    val d2 = domain(10 upto 13)
    val n = new Node(new VarState(0,"d1", d1) :: new VarState(1,"d2", d2) :: Nil)
    logger.info("TestNode2")
    logger.info("Root")
    logger.info(n.toString)
    logger.info("1st level")
    val s1 = n.successors 
    s1.foreach(s => logger.info(s.toString))
    val s2 = s1.flatMap(_.successors)
    logger.info("2nd level")
    s2.foreach(s => logger.info(s.toString))
    assert(s1.length == 3)
    assert(s2.length == 12)
  }

  def testNode3 {
    val d1:Domain = 0
    val n = new Node(new VarState(0,"d1", d1) :: Nil)
    logger.info("TestNode3")
    logger.info("Root")
    logger.info(n.toString)
    logger.info("1st level")
    val s1 = n.successors
    s1.foreach(s => logger.info(s.toString))
    assert(s1.length == 0)
  }
    
}
