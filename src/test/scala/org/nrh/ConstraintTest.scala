package org.nrh

import org.nrh.scream._
import org.nrh.scream.Domain._
import org.junit._
import org.junit.Assert._

object ConstraintTest {
  def main(args: Array[String]){
    org.junit.runner.JUnitCore.runClasses(classOf[ConstraintTest])
  }

  def testAdd1(num:BigInt):scala.collection.Map[String,Range] = {
    val problem = new Problem
    val x = problem.newVar
    val y = problem.newVar
    problem.constrain(
      x == num,
      x + y == 20
    )
    return problem.label(Map("x" -> x, 
			     "y" -> y))
  }
  
  def testAdd2(num:BigInt):scala.collection.Map[String,Range] = {
    val problem = new Problem
    val a = problem.newVar
    val b = problem.newVar
    val c = problem.newVar
    problem.constrain(
      a == num,
      b == (num - 1),
      a + b == c
    )
    return problem.label(Map("a" -> a,
			     "b" -> b,
			     "c" -> c))
  }

  def testMinus1(num:BigInt):scala.collection.Map[String,Range] = {
    val problem = new Problem
    val a = problem.newVar
    val b = problem.newVar
    val c = problem.newVar
    problem.constrain(
      a == 17,
      b == num,
      a - b == c
    )
    return problem.label(Map("a" -> a,
			     "b" -> b,
			     "c" -> c))
  }

  def testMult1(num:BigInt):scala.collection.Map[String,Range] = {
    val problem = new Problem
    val a = problem.newVar
    val b = problem.newVar
    val c = problem.newVar
    problem.constrain(
      a == 20,
      b == num,
      a * b == c
    )
    return problem.label(Map("a" -> a,
			     "b" -> b,
			     "c" -> c))
  }

  def testDiv1(num1:BigInt, num2:BigInt):scala.collection.Map[String,Range] = {
    val problem = new Problem
    val a = problem.newVar
    val b = problem.newVar
    val c = problem.newVar
    problem.constrain(
      a == num1,
      b == num2,
      a / b == c
    )
    return problem.label(Map("a" -> a,
			     "b" -> b,
			     "c" -> c))
  }

  def testComplex1:scala.collection.Map[String,Range] = {
    val problem = new Problem
    val a = problem.newVar
    val b = problem.newVar
    problem.constrain(
      a + b == 20,
      a - b == 10
    )
    return problem.label(Map("a" -> a,
			     "b" -> b))
  }

}

class ConstraintTest {
  import org.nrh.ConstraintTest._

  @Test
  def test1 {
    val labels = testAdd1(5)
    assertTrue(labels("x") equals 5)
    assertTrue(labels("y") equals 15)
    println("test1 = " + labels)
  }

  @Test
  def test2 {
    val labels = testAdd2(5)
    assertTrue(labels("a") equals 5)
    assertTrue(labels("b") equals 4)
    assertTrue(labels("c") equals 9)
    println("test2 = " + labels)
  }

  @Test
  def test3 {
    val labels = testMinus1(10)
    assertTrue(labels("a") equals 17)
    assertTrue(labels("b") equals 10)
    assertTrue(labels("c") equals 7)
    println("test3 = " + labels)
  }

  @Test
  def test4 {
    val labels = testMult1(12)
    assertTrue(labels("a") equals 20)
    assertTrue(labels("b") equals 12)
    assertTrue(labels("c") equals 240)
    println("test4 = " + labels)
  }

  @Test
  def test5 { 
    val labels = testDiv1(32, 8)
    assertTrue(labels("a") equals 32)
    assertTrue(labels("b") equals 8)
    assertTrue(labels("c") equals 4)
    println("test5 = " + labels)
  }

  @Test 
  def test6 {
    println("")
    println("BEGIN TEST6")
    println("")
    val labels = testComplex1
    println("test6 = " + labels)
    println("")
    println("")
    val a = labels("a")
    val b = labels("b")
    assertTrue(a equals 15)
    assertTrue(b equals 5)
  }

}
