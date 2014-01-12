package psp
package demo
package test

import org.junit._
import Assert._
import scala.sys.process._

class PipelineTest {
  private var ops = 0
  def record(x: Int): Int = try x finally ops += 1

  def us(max: Int)        = new Pipeline[Int](1 to max foreach _) map record
  def rangeView(max: Int) = (1 to max).view map record
  def stream(max: Int)    = (Stream from 1 take max) map record
  def list(max: Int)      = (1 to max).toList map record

  def countOps[A](body: => A): Int = {
    ops = 0
    val result = body
    assertEquals("2/2/2", result)
    ops
  }
  def countColl[A](max: Int, create: Int => Traversable[Int]) = countOps(
    create(max) map (_ + 1) map (_ + 1) flatMap (_ => create(2)) take 6 filter (_ % 2 == 0) mkString "/"
  )
  def countColl2[A](max: Int, create: Int => Pipeline[Int]) = countOps(
    create(max) map (_ + 1) map (_ + 1) flatMap (_ => create(2)) take 6 filter (_ % 2 == 0) mkString "/"
  )

  @Test def f1 = assertEquals(9, countColl2(10, us))
  @Test def f2 = assertEquals(9, countColl(10, rangeView))
  @Test def f3 = assertEquals(9, countColl(10, stream))
  @Test def f4 = assertEquals(30, countColl(10, list))
}
