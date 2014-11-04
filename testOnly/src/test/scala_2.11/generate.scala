package psp
package tests

import psp.std._

/** TODO - actually generate the code via an sbt generator.
 */
object Generator {
  val sciList = scala.List
  def tq    = "\"\"\""
  def lhses = sciList("\"abc\"", "scala.collection.Seq(1, 2, 3)", "scala.collection.mutable.Seq('a', 'b', 'c')", "Array(true, false, true)")
  def ops   = sciList("indexOf", "contains")
  def rhses = sciList("59d", "('b': Char)", "1")
  def envs  = sciList(
    "scalaLibraryCode" -> "",
    "policyByEquals"   -> ".byEquals",
    "policyByRef"      -> ".byRef",
    "policyStraight"   -> ""
  )
  def vals = for ((name, stub) <- envs) yield {
    val lines = for (l <- lhses; op <- ops; r <- rhses) yield s"$l$stub $op $r"
    s"  final val $name = " + tq + "\n    " + (lines mkString "\n    ") + "\n  " + tq
  }
  def template = s"""
    |package psp
    |package tests
    |package generated
    |
    |object Expressions {
    |  ${ vals mkString "\n" }
    |}""".stripMargin
}

object Expressions {
  final val pspShowCode = """
    /* no */ class Bippy ; val x = new Bippy ; show"$x"
    /* ok */ class Bippy ; val x = new Bippy ; implicit val s = Show[Bippy](_ => "yo") ; show"$x"
  """

  final val scalaLibraryCode = """
    "abc" indexOf 59d
    "abc" indexOf ('b': Char)
    "abc" indexOf 1
    "abc" contains 59d
    "abc" contains ('b': Char)
    "abc" contains 1
    scala.collection.Seq(1, 2, 3) indexOf 59d
    scala.collection.Seq(1, 2, 3) indexOf ('b': Char)
    scala.collection.Seq(1, 2, 3) indexOf 1
    scala.collection.Seq(1, 2, 3) contains 59d
    scala.collection.Seq(1, 2, 3) contains ('b': Char)
    scala.collection.Seq(1, 2, 3) contains 1
    scala.collection.mutable.Seq('a', 'b', 'c') indexOf 59d
    scala.collection.mutable.Seq('a', 'b', 'c') indexOf ('b': Char)
    scala.collection.mutable.Seq('a', 'b', 'c') indexOf 1
    scala.collection.mutable.Seq('a', 'b', 'c') contains 59d
    scala.collection.mutable.Seq('a', 'b', 'c') contains ('b': Char)
    scala.collection.mutable.Seq('a', 'b', 'c') contains 1
    Array(true, false, true) indexOf 59d
    Array(true, false, true) indexOf ('b': Char)
    Array(true, false, true) indexOf 1
    Array(true, false, true) contains 59d
    Array(true, false, true) contains ('b': Char)
    Array(true, false, true) contains 1
  """
  final val policyByEquals = """
    "abc".byEquals indexOf 59d
    "abc".byEquals indexOf ('b': Char)
    "abc".byEquals indexOf 1
    "abc".byEquals contains 59d
    "abc".byEquals contains ('b': Char)
    "abc".byEquals contains 1
    scala.collection.Seq(1, 2, 3).byEquals indexOf 59d
    scala.collection.Seq(1, 2, 3).byEquals indexOf ('b': Char)
    scala.collection.Seq(1, 2, 3).byEquals indexOf 1
    scala.collection.Seq(1, 2, 3).byEquals contains 59d
    scala.collection.Seq(1, 2, 3).byEquals contains ('b': Char)
    scala.collection.Seq(1, 2, 3).byEquals contains 1
    scala.collection.mutable.Seq('a', 'b', 'c').byEquals indexOf 59d
    scala.collection.mutable.Seq('a', 'b', 'c').byEquals indexOf ('b': Char)
    scala.collection.mutable.Seq('a', 'b', 'c').byEquals indexOf 1
    scala.collection.mutable.Seq('a', 'b', 'c').byEquals contains 59d
    scala.collection.mutable.Seq('a', 'b', 'c').byEquals contains ('b': Char)
    scala.collection.mutable.Seq('a', 'b', 'c').byEquals contains 1
    Array(true, false, true).byEquals indexOf 59d
    Array(true, false, true).byEquals indexOf ('b': Char)
    Array(true, false, true).byEquals indexOf 1
    Array(true, false, true).byEquals contains 59d
    Array(true, false, true).byEquals contains ('b': Char)
    Array(true, false, true).byEquals contains 1
  """
  final val policyByRef = """
    "abc".byRef indexOf 59d
    "abc".byRef indexOf ('b': Char)
    "abc".byRef indexOf 1
    "abc".byRef contains 59d
    "abc".byRef contains ('b': Char)
    "abc".byRef contains 1
    scala.collection.Seq(1, 2, 3).byRef indexOf 59d
    scala.collection.Seq(1, 2, 3).byRef indexOf ('b': Char)
    scala.collection.Seq(1, 2, 3).byRef indexOf 1
    scala.collection.Seq(1, 2, 3).byRef contains 59d
    scala.collection.Seq(1, 2, 3).byRef contains ('b': Char)
    scala.collection.Seq(1, 2, 3).byRef contains 1
    scala.collection.mutable.Seq('a', 'b', 'c').byRef indexOf 59d
    scala.collection.mutable.Seq('a', 'b', 'c').byRef indexOf ('b': Char)
    scala.collection.mutable.Seq('a', 'b', 'c').byRef indexOf 1
    scala.collection.mutable.Seq('a', 'b', 'c').byRef contains 59d
    scala.collection.mutable.Seq('a', 'b', 'c').byRef contains ('b': Char)
    scala.collection.mutable.Seq('a', 'b', 'c').byRef contains 1
    Array(true, false, true).byRef indexOf 59d
    Array(true, false, true).byRef indexOf ('b': Char)
    Array(true, false, true).byRef indexOf 1
    Array(true, false, true).byRef contains 59d
    Array(true, false, true).byRef contains ('b': Char)
    Array(true, false, true).byRef contains 1
  """
}
