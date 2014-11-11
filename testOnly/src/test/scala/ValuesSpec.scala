package psp
package tests

import org.scalacheck._, Prop._, Gen._
import psp.std._, api._, StdEq._, StdShow._

class ValuesSpec extends ScalacheckBundle {
  def bundle = "UInt laws"
  object uintLaws extends Laws[UInt]
  import uintLaws._

  type UIntOp = (UInt, UInt) => UInt
  final case class NamedOp(name: String, op: UIntOp)

  def named(xs: NamedProp*): View[NamedProp]    = xs.m
  def ops(xs: (String, UIntOp)*): View[NamedOp] = xs.m map (k => NamedOp(k._1, k._2))

  lazy val allOps = ops(
    "&"   -> (_ & _),
    "|"   -> (_ | _),
    "^"   -> (_ ^ _),
    "max" -> (_ max _),
    "min" -> (_ min _),
    "+"   -> (_ + _),
    "*"   -> (_ * _),
    "/"   -> ((x, y) => UInt(x.longValue / y.longValue)),
    "-"   -> ((x, y) => UInt(x.longValue - y.longValue))
  )
  // The inverted properties check that the condition fails at least once for that operation.
  // Mostly it's a sanity check that something is being accomplished here.
  final case class Quality(mustHave: String, mustNot: String, makeProp: NamedOp => Prop, yes: String, no: String) {
    val isYes   = mustHave.wordSet
    val isNo    = mustNot.wordSet
    val hasProp = (s: String) => isYes(s) || isNo(s)

    def describe(s: String): String     = if (isYes(s)) yes.trim else if (isNo(s)) no.trim else "N/A"
    def apply(p: NamedOp): Option[Prop] = if (!hasProp(p.name)) None else Some( if (isYes(p.name)) makeProp(p) else !makeProp(p) )
  }

  // generate from the full range of uints
  def fullRangeOps: Each[NamedProp] = {
    implicit def arbUInt = Arb(gen.uint)
    def qualities: Each[Quality] = exSeq(
      Quality("& |   max min    ", "^ + * / -", x => forAll(idempotence(x.op)), "idempotent", "NOT idempotent"),
      Quality("& | ^ max min + *", "      / -", x => forAll(commutative(x.op)), "commutes  ", "does NOT commute"),
      Quality("& | ^ max min + *", "        -", x => forAll(associative(x.op)), "associates", "does NOT associate")
    )
    def one(op: NamedOp): NamedProp = {
      def name = "%-5s" format ("`" + op.name + "`")
      val str  = qualities map (_ describe op.name) map ("%18s" format _) mkString ", "
      val desc = fshow"Relation laws for UInt $name%s  $str%s"
      val prop = qualities flatMap (q => q(op).toDirect) reducel (_ && _)
      desc -> prop
    }
    (allOps map one) ++ named("UInts are non-negative" -> forAll(gen.uint)((x: UInt) => 0L <= x.longValue))
  }

  // generate from uints <= IntMax
  def smallishOps: Each[NamedProp] = {
    implicit val arbUInt = Arb(gen.posInt map UInt)
    named("on \"small\" uints, x <= x + y" -> forAll((x: UInt, y: UInt) => x <= (x + y) && y <= (x + y)))
  }

  def props = (fullRangeOps ++ smallishOps).toScalaVector
}
