package psp
package tests

import org.scalacheck._, Prop._, Gen._
import psp.std._
import StdEq._

class ValuesSpec extends ScalacheckBundle {
  def bundle = "Values"
  object uintLaws extends Laws[UInt]
  import uintLaws._

  type UIntOp = (UInt, UInt) => UInt
  final case class NamedOp(name: String, op: UIntOp)

  def named(xs: NamedProp*): Seq[NamedProp]    = xs.toSeq
  def ops(xs: (String, UIntOp)*): Seq[NamedOp] = xs.toSeq map (k => NamedOp(k._1, k._2))

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
  final case class Quality(mustHave: String, mustNot: String, makeProp: NamedOp => Prop, desc: Boolean => String) {
    val isYes   = mustHave.wordSet
    val isNo    = mustNot.wordSet
    val hasProp = (s: String) => isYes(s) || isNo(s)

    def apply(p: NamedOp): Option[NamedProp] =
      if (!hasProp(p.name)) None else {
        val yes = isYes(p.name)
        val str = "UInt `%s` %s".format(p.name, desc(yes))
        Some(NamedProp(str, if (yes) makeProp(p) else !makeProp(p)))
      }
  }

  // generate from the full range of uints
  def fullRangeOps: Seq[NamedProp] = {
    implicit def arbUInt = Arbitrary(genUInt)
    def qualities = List(
      Quality("& | max min", "^ + * / -", x => forAll(idempotence(x.op)), x => if (x) "is idempotent" else "is NOT idempotent"),
      Quality("& | ^ max min + *", "/ -", x => forAll(commutative(x.op)), x => if (x) "commutes" else "does NOT commute"),
      Quality("& | ^ max min + *", "-", x => forAll(associative(x.op)), x => if (x) "associates" else "does NOT associate")
    )
    val cs = allOps flatMap (op => qualities flatMap (q => q(op)))
    cs ++ named("UInts are non-negative" -> forAll(genUInt)((x: UInt) => 0L <= x.longValue))
  }

  // generate from uints <= IntMax
  def smallishOps = {
    implicit val arbUInt = Arbitrary(genPosInt map UInt)
    named("on \"small\" uints, x <= x + y" -> forAll((x: UInt, y: UInt) => x <= (x + y) && y <= (x + y)))
  }

  def props = fullRangeOps ++ smallishOps
}
