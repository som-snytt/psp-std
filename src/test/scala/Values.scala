package psp
package tests

import org.scalacheck._, Prop._, Gen._
import psp.std._

class ValuesSpec extends ScalacheckBundle {
  def bundle = "Values"
  object uintLaws extends Laws[UInt]
  import uintLaws._

  type UIntOp = (UInt, UInt) => UInt

  def commutativeOps = List[(String, UIntOp)](
    "+"   -> (_ + _),
    "*"   -> (_ * _),
    "&"   -> (_ & _),
    "|"   -> (_ | _),
    "^"   -> (_ ^ _),
    "max" -> (_ max _),
    "min" -> (_ min _)
  )
  // Mostly, a sanity check that something is being accomplished here
  def nonCommutativeOps = List[(String, UIntOp)](
    "-" -> ((x, y) => UInt(x.longValue - y.longValue)),
    "/" -> ((x, y) => UInt(x.longValue / y.longValue))   //  ... but it does associate
  )

  // generate from the full range of uints
  def fullRangeOps: Seq[NamedProp] = {
    implicit def arbUInt = Arbitrary(genUInt)
    def cs0 = commutativeOps flatMap { case (name, op) =>
      Seq[NamedProp](
        s"`$name` commutes"   -> forAll(commutative(op)),
        s"`$name` associates" -> forAll(associative(op))
      )
    }
    def cs1 = nonCommutativeOps flatMap { case (name, op) =>
      Seq[NamedProp](
        s"`$name` does not commute"   -> !forAll(commutative(op))
      )
    }
    cs0 ++ cs1 ++ Seq(NamedProp("UInts are positive", forAll(genUInt)((x: UInt) => 0L <= x.longValue)))
  }
  // generate from uints <= IntMax
  def smallishOps = {
    implicit val arbUInt = Arbitrary(genPosInt map UInt)
    Seq[NamedProp](
      "`+` on small uints" -> forAll((x: UInt, y: UInt) => x <= (x + y) && y <= (x + y))
    )
  }

  def props = fullRangeOps ++ smallishOps
}
