package psp
package core
package impl

final class Function1Ops[-T, +R](val f: T => R) extends AnyVal {
  def labeled(label: String): T => R = new LabeledFunction(f, label)
}

final class PartialFunctionOps[T, R](val pf: T =?> R) extends AnyVal {
  def labeled(label: String): T =?> R     = new LabeledPartialFunction(pf, label)
  def contraMap[T1](f: T1 => T): T1 =?> R = { case x if pf isDefinedAt f(x) => pf(f(x)) }
  def mapValues[R1](f: R => R1): T =?> R1 = { case x if pf isDefinedAt x => f(pf(x)) }
}

final class LabeledFunction[-T, +R](f: T => R, val label: String) extends (T => R) with Labeled {
  def apply(x: T): R = f(x)
}
final class LabeledPartialFunction[-T, +R](pf: PartialFunction[T, R], val label: String) extends PartialFunction[T, R] with Labeled {
  def isDefinedAt(x: T) = pf isDefinedAt x
  def apply(x: T): R = pf(x)
}


final class PpInterpolatorOps(val stringContext: StringContext) {
  final def pp(args: Any*): String = StringContext(stringContext.parts: _*).s(args map (_.to_s): _*)
}
