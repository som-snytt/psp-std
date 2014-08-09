package psp
package std

import java.util.regex.Pattern
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.blackbox.Context

/** TODO - mock the ridiculous scala ast with a clean one with the same structure.
 *  Same thing with positions. Then they will be easy to lift and unlift without
 *  struggling with Universes and Importers and Mirrors and all the crazy bullshit
 *  which made it so easy to stop working on scala in the first place.
 *
 *  So for now it's the high-tech approach of calling toString on the tree and
 *  type and passing those, but at least that lets us print something interesting.
 */
package macros {
  sealed trait TypecheckResult {
    def isError: Boolean
    def errorMessage: String
    def tree: String
    def tpe: String
  }
  final case class Typed(tree: String, tpe: String) extends TypecheckResult {
    def isError = false
    def errorMessage = ""
  }
  final case class TypeError(errorMessage: String) extends TypecheckResult {
    def isError = true
    def tree: String = "<error>"
    def tpe: String = "<error>"
  }
  final case class Typechecked(code: String, result: TypecheckResult) {
    def typechecks = !result.isError
    def message = result.errorMessage
    def tree = result.tree
    def tpe = result.tpe
    override def toString = if (result.isError) s"$code\n$message\n" else s"$tree: $tpe"
  }
  object Typechecked {
    def error(code: String, message: String): Typechecked           = new Typechecked(code, TypeError(message))
    def typed(code: String, tree: String, tpe: String): Typechecked = new Typechecked(code, Typed(tree, tpe))
  }

  class Typechecker(val c: Context) {
    import c.universe._

    private def fail(msg: String) = c.abort(c.enclosingPosition, msg)
    private def check(code: String): c.Expr[Typechecked] = {
      def treeStr(tree: Tree) = "%s: %s".format(tree, tree.tpe)
      def typed()             = scala.util.Try(c.typecheck(c.parse(code)))
      def good(tree: Tree)    = q"Typechecked.typed($code, ${tree.toString}, ${tree.tpe.toString})"
      def bad(t: Throwable)   = q"Typechecked.error($code, ${t.getMessage})"

      c.Expr(typed().fold(good, bad))
    }

    def typecheckedLines(exprs: c.Expr[String]): c.Expr[Vector[Typechecked]] = {
      val code: String = exprs.tree match {
        case Literal(Constant(s: String)) => s.trim
        case _                            => fail("not a literal string")
      }
      val lines = augmentString(code).lines.toVector map (_.trim) filterNot (_.length == 0) map check
      val res = lines.foldRight(q"Vector[Typechecked]()")((x, xs) => q"$x +: $xs")
      c.Expr(res)
    }

    def typechecked(expr: c.Expr[String]): c.Expr[Typechecked] = check(
      expr.tree match {
        case Literal(Constant(s: String)) => s.trim
        case _                            => fail("not a literal string")
      }
    )
  }
}

package object macros {
  def typechecked(expr: String): Typechecked               = macro Typechecker.typechecked
  def typecheckedLines(exprs: String): Vector[Typechecked] = macro Typechecker.typecheckedLines
}
