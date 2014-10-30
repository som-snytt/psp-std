package psp
package std

import api._, StdShow._

final case class FunctionGrid[A, B](values: View[A], functions: View[A => B]) {
  def rows: View[View[B]]                         = values map (v => functions map (f => f(v)))
  def columns: View[View[B]]                      = functions map (f => values map (v => f(v)))
  def renderLines(implicit z: Show[B]): Direct[String]               = {
    val widths = columns map (col => col map (x => (z show x).length) max)
    val rowFormat = widths map (_.size.leftFormatString) mkString " "
    rows map (row => rowFormat.format(row.seq: _*))
  }
  def render(implicit z: Show[B]): String = renderLines.joinLines.render
}

object Grid {
  def apply[A](f: A => (A, A)): A => View[A] = {
    def loop(x: A): View[A] = f(x) |> { case (a, b) => a +: loop(b) }
    loop
  }
}
