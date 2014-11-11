package psp
package std

import api._, pio._

package scalac {
  object PathTokens extends PathCache(p => new TokenAnalysis(tokenGlobal, p.chars))
  object PathSource extends PathCache(p => new Source(p.chars, PathTokens(p)))
}

package object scalac {
  type Global   = scala.tools.nsc.Global
  type Settings = scala.tools.nsc.Settings

  val NoLineAndIndex: LineAndIndex = LineAndIndex(Nth.undefined, Index.undefined)
  val NoToken: Token               = Token(Id.None, ScalacToken(-1), "<none>", NoLineAndIndex)

  lazy val tokenGlobal = newGlobal("-Yrangepos")

  def tokenAnalysis(content: String): TokenAnalysis = tokenAnalysis(content.toCharArray)
  def tokenAnalysis(content: Chars): TokenAnalysis  = new TokenAnalysis(tokenGlobal, content)
  def source(code: String): Source                  = new Source(code.toCharArray, tokenAnalysis(code.toCharArray))
  def source(path: Path): Source                    = PathSource(path)

  implicit def emptyToken: Empty[Token] = Empty(NoToken)

  def newSettings(settings: String): Settings = new Settings doto (_ processArgumentString settings)
  def newGlobal(settings: String): Global     = new Global(newSettings(settings))
}
