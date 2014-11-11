package psp
package std
package scalac

import api._, pio._, StdShow._, StdEq._, ansi._

final case class Source(content: Array[Char], analysis: TokenAnalysis) {
  val newlineIndices: Direct[Index] = content indicesOf '\n'

  lazy val lineStartIndices: Direct[Index]     = 0.index +: newlineIndices.map(_.next) filter content.containsIndex
  lazy val lineStartOffsets: Direct[Offset]    = lineStartIndices map (_.toOffset)
  lazy val lineVector: Direct[String]          = lineStartOffsets map lineFrom
  lazy val numberedLines: Direct[NumberedLine] = lineVector.nths map numberedLine
  lazy val anonymizedText: Doc                 = numberedLines.map(_.anonymous_s).joinLines <> "\n"
  lazy val numberedText: Doc                   = numberedLines.map(_.numbered_s).joinLines <> "\n"

  private def numberedLine(n: Nth)             = new NumberedLine(n, lineStartOffsets(n.toIndex), lineVector(n.toIndex), analysis.tokens filter (_.line == n))
  private def lineFrom(offset: Offset): String = content drop offset.toSize takeWhile (_ != '\n') force
  override def toString = s"Source(${content.length} bytes, ${analysis.tokens.length} tokens)"
}

final case class NumberedLine(line: Nth, offset: Offset, lineText: String, tokens: Direct[Token]) {
  import scala.tools.nsc.ast.parser.Tokens._
  private def shownType(token: Token): ColorString = token.intToken match {
    case IDENTIFIER | BACKQUOTED_IDENT                           => "id" %> blue
    case CHARLIT | INTLIT | LONGLIT                              => "int" %> green
    case DOUBLELIT | FLOATLIT                                    => "double" %> green
    case STRINGLIT                                               => "string" %> green
    case INTERPOLATIONID                                         => "interpolate" %> green.bold
    case STRINGPART                                              => "lit" %> green
    case LBRACE | RBRACE | LPAREN | RPAREN | LBRACKET | RBRACKET => token.shown %> cyan.bold
    case NEWLINE                                                 => emptyValue
    case NEWLINES                                                => emptyValue
    case _                                                       => ColorString(token.shown)
  }

  private def noSpace(x: ColorString, y: ColorString): Boolean = y.raw match {
    case "." | "," | ":" | "]" | ")" => true
    case _                           => sciList("[", "(", ".") exists x.raw.endsWith
  }
  def tokenString: ColorString = tokens map shownType zreduce ((x, y) => if (noSpace(x, y)) x <> y else x <+> y)
  def leadingSpace             = lineText takeWhile (_.isWhitespace) build
  def anonymous_s              = "%4s  %6s  %s%s".format(line.nthValue, offset.offsetValue, leadingSpace, tokenString.colorized)
  def numbered_s               = "%4s  %6s  %s".format(line.nthValue, offset.offsetValue, lineText)
}
