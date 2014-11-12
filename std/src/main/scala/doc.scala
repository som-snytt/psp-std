package psp
package std

import api._

sealed trait Doc

object Doc extends DocConstants {
  final case object WordBreak                                 extends Doc
  final case object LineBreak                                 extends Doc
  final case class Group(doc: Doc)                            extends Doc
  final case class Line(alt: String)                          extends Doc // alt is what to use instead of a linebreak if possible, e.g. "; " or " "
  final case class Nest(doc: Doc, indent: Indent)             extends Doc
  final case class Text(value: String)                        extends Doc
  final case class Cat(left: Doc, right: Doc)                 extends Doc
  final case class Format(format: FormatString, args: DocSeq) extends Doc
  final case class Shown[A](value: A, shows: Show[A])         extends Doc

  final case class Width(value: Int) extends AnyVal
  final case class Indent(level: Int) extends AnyVal
  final case class Layout(body: String) extends AnyVal
  final case class FormatString(chars: String) extends AnyVal {
    def %(args: Doc*): Doc = Format(this, args.seq)
  }

  def apply(s: String): Doc     = Text(s)
  def apply[A: Show](x: A): Doc = Shown(x, implicitly[Show[A]])

  implicit def showDoc: Show[Doc] = Show(x => "" + x)
}

trait DocConstants {
  private implicit def liftString(s: String): Doc = Doc.Text(s)

  val `extends`: Doc  = "extends"
  val `new`: Doc      = "new"
  val `super`: Doc    = "super"
  val `this`: Doc     = "this"
  val `with`: Doc     = "with"

  val ampersand: Doc   = "&"
  val asterisk: Doc    = "*"
  val atsign: Doc      = "@"
  val backquote: Doc   = "`"
  val backslash: Doc   = "\\"
  val caret: Doc       = "^"
  val colon: Doc       = ":"
  val comma: Doc       = ","
  val dollar: Doc      = "$"
  val dot: Doc         = "."
  val dquote: Doc      = "\""
  val assign: Doc      = "="
  val empty: Doc       = ""
  val exclamation: Doc = "!"
  val forwslash: Doc   = "/"
  val hash: Doc        = "#"
  val langle: Doc      = "<"
  val lbrace: Doc      = "{"
  val lbracket: Doc    = "["
  val lparen: Doc      = "("
  val minus: Doc       = "-"
  val percent: Doc     = "%"
  val plus: Doc        = "+"
  val question: Doc    = "?"
  val rangle: Doc      = ">"
  val rbrace: Doc      = "}"
  val rbracket: Doc    = "]"
  val rparen: Doc      = ")"
  val semi: Doc        = ";"
  val space: Doc       = " "
  val squote: Doc      = "'"
  val tilde: Doc       = "~"
  val underscore: Doc  = "_"
  val verticalbar: Doc = "|"

  val word: Doc       = Doc.WordBreak
  val line: Doc       = Doc.LineBreak
  val commaSpace: Doc = comma <> space
  val linebreak: Doc  = Doc.Line("")
  val softbreak: Doc  = Doc.Group(linebreak)
  val softline: Doc   = Doc.Group(line)
}
