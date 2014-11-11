package psp
package std
package scalac

import api._
import scala.tools.nsc.ast.parser.Tokens._

final case class ScalacToken(value: Int) extends AnyVal
final case class LineAndIndex(line: Nth, index: Index)
final case class Token(id: Id, scalacToken: ScalacToken, basicValue: Any, pos: LineAndIndex) {
  def isEOF: Boolean = intToken == EOF
  def intToken: Int  = scalacToken.value
  def shown: String  = s"$basicValue"
  def offset         = pos.index.toOffset
  def line           = pos.line
}

final class Id private (val value: Int) extends AnyVal {
  def to_s = toString
  def index: Index = Index(value)
  def compare(that: Id) = (value compare that.value).intValue
  override def toString = s"$value"
}
object Id {
  val None = new Id(-1)
  def apply(id: Int): Id = if (id < 0) None else new Id(id)
}

object ScalacToken {
  def show(value: Int): String = value match {
    case ABSTRACT         => "abstract"
    case ARROW            => "=>"
    case AT               => "@"
    case BACKQUOTED_IDENT => "`<id>`"
    case CASE             => "case"
    case CASECLASS        => "case class"
    case CASEOBJECT       => "case object"
    case CATCH            => "catch"
    case CHARLIT          => "charlit"
    case CLASS            => "class"
    case COLON            => ":"
    case COMMA            => ","
    case COMMENT          => "/* */"
    case DEF              => "def"
    case DO               => "do"
    case DOT              => "."
    case DOUBLELIT        => "doublelit"
    case ELSE             => "else"
    case EMPTY            => "<empty>"
    case EOF              => "<eof>"
    case EQUALS           => "="
    case ERROR            => "error"
    case ESCAPE           => "<esc>"
    case EXTENDS          => "extends"
    case FALSE            => "false"
    case FINAL            => "final"
    case FINALLY          => "finally"
    case FLOATLIT         => "floatlit"
    case FOR              => "for"
    case FORSOME          => "forSome"
    case HASH             => "#"
    case IDENTIFIER       => "<id>"
    case IF               => "if"
    case IGNORE           => "<ignore>"
    case IMPLICIT         => "implicit"
    case IMPORT           => "import"
    case INTERPOLATIONID  => "interpolationid"
    case INTLIT           => "intlit"
    case LARROW           => "<-"
    case LAZY             => "lazy"
    case LBRACE           => "{"
    case LBRACKET         => "["
    case LONGLIT          => "longlit"
    case LPAREN           => "("
    case MACRO            => "macro"
    case MATCH            => "match"
    case NEW              => "new"
    case NEWLINE          => "<nl>"
    case NEWLINES         => "<nls>"
    case NULL             => "null"
    case OBJECT           => "object"
    case OVERRIDE         => "override"
    case PACKAGE          => "package"
    case PRIVATE          => "private"
    case PROTECTED        => "protected"
    case RBRACE           => "}"
    case RBRACKET         => "]"
    case RETURN           => "return"
    case RPAREN           => ")"
    case SEALED           => "sealed"
    case SEMI             => ";"
    case STRINGLIT        => "stringlit"
    case STRINGPART       => "stringpart"
    case SUBTYPE          => "<:"
    case SUPER            => "super"
    case SUPERTYPE        => ">:"
    case SYMBOLLIT        => "symbollit"
    case THEN             => "then"
    case THIS             => "this"
    case THROW            => "throw"
    case TRAIT            => "trait"
    case TRUE             => "true"
    case TRY              => "try"
    case TYPE             => "type"
    case UNDEF            => "<undef>"
    case USCORE           => "_"
    case VAL              => "val"
    case VAR              => "var"
    case VIEWBOUND        => "<%"
    case WHILE            => "while"
    case WHITESPACE       => "<sp>"
    case WITH             => "with"
    case XMLSTART         => "<xmlstart>"
    case YIELD            => "yield"
    case value            => s"$value"
  }
}
