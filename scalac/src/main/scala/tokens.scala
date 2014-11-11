package psp
package std
package scalac

import api._, StdEq._

class TokenAnalysis(val global: Global, val content: Chars) {
  import global._

  val newlines: Each[Index] = content indicesOf '\n'
  val tokens: Direct[Token] = tokenize()

  private class TokenScanner extends syntaxAnalyzer.Scanner {
    val buf = content
    override val decodeUni: Boolean = !settings.nouescape.value
    init()
    // scalac helpfully throws errors when trying to compile itself.
    // "macro is now a reserved word; usage as an identifier is disallowed"
    def deprecationWarning(off: Int, msg: String): Unit   = echoErr(s"deprecationWarning($off, $msg)")
    def error  (off: Int, msg: String): Unit              = echoErr(s"error($off, $msg)")
    def incompleteInputError(off: Int, msg: String): Unit = echoErr(s"incompleteInputError($off, $msg)")
    def warning(off: Int, msg: String): Unit              = echoErr(s"warning($off, $msg)") // 2.10 only
  }

  private def tokenize(): Direct[Token] = {
    import scala.tools.nsc.ast.parser.Tokens._
    val in = new TokenScanner
    var id = 0
    def currentId()     = try Id(id) finally id += 1
    def currentChar()   = Index(in.offset)
    def currentLine()   = Index(newlines count (_ < currentChar)).toNth
    def currentScalac() = ScalacToken(in.token)
    def currentPos()    = LineAndIndex(currentLine, currentChar)
    def currentToken()  = Token(currentId(), currentScalac(), currentValue(), currentPos())
    def currentValue(): Any = in.token match {
      case IDENTIFIER | BACKQUOTED_IDENT => in.name
      case CHARLIT | INTLIT | LONGLIT    => in.intVal
      case DOUBLELIT | FLOATLIT          => in.floatVal
      case INTERPOLATIONID               => in.strVal
      case STRINGLIT                     => in.strVal
      case SYMBOLLIT                     => scala.Symbol(in.strVal)
      case TRUE                          => true
      case FALSE                         => false
      case NULL                          => null
      case NEWLINE                       => "<nl>"
      case NEWLINES                      => "<nls>"
      case SEMI                          => ";"
      case x                             => syntaxAnalyzer token2string x stripPrefix "'" stripSuffix "'"
    }
    def next(): Token = andResult(currentToken(), in.nextToken())
    Each continually next() takeToFirst (_.isEOF)
  }
}
