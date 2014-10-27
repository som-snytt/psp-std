import scala.collection.{ mutable => scm, immutable => sci }
import java.nio.{ file => jnf }
import psp.std._, api._, ansi._, pio._
import StdEq._, StdShow._, StdZero._, StdMonoid._
import psp.std.repl.ReplImport._

def flongs = Foreach from 0L map (n => printResult(s"> $n")(n)) counting
def dlongs = Direct from 0L map (n => printResult(s"> $n")(n)) counting
