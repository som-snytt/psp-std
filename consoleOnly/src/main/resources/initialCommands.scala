import scala.collection.{ mutable => scm, immutable => sci }
import java.nio.{ file => jnf }
import psp.std._, api._, ansi._, pio._, scalac._
import StdEq._, StdShow._, StdZero._, StdMonoid._
import psp.std.repl.ReplImport._

def flongs = Indexed from 0L map (n => printResult(s"> $n")(n))
def dlongs = Indexed from 0L map (n => printResult(s"> $n")(n))
def int20  = 1 to 20 map (x => printResult(s"> $x")(x))
