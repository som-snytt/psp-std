package psp
package dmz

import scala._
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import java.lang.{ String, Math }
import sc.convert._

trait PolicyDmz extends AnyRef
      with ScalaLibrary
      with JavaLibrary
      with DecorateAsScala
      with DecorateAsJava

/** Objects mimicking the most commonly used names from scala's
 *  default namespace, since we aren't auto-importing all that
 *  stuff any longer.
 */
object System {
  def out = java.lang.System.out
  def err = java.lang.System.err
  def in  = java.lang.System.in

  def setOut(out: java.io.PrintStream): Unit = java.lang.System.setOut(out)
  def setErr(err: java.io.PrintStream): Unit = java.lang.System.setErr(err)
  def setIn(in: java.io.InputStream): Unit   = java.lang.System.setIn(in)
}
object Console {
  def out = scala.Console.out
  def err = scala.Console.err
  def in  = scala.Console.in

  def echoErr(msg: Any): Unit = err println msg
  def echoOut(msg: Any): Unit = out println msg
}
object sys {
  def error(msg: String): Nothing = scala.sys.error(msg)
  def props                       = scala.sys.props
  def env                         = scala.sys.env
}
object :: {
  def apply[A](hd: A, tl: sci.List[A]): sci.::[A] = sci.::(hd, tl)
  def unapply[A](xs: sci.List[A]) = if (xs.isEmpty) None else Some((xs.head, xs.tail))
}
object Try {
  def apply[A](body: => A): scala.util.Try[A] = scala.util.Try[A](body)
}
object Success {
  def unapply[A](x: scala.util.Try[A]): Option[A] = x match {
    case scala.util.Success(x) => Some(x)
    case _                     => None
  }
}
object Failure {
  def unapply[A](x: scala.util.Try[A]): Option[Throwable] = x match {
    case scala.util.Failure(x) => Some(x)
    case _                     => None
  }
}
object Option {
  def empty[A] : Option[A]     = scala.None
  def apply[A](x: A)           = if (x == null) scala.None else scala.Some[A](x)
  def unapply[A](x: Option[A]) = x
}
object Some {
  def apply[A](x: A)               = scala.Some[A](x)
  def unapply[A](x: scala.Some[A]) = x
}
object Seq {
  def empty[A] : sci.Seq[A]        = sci.Seq()
  def apply[A](xs: A*): sci.Seq[A] = sci.Seq[A](xs: _*)
  def unapplySeq[A](xs: sc.Seq[A]) = Some(xs)
}
object Set {
  def empty[A] : sci.Set[A]        = sci.Set[A]()
  def apply[A](xs: A*): sci.Set[A] = sci.Set[A](xs: _*)
}

object math {
  def max(x: Int, y: Int): Int          = Math.max(x, y)
  def max(x: Long, y: Long): Long       = Math.max(x, y)
  def max(x: Float, y: Float): Float    = Math.max(x, y)
  def max(x: Double, y: Double): Double = Math.max(x, y)

  def min(x: Int, y: Int): Int          = Math.min(x, y)
  def min(x: Long, y: Long): Long       = Math.min(x, y)
  def min(x: Float, y: Float): Float    = Math.min(x, y)
  def min(x: Double, y: Double): Double = Math.min(x, y)

  def abs(x: Int): Int       = Math.abs(x)
  def abs(x: Long): Long     = Math.abs(x)
  def abs(x: Float): Float   = Math.abs(x)
  def abs(x: Double): Double = Math.abs(x)

  def round(x: Float): Int   = Math.round(x)
  def round(x: Double): Long = Math.round(x)
  def round(x: Int): Int     = x
  def round(x: Long): Long   = x

  def signum(x: Float): Float   = Math.signum(x)
  def signum(x: Double): Double = Math.signum(x)
  def signum(x: Int): Int       = if (x < 0) -1 else if (x > 0) 1 else 0
  def signum(x: Long): Long     = if (x < 0L) -1L else if (x > 0L) 1L else 0L

  def IEEEremainder(f1: Double, f2: Double): Double       = Math.IEEEremainder(f1, f2)
  def acos(x: Double): Double                             = Math.acos(x)
  def asin(x: Double): Double                             = Math.asin(x)
  def atan(x: Double): Double                             = Math.atan(x)
  def atan2(y: Double, x: Double): Double                 = Math.atan2(y, x)
  def cbrt(x: Double): Double                             = Math.cbrt(x)
  def ceil(x: Double): Double                             = Math.ceil(x)
  def copySign(magnitude: Double, sign: Double): Double   = Math.copySign(magnitude, sign)
  def cos(x: Double): Double                              = Math.cos(x)
  def cosh(x: Double): Double                             = Math.cosh(x)
  def exp(x: Double): Double                              = Math.exp(x)
  def expm1(x: Double): Double                            = Math.expm1(x)
  def floor(x: Double): Double                            = Math.floor(x)
  def hypot(x: Double, y: Double): Double                 = Math.hypot(x, y)
  def log(x: Double): Double                              = Math.log(x)
  def log10(x: Double): Double                            = Math.log10(x)
  def log1p(x: Double): Double                            = Math.log1p(x)
  def nextAfter(start: Double, direction: Double): Double = Math.nextAfter(start, direction)
  def nextUp(x: Double): Double                           = Math.nextUp(x)
  def pow(x: Double, y: Double): Double                   = Math.pow(x, y)
  def random(): Double                                    = Math.random()
  def rint(x: Double): Double                             = Math.rint(x)
  def sin(x: Double): Double                              = Math.sin(x)
  def sinh(x: Double): Double                             = Math.sinh(x)
  def sqrt(x: Double): Double                             = Math.sqrt(x)
  def tan(x: Double): Double                              = Math.tan(x)
  def tanh(x: Double): Double                             = Math.tanh(x)
  def toDegrees(x: Double): Double                        = Math.toDegrees(x)
  def toRadians(x: Double): Double                        = Math.toRadians(x)
  def ulp(x: Double): Double                              = Math.ulp(x)
}

object Arrays {
  // Static methods in java.util.Arrays
  //
  // boolean deepEquals(java.lang.Object[],java.lang.Object[])
  // boolean equals(boolean[],boolean[])
  // boolean equals(byte[],byte[])
  // boolean equals(char[],char[])
  // boolean equals(double[],double[])
  // boolean equals(float[],float[])
  // boolean equals(int[],int[])
  // boolean equals(java.lang.Object[],java.lang.Object[])
  // boolean equals(long[],long[])
  // boolean equals(short[],short[])
  // boolean[] copyOf(boolean[],int)
  // boolean[] copyOfRange(boolean[],int,int)
  // byte[] copyOf(byte[],int)
  // byte[] copyOfRange(byte[],int,int)
  // char[] copyOf(char[],int)
  // char[] copyOfRange(char[],int,int)
  // double[] copyOf(double[],int)
  // double[] copyOfRange(double[],int,int)
  // float[] copyOf(float[],int)
  // float[] copyOfRange(float[],int,int)
  // int binarySearch(byte[],byte)
  // int binarySearch(byte[],int,int,byte)
  // int binarySearch(char[],char)
  // int binarySearch(char[],int,int,char)
  // int binarySearch(double[],double)
  // int binarySearch(double[],int,int,double)
  // int binarySearch(float[],float)
  // int binarySearch(float[],int,int,float)
  // int binarySearch(int[],int)
  // int binarySearch(int[],int,int,int)
  // int binarySearch(java.lang.Object[],int,int,java.lang.Object)
  // int binarySearch(java.lang.Object[],int,int,java.lang.Object,java.util.Comparator)
  // int binarySearch(java.lang.Object[],java.lang.Object)
  // int binarySearch(java.lang.Object[],java.lang.Object,java.util.Comparator)
  // int binarySearch(long[],int,int,long)
  // int binarySearch(long[],long)
  // int binarySearch(short[],int,int,short)
  // int binarySearch(short[],short)
  // int deepHashCode(java.lang.Object[])
  // int hashCode(boolean[])
  // int hashCode(byte[])
  // int hashCode(char[])
  // int hashCode(double[])
  // int hashCode(float[])
  // int hashCode(int[])
  // int hashCode(java.lang.Object[])
  // int hashCode(long[])
  // int hashCode(short[])
  // int[] copyOf(int[],int)
  // int[] copyOfRange(int[],int,int)
  // java.lang.Object[] copyOf(java.lang.Object[],int)
  // java.lang.Object[] copyOf(java.lang.Object[],int,java.lang.Class)
  // java.lang.Object[] copyOfRange(java.lang.Object[],int,int)
  // java.lang.Object[] copyOfRange(java.lang.Object[],int,int,java.lang.Class)
  // java.lang.String deepToString(java.lang.Object[])
  // java.lang.String toString(boolean[])
  // java.lang.String toString(byte[])
  // java.lang.String toString(char[])
  // java.lang.String toString(double[])
  // java.lang.String toString(float[])
  // java.lang.String toString(int[])
  // java.lang.String toString(java.lang.Object[])
  // java.lang.String toString(long[])
  // java.lang.String toString(short[])
  // java.util.List asList(java.lang.Object[])
  // long[] copyOf(long[],int)
  // long[] copyOfRange(long[],int,int)
  // short[] copyOf(short[],int)
  // short[] copyOfRange(short[],int,int)
  // void fill(boolean[],boolean)
  // void fill(boolean[],int,int,boolean)
  // void fill(byte[],byte)
  // void fill(byte[],int,int,byte)
  // void fill(char[],char)
  // void fill(char[],int,int,char)
  // void fill(double[],double)
  // void fill(double[],int,int,double)
  // void fill(float[],float)
  // void fill(float[],int,int,float)
  // void fill(int[],int)
  // void fill(int[],int,int,int)
  // void fill(java.lang.Object[],int,int,java.lang.Object)
  // void fill(java.lang.Object[],java.lang.Object)
  // void fill(long[],int,int,long)
  // void fill(long[],long)
  // void fill(short[],int,int,short)
  // void fill(short[],short)
  // void sort(byte[])
  // void sort(byte[],int,int)
  // void sort(char[])
  // void sort(char[],int,int)
  // void sort(double[])
  // void sort(double[],int,int)
  // void sort(float[])
  // void sort(float[],int,int)
  // void sort(int[])
  // void sort(int[],int,int)
  // void sort(java.lang.Object[])
  // void sort(java.lang.Object[],int,int)
  // void sort(java.lang.Object[],int,int,java.util.Comparator)
  // void sort(java.lang.Object[],java.util.Comparator)
  // void sort(long[])
  // void sort(long[],int,int)
  // void sort(short[])
  // void sort(short[],int,int)
}

object Files {

}
