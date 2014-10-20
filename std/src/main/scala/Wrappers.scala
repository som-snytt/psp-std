package psp
package std

import StdEq._

object +: {
  def unapply[A](xs: Array[A])       = if (xs.length == 0) None else Some(xs(0) -> (xs drop 1))
  def unapply[A](xs: api.Foreach[A])     = if (xs.isEmpty) None else Some(xs.head -> xs.drop(1))
  def unapply[A](xs: sCollection[A]) = if (xs.isEmpty) None else Some(xs.head -> xs.tail)
}

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T => R) with ShowDirect {
  def apply(x: T): R = f(x)
}
final class LabeledPartialFunction[-T, +R](pf: T ?=> R, val to_s: String) extends (T ?=> R) with ShowDirect {
  def isDefinedAt(x: T) = pf isDefinedAt x
  def apply(x: T): R    = pf(x)
}
final class Utf8(val bytes: Array[Byte]) extends AnyVal {
  def chars: Chars = scala.io.Codec fromUTF8 bytes
  def to_s: String = new String(chars)
  override def toString = to_s
}

trait ClassLoaderTrait {
  def loader: jClassLoader

  def parentChain: pSeq[jClassLoader] = {
    def loop(cl: jClassLoader): pSeq[jClassLoader] = cl match {
      case null => Nil
      case _    => cl +: loop(cl.getParent)
    }
    loop(loader)
  }
  def uris: pSeq[jUri] = loader match {
    case cl: URLClassLoader => cl.getURLs.pseq map (_.toURI)
    case _                  => Nil
  }
}

final class PolicyLoader(val classMap: pMap[String, Bytes]) extends ClassLoader {
  private val keys        = classMap.keyVector
  private val instanceMap = scmMap[String, jClass]()
  private val errorMap    = scmMap[String, LinkageError]()
  private def isNoClassDefFoundError(t: Throwable) = t match {
    case _: NoClassDefFoundError => true
    case _                       => false
  }

  def totalClasses = classMap.sizeInfo
  def names        = keys
  def classes      = names map findClass
  def errors       = errorMap.toMap
  def missing      = errorMap.pmap filterValues isNoClassDefFoundError keys
  def otherErrors  = errorMap.pmap filterValues !(isNoClassDefFoundError _)
  def totalBytes   = classMap.values map (_.length) sum

  def define(name: String): jClass               = define(name, classMap(name))
  def define(name: String, bytes: Bytes): jClass = defineClass(name, bytes, 0, bytes.length, null)

  private def doDefine(name: String): jClass = try define(name) catch {
    case t: LinkageError => errorMap(name) = t ; null
    case t: Throwable    => println(s"Caught $t") ; null
  }

  override def findClass(name: String) = instanceMap.getOrElse(name,
    findLoadedClass(name) match {
      case cl: jClass                     => cl
      case _ if !(classMap contains name) => super.findClass(name)  /** NoClassDefFound */
      case _                              => doDefine(name) doto (instanceMap(name) = _)
    }
  )

  override def toString = s"Loader($totalClasses classes, $totalBytes bytes)"
}
