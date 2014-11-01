package psp
package std

import api._, StdEq._

class FunctionEqualizer[A, B : Eq](f: A => B, g: A => B) extends (A ?=> B) {
  def isDefinedAt(x: A) = f(x) === g(x)
  def apply(x: A): B    = f(x)
  def forall(xs: Each[A]): Boolean = xs forall isDefinedAt
}

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T ?=> R) with ForceShowDirect {
  def isDefinedAt(x: T) = f match {
    case f: PartialFunction[_, _] => f isDefinedAt x
    case _                        => true
  }
  def apply(x: T): R = f(x)
}

final class PolicyLoader(val classMap: ExMap[String, Bytes]) extends ClassLoader {
  private val keys        = classMap.domain.pvec
  private val instanceMap = scmMap[String, jClass]()
  private val errorMap    = scmMap[String, LinkageError]()
  private def isNoClassDefFoundError(t: Throwable) = t match {
    case _: NoClassDefFoundError => true
    case _                       => false
  }

  def totalClasses = classMap.size
  def names        = keys
  def classes      = names map (x => findClass(x))
  def errors       = errorMap.toMap
  def missing      = errorMap.m.pmap filterValues isNoClassDefFoundError keys
  def otherErrors  = errorMap.m.pmap filterValues !(isNoClassDefFoundError _)
  def totalBytes   = classMap.values map (_.length) sum

  def define(name: String): jClass               = define(name, classMap(name))
  def define(name: String, bytes: Bytes): jClass = defineClass(name, bytes, 0, bytes.length, null)

  private def doDefine(name: String): jClass = try define(name) catch {
    case t: LinkageError => errorMap(name) = t ; null
    case t: Throwable    => println(s"Caught $t") ; null
  }

  override def findClass(name: String): jClass = instanceMap.getOrElse(name,
    findLoadedClass(name) match {
      case cl: jClass                     => cl
      case _ if !(classMap contains name) => super.findClass(name)  /** NoClassDefFound */
      case _                              => doDefine(name) doto (instanceMap(name) = _)
    }
  )

  override def toString = s"Loader($totalClasses classes, $totalBytes bytes)"
}
