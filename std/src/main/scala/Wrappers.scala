package psp
package std

import api._, StdEq._

object +: {
  def unapply[A](xs: Array[A])       = if (xs.length == 0) None else Some(xs(0) -> (xs drop 1))
  def unapply[A](xs: pSeq[A])        = xs match { case PSeq(hd, _*) => Some(hd -> (xs drop 1)) ; case _ => None }
  def unapply[A](xs: sCollection[A]) = if (xs.isEmpty) None else Some(xs.head -> xs.tail)
}

class FunctionEqualizer[A, B : Eq](f: A => B, g: A => B) extends (A ?=> B) {
  def isDefinedAt(x: A) = f(x) === g(x)
  def apply(x: A): B    = f(x)
  def forall(xs: pSeq[A]): Boolean = xs forall isDefinedAt
}

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T ?=> R) with ShowDirect {
  def isDefinedAt(x: T) = f match {
    case f: PartialFunction[_, _] => f isDefinedAt x
    case _                        => true
  }
  def apply(x: T): R = f(x)
  override def toString = to_s
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

final class PolicyClass(val clazz: jClass) extends AnyVal with ShowDirect {
  def isAnnotation     = clazz.isAnnotation
  def isAnonymousClass = clazz.isAnonymousClass
  def isArray          = clazz.isArray
  def isEnum           = clazz.isEnum
  def isInterface      = clazz.isInterface
  def isLocalClass     = clazz.isLocalClass
  def isMemberClass    = clazz.isMemberClass
  def isPrimitive      = clazz.isPrimitive
  def isSynthetic      = clazz.isSynthetic

  def ancestorNames: pVector[String]           = ancestors map (_.rawName)
  def ancestors: pVector[PolicyClass]          = this transitiveClosure (_.parents) pvec
  def exists                                   = clazz != null
  def fields: pVector[jField]                  = clazz.getFields.pvec
  def getCanonicalName: String                 = clazz.getCanonicalName
  def getClassLoader: ClassLoader              = clazz.getClassLoader
  def getClasses: pVector[PolicyClass]         = clazz.getClasses.pvec map (x => x)
  def getComponentType: PolicyClass            = clazz.getComponentType
  def getDeclaredClasses: pVector[PolicyClass] = clazz.getDeclaredClasses.pvec map (x => x)
  def getDeclaringClass: PolicyClass           = clazz.getDeclaringClass
  def getEnclosingClass: PolicyClass           = clazz.getEnclosingClass
  def getInterfaces: pVector[PolicyClass]      = clazz.getInterfaces.pvec map (x => x)
  def getSuperclass: Option[PolicyClass]       = Option(clazz.getSuperclass) map (x => new PolicyClass(x))
  def hasModuleName: Boolean                   = rawName endsWith "$"
  def methods: pVector[jMethod]                = clazz.getMethods.pvec
  def nameSegments: pVector[String]            = rawName.dottedSegments
  def pClass: PolicyClass                      = this
  def parentInterfaces: pVector[PolicyClass]   = clazz.getInterfaces.pvec map (x => x)
  def parents: pVector[PolicyClass]            = getSuperclass.pvec ++ parentInterfaces
  def qualifiedName: String                    = rawName.mapSplit('.')(decodeName)
  def rawName: String                          = clazz.getName
  def shortName: String                        = unqualifiedName
  def to_s                                     = s"$clazz"
  def unqualifiedName: String                  = decodeName(nameSegments.last)

  override def toString = to_s
}

final class PolicyLoader(val classMap: pMap[String, Bytes]) extends ClassLoader {
  private val keys        = classMap.keyVector
  private val instanceMap = scmMap[String, jClass]()
  private val errorMap    = scmMap[String, LinkageError]()
  private def isNoClassDefFoundError(t: Throwable) = t match {
    case _: NoClassDefFoundError => true
    case _                       => false
  }

  def totalClasses = classMap.size
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
