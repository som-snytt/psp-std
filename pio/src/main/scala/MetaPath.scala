package psp
package std
package pio

import api._
import MetaPath._
import StdEq._

final case class SinglePath(path: Path) extends AnyVal {
  def entries        = path.entries.pseq
  def deepEntries    = path.deepEntries.pseq
  def deepDirClasses = path.deepClasses map path.relativize
  def deepClasses    = if (path.isDirectory() && !path.isAbsolute) deepDirClasses else path.deepClasses

  override def toString = path.toString
}
final case class SinglePaths(singles: pVector[SinglePath]) extends Direct[SinglePath] {
  def size                           = singles.size
  def foreach(f: SinglePath => Unit) = singles foreach f
  def elemAt(idx: Index)             = singles(idx)
  def length                         = singles.length
  def paths                          = singles map (_.path)
  def entries                        = paths flatMap (_.entries)
  // def deepEntries                 = paths flatMap (_.deepEntries)
  // def packageNames                = deepFiles flatMap packagesIn dsort

  val pathToPackage: pMap[Path, pVector[String]] = paths mapOnto (_.deepPackageNames)
  val packageToPath: pMap[String, pVector[Path]] = pathToPackage.values.flatMap(x => x).distinct mapOnto (pkg => paths filter (p => pathToPackage(p) containsByEquals pkg))
}

/** A MetaPath encapsulates the character content of a classpath string as
 *  well as its contextual semantics. Resolving a metapath involves first
 *  flattening it, which means splitting on the classpath separator so that
 *  there are no composite paths, and then expanding each one, which means
 *  replacing *-paths and extdir paths with the contents of those directories
 *  where appropriate.
 */
case class MetaPath(path: Path, style: MetaStyle) {
  private def sep     = java.io.File.pathSeparatorChar
  private def chars   = path.toString
  private def singles = chars splitChar sep map single

  private def newPath(s: String) = path.getFileSystem getPath s
  private def single(s: String): MetaPath = copy(path = newPath(s))

  def isComposite                = chars containsChar sep
  def flatten: pVector[MetaPath] = if (isComposite) singles else this :: Nil
  def expanded: SinglePaths      = MetaPath expansion this
  def entries: pVector[Path]     = expanded flatMap (_.entries)

  override def toString = s"meta[$path]"
}

object MetaPath {
  def expansion(mp: MetaPath): SinglePaths = SinglePaths(
    if (mp.isComposite)
      mp.flatten flatMap expansion
    else mp.style match {
      case Expand     => mp.path.entries map SinglePath
      case ExpandStar => mp.path.parent.entries map SinglePath
      case Literal    => newVector(SinglePath(mp.path))
    }
  )

  sealed trait MetaStyle
  case object Literal extends MetaStyle
  case object Expand extends MetaStyle
  case object ExpandStar extends MetaStyle

  def literal(uri: jUri): MetaPath     = literal(uri.fs)
  def literal(chars: String): MetaPath = literal(path(chars))
  def literal(path: Path): MetaPath    = MetaPath(path, Literal)
  def expand(chars: String)            = MetaPath(path(chars), Expand)
  def expandStar(chars: String)        = MetaPath(path(chars), ExpandStar)
}
