package psp
package std

/** Sorry there's no way to put mutable and immutable into the namespace.
 *  You can import them individually into every file until you die.
 */
import scala.collection.{ mutable, immutable, generic }

/** Yes I know all about implicit classes.
 *  There's no way to write an implicit value class which doesn't hardcode
 *  its location into an object. Separating the implicit conversion from
 *  the class allows clients to build their own package object.
 *
 *  This is all a consequence of scala offering no means for managing namespaces,
 *  so namespace management has become hopelessly entangled with unrelated concerns
 *  like inheritance, specificity, method dispatch, and so forth.
 */
trait PackageLevel extends Implicits with Creators {
  val NoIndex = Index.empty
  val NoNth   = Nth.empty

  type ClassTag[A]            = scala.reflect.ClassTag[A]
  type GenTraversableOnce[+A] = scala.collection.GenTraversableOnce[A]
}

trait Creators {
  def index(x: Int): Index   = Index(x)
  def offset(x: Int): Offset = Offset(x)
  def nth(x: Int): Nth       = Nth(x)

  // Mostly obviating the need for those mutable/immutable identifiers.
  def mutableSeq[A](xs: A*): mutable.Seq[A]                 = mutable.Seq(xs: _*)
  def mutableSet[A](xs: A*): mutable.Set[A]                 = mutable.Set(xs: _*)
  def mutableMap[K, V](kvs: (K, V)*): mutable.Map[K, V]     = mutable.Map[K, V](kvs: _*)
  def immutableSeq[A](xs: A*): immutable.Seq[A]             = immutable.Seq(xs: _*)
  def immutableSet[A](xs: A*): immutable.Set[A]             = immutable.Set(xs: _*)
  def immutableMap[K, V](kvs: (K, V)*): immutable.Map[K, V] = immutable.Map[K, V](kvs: _*)

  // OrderedMap is our own creation since SortedMap is way overspecified
  // and LinkedHashMap is too slow and only comes in a mutable variety.
  def orderedMap[K, V](kvs: (K, V)*): OrderedMap[K, V]                 = new OrderedMap[K, V](kvs map (_._1), kvs.toMap)
  def orderedMap[K, V](keys: Seq[K], map: Map[K, V]): OrderedMap[K, V] = new OrderedMap[K, V](keys, map)

  // A few builders.
  def listBuilder[A](xs: A*)            = List.newBuilder[A] ++= xs
  def arrayBuilder[A: ClassTag](xs: A*) = Array.newBuilder[A] ++= xs
  def vectorBuilder[A](xs: A*)          = Vector.newBuilder[A] ++= xs
}

trait Implicits {
  implicit def showStringContextOps(sc: StringContext): ShowInterpolator = new ShowInterpolator(sc)

  // Implicit classes for non-collection types.
  implicit def stringExtensionOps(s: String): StringExtensionOps            = new StringExtensionOps(s)
  implicit def arrayExtensionOps[A](xs: Array[A]): ArrayExtensionOps[A]     = new ArrayExtensionOps[A](xs)
  implicit def anyExtensionOps[A](x: A): AnyExtensionOps[A]                 = new AnyExtensionOps[A](x)
  implicit def tryExtensionOps[A](x: scala.util.Try[A]): TryExtensionOps[A] = new TryExtensionOps[A](x)

  // Implicit classes with typeclass-based membership.
  implicit def showExtensionOps[A](x: A): ShowExtensionOps[A] = new ShowExtensionOps[A](x)
  implicit def eqExtensionOps[A](x: A): EqExtensionOps[A]     = new EqExtensionOps[A](x)

  // The delicate dance against scala's hostile-to-correctness intrinsics.
  implicit def showableToShown[A: Show](x: A): Shown[A] = new Shown[A](x)

  // Implicit classes for various collections. Mostly we try not to split hairs and attach to GenTraversableOnce.
  // It's not like you have any idea what the performance characteristics of the target are anyway.
  implicit def sortedMapExtensionOps[K, V](xs: scala.collection.SortedMap[K, V]): SortedMapExtensionOps[K, V]                      = new SortedMapExtensionOps[K, V](xs)
  implicit def mapExtensionOps[K, V](xs: scala.collection.Map[K, V]): MapExtensionOps[K, V]                                        = new MapExtensionOps[K, V](xs)
  implicit def seqExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): SeqExtensionOps[CC, A]                             = new SeqExtensionOps[CC, A](xs)
  implicit def seqNthExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): AddNthApplyToSeq[CC, A]                         = new AddNthApplyToSeq[CC, A](xs)
  implicit def seqIndexExtensionOps[CC[X] <: scala.collection.Seq[X], A](xs: CC[A]): AddIndexApplyToSeq[CC, A]                     = new AddIndexApplyToSeq[CC, A](xs)
  implicit def genTraversableOnceExtensionOps[CC[X] <: GenTraversableOnce[X], A](xs: CC[A]): GenTraversableOnceExtensionOps[CC, A] = new GenTraversableOnceExtensionOps[CC, A](xs)
}
