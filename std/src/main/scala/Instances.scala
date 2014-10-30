package psp
package std

import api._
import psp.std.scalac.token.Keyword
import StdShow.stringShow

trait AlgebraInstances {
  implicit def identityAlgebra : BooleanAlgebra[Boolean]           = Algebras.Identity
  implicit def predicateAlgebra[A] : BooleanAlgebra[Predicate[A]]  = new Algebras.Predicate[A]
  implicit def intensionalSetAlgebra[A] : BooleanAlgebra[inSet[A]] = new Algebras.inSetAlgebra[A]
}

trait ReadInstances {
  implicit def bigDecRead: Read[BigDecimal] = Read(s => BigDecimal(s))
  implicit def bigIntRead: Read[BigInt]     = Read(s => BigInt(s))
  implicit def doubleRead: Read[Double]     = Read(_.toDouble)
  implicit def floatRead: Read[Float]       = Read(_.toFloat)
  implicit def intRead: Read[Int]           = Read(_.toInt)
  implicit def longRead: Read[Long]         = Read(_.toLong)
  implicit def regexRead: Read[Regex]       = Read(Regex)
  implicit def stringRead: Read[String]     = Read(s => s)
  implicit def uriRead: Read[jUri]          = Read(jUri)
}

trait OrderInstances {
  implicit def booleanOrder: Order[Boolean] = orderBy[Boolean](x => if (x) 1 else 0)
  implicit def byteOrder: Order[Byte]       = Order.fromInt[Byte](_ - _)
  implicit def charOrder: Order[Char]       = Order.fromInt[Char](_ - _)
  implicit def intOrder: Order[Int]         = Order.fromInt[Int](_ - _)
  implicit def longOrder: Order[Long]       = Order.fromLong[Long](_ - _)
  implicit def shortOrder: Order[Short]     = Order.fromInt[Short](_ - _)
  implicit def stringOrder: Order[String]   = Order.fromLong[String](_ compareTo _)

  implicit def indexOrder: Order[Index]              = orderBy[Index](_.indexValue)
  implicit def nthOrder: Order[Nth]                  = orderBy[Nth](_.nthValue)
  implicit def offsetOrder: Order[Offset]            = orderBy[Offset](_.offsetValue)
  implicit def preciseOrder[A <: Precise] : Order[A] = orderBy[A](_.value)

  implicit def tuple2Order[A: Order, B: Order] : Order[(A, B)]              = Order[(A, B)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2))
  implicit def tuple3Order[A: Order, B: Order, C: Order] : Order[(A, B, C)] = Order[(A, B, C)]((x, y) => Order.fold(x._1 compare y._1, x._2 compare y._2, x._3 compare y._3))

  implicit def sizePartialOrder: PartialOrder[Size] = PartialOrder(Size.partialCompare)
}

trait MonoidInstances {
  import StdZero._

  implicit def seqAddition[A] : Sums[Each[A]]       = Sums(_ ++ _)
  implicit def vectorAddition[A] : Sums[Direct[A]] = Sums(_ ++ _)
}

trait ZeroInstances {
  implicit def arrayZero[A: CTag] : Zero[Array[A]]           = Zero(Array[A]())
  implicit def bigDecimalZero: Zero[BigDecimal]              = Zero(BigDecimal(0))
  implicit def bigIntZero: Zero[BigInt]                      = Zero(BigInt(0))
  implicit def booleanZero: Zero[Boolean]                    = Zero(false)
  implicit def byteZero: Zero[Byte]                          = Zero(0.toByte)
  implicit def charZero: Zero[Char]                          = Zero(0.toChar)
  implicit def doubleZero: Zero[Double]                      = Zero(0d)
  implicit def docZero: Zero[Doc]                            = Zero(Doc.empty)
  implicit def floatZero: Zero[Float]                        = Zero(0f)
  implicit def fileTimeZero: Zero[FileTime]                  = Zero(NoFileTime)
  implicit def jFileZero: Zero[jFile]                        = Zero(NoFile)
  implicit def jPathZero: Zero[Path]                         = Zero(NoPath)
  implicit def indexZero: Zero[Index]                        = Zero(NoIndex)
  implicit def intZero: Zero[Int]                            = Zero(0)
  implicit def longZero: Zero[Long]                          = Zero(0L)
  implicit def optionZero[A] : Zero[Option[A]]               = Zero(None)
  implicit def pVectorZero[A] : Zero[Direct[A]]             = Zero(Direct())
  implicit def pSeqZero[A] : Zero[Each[A]]                   = Zero(Each.elems())
  implicit def exSetZero[A: HashEq] : Zero[exSet[A]]         = Zero(exSet[A]())
  implicit def exMapZero[K: HashEq, V] : Zero[exMap[K, V]]   = Zero(exMap[K, V]())
  implicit def scIterableZero[A] : Zero[scIterable[A]]       = Zero(Nil)
  implicit def scIteratorZero[A] : Zero[scIterator[A]]       = Zero(scIterator.empty)
  implicit def scSeqZero[A] : Zero[scSeq[A]]                 = Zero(Nil)
  implicit def scSetZero[A] : Zero[scSet[A]]                 = Zero(sciSet())
  implicit def scTraversableZero[A] : Zero[scTraversable[A]] = Zero(Nil)
  implicit def sciListZero[A] : Zero[sciList[A]]             = Zero(Nil)
  implicit def sciMapZero[A, B] : Zero[sciMap[A, B]]         = Zero(sciMap())
  implicit def sciVectorZero[A] : Zero[sciVector[A]]         = Zero(sciVector())
  implicit def shortZero: Zero[Short]                        = Zero(0.toShort)
  implicit def showZero[A]: Zero[Show[A]]                    = Zero(Show.natural[A]())
  implicit def shownZero: Zero[Shown]                        = Zero(Shown.empty)
  implicit def stringZero: Zero[String]                      = Zero("")
  implicit def unitZero: Zero[Unit]                          = Zero(())
  implicit def viewZero[A] : Zero[View[A]]                   = Zero(exView())
}

trait EqInstances {
  import HashEq.natural

  implicit def booleanEq: HashEq[Boolean] = natural()
  implicit def byteEq: HashEq[Byte]       = natural()
  implicit def charEq: HashEq[Char]       = natural()
  implicit def doubleEq: HashEq[Double]   = natural()
  implicit def floatEq: HashEq[Float]     = natural()
  implicit def intEq: HashEq[Int]         = natural()
  implicit def longEq: HashEq[Long]       = natural()
  implicit def shortEq: HashEq[Short]     = natural()
  implicit def unitHash: HashEq[Unit]     = natural()

  implicit def indexEq: HashEq[Index]             = natural()
  implicit def jTypeEq: HashEq[jType]             = natural()
  implicit def nthEq: HashEq[Nth]                 = natural()
  implicit def offsetEq: HashEq[Offset]           = natural()
  implicit def stringEq: HashEq[String]           = natural()
  implicit def policyClassEq: HashEq[PolicyClass] = natural()

  implicit def sizeEq: HashEq[Size] = HashEq(Size.equiv, Size.hash)
  implicit def pathEq: HashEq[Path] = hashEqBy[Path](_.toString)

  implicit def tryEq[A](implicit z1: Eq[A], z2: Eq[Throwable]): Eq[Try[A]] = Eq {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y
    case _                        => false
  }

  // Since Sets are created with their own notion of equality, you can't pass
  // an Eq instance. Map keys are also a set.
  implicit def arrayHashEq[A: HashEq] : HashEq[Array[A]]       = hashEqBy[Array[A]](_.pvec)
  implicit def vectorHashEq[A: Eq] : HashEq[Direct[A]]        = HashEq((xs, ys) => (xs.toScalaVector corresponds ys.toScalaVector)(_ === _), _.toScalaVector.##)
  implicit def exSetEq[A] : Eq[exSet[A]]                       = Eq(symmetrically[exSet[A]](_ isSubsetOf _))
  implicit def exMapEq[K, V: Eq] : Eq[exMap[K, V]]             = Eq((xs, ys) => xs.domain === ys.domain && (equalizer(xs.apply, ys.apply) forall xs.keys))
  implicit def tuple2Eq[A: HashEq, B: HashEq] : HashEq[(A, B)] = HashEq[(A, B)]({ case ((x1, y1), (x2, y2)) => x1 === x2 && y1 === y2 }, x => x._1.hash + x._2.hash)

  implicit def equivFromOrder[A: Order] : Eq[A] = Eq[A](_ compare _ eq Cmp.EQ)

  def equalizer[A, B: Eq](f: A => B, g: A => B): FunctionEqualizer[A, B] = new FunctionEqualizer(f, g)
  def symmetrically[A](f: Relation[A]): Relation[A]                      = (x, y) => f(x, y) && f(y, x)
}

/** An incomplete selection of show compositors.
 *  Not printing the way scala does.
 */
trait ShowInstances extends ShowForeach {
  implicit def attrNameShow : Show[jAttributeName]            = Show.natural()
  implicit def booleanShow: Show[Boolean]                     = Show.natural()
  implicit def charShow: Show[Char]                           = Show.natural()
  implicit def doubleShow: Show[Double]                       = Show.natural()
  implicit def indexShow: Show[Index]                         = showBy[Index](_.indexValue)
  implicit def intShow: Show[Int]                             = Show.natural()
  implicit def longShow: Show[Long]                           = Show.natural()
  implicit def nthShow: Show[Nth]                             = showBy[Nth](_.nthValue)
  implicit def scalaNumberShow: Show[ScalaNumber]             = Show.natural()
  implicit def showClass: Show[jClass]                        = Show(_.shortName)
  implicit def showDirect: Show[ShowDirect]                   = Show(_.to_s)
  implicit def showPath: Show[Path]                           = showBy[Path](_.toString)
  implicit def stringShow: Show[String]                       = Show(x => x)
  implicit def stackTraceElementShow: Show[StackTraceElement] = Show("\tat" + _ + "\n")
  implicit def keywordShow: Show[Keyword] = Show[Keyword] {
    case Keyword.Empty            => ""
    case Keyword.ClassConstructor => ""
    case Keyword.ValueParameter   => ""
    case Keyword.TypeParameter    => ""
    case Keyword.Constructor      => "def this"
    case Keyword.PackageObject    => "package object"
    case Keyword.CaseObject       => "case object"
    case Keyword.CaseClass        => "case class"
    case k                        => k.toString.toLowerCase
  }
  implicit def showSize: Show[Size] = Show[Size] {
    case IntSize(size)         => show"$size"
    case LongSize(size)        => show"$size"
    case Bounded(lo, Infinite) => show"$lo+"
    case Bounded(lo, hi)       => show"[$lo,$hi]"
    case Infinite              => "<inf>"
  }

  implicit def optShow[A: Show] : Show[Option[A]]             = Show(_.fold("-")(_.to_s))
  implicit def tupleShow[A: Show, B: Show] : Show[(A, B)]     = Show { case (x, y) => show"$x -> $y" }
}


trait ShowForeach0 {
  def maxElements: Precise = 10
  def minElements: Precise = 3

  implicit def exViewShow[A] : Show[View[A]]      = Show(_.viewOps mk_s " ")
  implicit def exSeqShow[A: Show] : Show[Each[A]] = Show(foreachString[A])

  def foreachString[A: Show](xs: Each[A]): String = xs match {
    case xs: ShowDirect => xs.to_s
    case _              =>
      xs splitAt maxElements.lastIndex match {
        case SplitView(xs, ys) if ys.isEmpty => "[ " ~ (xs mk_s ", ") ~ " ]"
        case SplitView(xs, _)                => "[ " ~ (xs take minElements mk_s ", ") ~ ", ... ]"
      }
  }
}

trait ShowForeach1 extends ShowForeach0 {
  implicit def inSetShow[A: Show] : Show[inSet[A]] = Show(inSetString[A])

  def inSetString[A: Show](xs: inSet[A]): String = {
    import IntensionalSet._
    xs match {
      case xs: ExtensionalSet[A] => (xs: Each[A]).to_s //show"$xs"
      case Complement(xs)        => show"Not($xs)"
      case Intersect(lhs, rhs)   => show"Intersect($lhs, $rhs)"
      case Union(lhs, rhs)       => show"Union($lhs, $rhs)"
      case Diff(lhs, rhs)        => show"Diff($lhs, $rhs)"
      case Filtered(lhs, rhs)    => pp"Filtered($lhs, $rhs)"
      case Impl(member, heq)     => pp"Impl($member, $heq)"
    }
  }
}

trait ShowForeach extends ShowForeach1 {
  self: ShowInstances =>

  implicit def jCollectionShow[A: Show] : Show[jIterable[A]]   = showBy[jIterable[A]](fromJava)
  implicit def sCollectionShow[A: Show] : Show[sCollection[A]] = showBy[sCollection[A]](fromScala)
  implicit def arrayShow[A: Show] : Show[Array[A]]             = showBy[Array[A]](Direct.fromArray)
  implicit def exSetShow[A: Show] : Show[exSet[A]]             = showBy(x => x: Each[A])
  implicit def exMapShow[K: Show, V: Show] : Show[exMap[K, V]] = Show(_.entries.tabular(_._1.to_s, _ => "->", _._2.to_s))
}
