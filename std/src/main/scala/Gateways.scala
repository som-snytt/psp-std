package psp
package std

import api._
import scala.{ collection => sc }
import scala.Any
import scala.Predef.StringCanBuildFrom

trait StdGateways extends Any
      with StdBuilds
      with StdOps
      with StdUniversal

trait StdBuilds0 extends Any                 { implicit def implicitBuildsFromCBF[A, That](implicit z: CanBuild[A, That]): Builds[A, That] = Builds wrap z          }
trait StdBuilds1 extends Any with StdBuilds0 { implicit def implicitBuildsList[A] : Builds[A, PolicyList[A]]                               = PolicyList.builder[A]  }
trait StdBuilds2 extends Any with StdBuilds1 { implicit def implicitBuildsDirect[A] : Builds[A, Direct[A]]                                 = Direct.builder[A]      }
trait StdBuilds3 extends Any with StdBuilds2 { implicit def implicitBuildsArray[A: CTag] : Builds[A, Array[A]]                             = Direct.arrayBuilder[A] }
trait StdBuilds  extends Any with StdBuilds3 { implicit def implicitBuildsString: Builds[Char, String]                                     = Direct.stringBuilder() }

trait StdOps extends Any {
  implicit def infixOpsPartialOrder[A: PartialOrder](x: A): infix.PartialOrderOps[A] = new infix.PartialOrderOps[A](x)
  implicit def infixOpsOrder[A: Order](x: A): infix.OrderOps[A]                      = new infix.OrderOps[A](x)
  implicit def infixOpsLabelable[A: Labelable](x: A): infix.LabelableOps[A]          = new infix.LabelableOps[A](x)
  implicit def infixOpsAlgebra[A: BooleanAlgebra](x: A): infix.AlgebraOps[A]         = new infix.AlgebraOps[A](x)
  implicit def infixOpsEq[A: Eq](x: A): infix.EqOps[A]                               = new infix.EqOps[A](x)
  implicit def infixOpsHash[A: Hash](x: A): infix.HashOps[A]                         = new infix.HashOps[A](x)
}

// Prefer opsAnyRef.
trait StdUniversal0 extends Any                   { implicit def opsAny[A](x: A): ops.AnyOps[A]                 = new ops.AnyOps[A](x)    }
trait StdUniversal extends Any with StdUniversal0 { implicit def opsAnyRef[A <: AnyRef](x: A): ops.AnyRefOps[A] = new ops.AnyRefOps[A](x) }
