package psp

import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import psp.core.impl._

package object core extends PspUtility with PspTypes with PspHighPriority with PspShadowScala with JioCreation {
  def zeroSize    = Size.Zero
  def unknownSize = SizeInfo.Unknown

  def jClassOf[T: ClassTag] : jClass[T] = classTag[T].runtimeClass.castTo[jClass[T]]
  def classTag[T: ClassTag] : ClassTag[T] = implicitly[ClassTag[T]]

  def labelpf[T, R](label: String)(pf: T =?> R): T =?> R = new LabeledPartialFunction(pf, label)
  def failEmpty(operation: String): Nothing = throw new NoSuchElementException(s"$operation on empty collection")
  def fail(msg: String): Nothing            = throw new RuntimeException(msg)

}
