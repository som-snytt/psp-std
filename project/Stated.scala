package psp
package build

import sbt._, Keys._
import scala.sys.process.Process

final case class ProjectAndVersions(p: ProjectReference, versions: Seq[String])

/** Encapsulation of the sbt craziness.
 */
final class Stated(val state: State) {
  val extracted = Project extract state
  import extracted._

  def currentRef     = extracted.currentRef
  def thisRef        = apply(thisProjectRef)
  def thisProject    = apply(Keys.thisProject)
  def currentProject = currentRef.project
  def settings       = structure.settings
  def relation       = Project.relation(structure, true)
  def scopedKeys     = relation._1s.toSeq
  def attrKeys       = scopedKeys map (_.key)
  def projectScope   = Load projectScope currentRef
  def remaining      = state.remainingCommands

  // private[this] methods extracted from Extracted
  def inCurrent[T](key: SettingKey[T]): Scope     = if (key.scope.project == This) key.scope.copy(project = Select(currentRef)) else key.scope
  def resolve[T](key: ScopedKey[T]): ScopedKey[T] = Project.mapScope(Scope.resolveScope(GlobalScope, currentRef.build, rootProject))(key.scopedKey)

  def apply[A](key: SettingKey[A]): A                       = applyScoped[A](inCurrent(key), key.key)
  def applyScoped[A](key: ScopedKey[A]): A                  = applyScoped[A](key.scope, key.key)
  def applyScoped[A](scope: Scope, key: AttributeKey[A]): A = structure.data.get(scope, key) getOrElse sys.error(s"Undefined: $key in $scope")

  def getOpt[A](key: SettingKey[A]): Option[A] = extracted getOpt key
  def runTask[A](key: TaskKey[A])              = extracted.runTask(key, state)

  def justRun[A](key: TaskKey[A])  = runTask(key)._1
  def justEval[A](key: TaskKey[A]) = runTask(key)._2

  def put[A](key: SettingKey[A], value: A): Stated = set(key := value)
  def runAll[A](task: TaskKey[A]): Stated          = new Stated(extracted.runAggregated(task in thisRef, state))
  def set(settings: Setting[_]*): Stated           = new Stated(extracted.append(settings, state))
  def run(commands: String*): Stated               = new Stated(state.copy(remainingCommands = remaining ++ commands))

  def err(msg: String)  = apply(sLog) error msg
  def warn(msg: String) = apply(sLog) warn msg
}
