package psp
package build

import sbt._, Keys._

/** Encapsulation of the sbt craziness.
 */
final class Stated(val state: State) {
  val extracted = Project extract state
  import extracted._

  def settings     = structure.settings
  def relation     = Project.relation(structure, true)
  def scopedKeys   = relation._1s.toSeq
  def attrKeys     = scopedKeys map (_.key)
  def projectScope = Load projectScope currentRef
  def remaining    = state.remainingCommands

  def apply[A](key: SettingKey[A]): A = extracted get key
  def get[A](key: TaskKey[A]): Option[A] = Project.evaluateTask(key, state) match {
    case None           => None
    case Some(Inc(inc)) => Incomplete.show(inc.tpe) ; None
    case Some(Value(v)) => Some(v)
  }
  def put[A](key: SettingKey[A], value: A): Stated = set(key := value)
  def set(settings: Setting[_]*): Stated           = map(extracted.append(settings, _))
  def run(commands: String*): Stated               = map(_.copy(remainingCommands = remaining ++ commands))
  def map(f: State => State): Stated               = new Stated(f(state))
}

trait Versioning {
  def lastReleasedArtifact = stdArtifact(milestoneName(lastMilestone))
  def nextMilestone        = lastMilestone + 1
  def nextMilestoneVersion = milestoneName(nextMilestone)
  def snapshotVersion      = nextMilestoneVersion + "-SNAPSHOT"
  def buildVersion         = if (isRelease) targetReleaseVersion else if (isMilestone) nextMilestoneVersion else snapshotVersion

  // Mima won't resolve the %% cross version.
  def stdArtifact(version: String): ModuleID = pspOrg % "psp-std_2.11" % version
  def milestoneName(n: Int): String          = "%s-M%s".format(targetReleaseVersion, n)
}
