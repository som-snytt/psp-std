package psp

import sbt._, Keys._

/** Code which we want access to in both the metabuild and the build.
 *  And if it comes up, the project itself.
 */
package object meta {
  def metaSettings = Seq(
                                resolvers +=  "paulp/maven" at "https://dl.bintray.com/paulp/maven",
    unmanagedSourceDirectories in Compile +=  baseDirectory.value / "../api/src/main",
    unmanagedSourceDirectories in Compile +=  baseDirectory.value / "project/src/main",
                      libraryDependencies ++= pluginIDs map (_.sbtPlugin),
                      libraryDependencies +=  "org.improving" %% "psp-const" % "1.0.1"
  )
  def pluginIDs = Seq(
    "me.lessis"        % "bintray-sbt"                % "0.1.2",
    "com.typesafe"     % "sbt-mima-plugin"            % "0.1.6",
    "com.gilt"         % "sbt-dependency-graph-sugar" % "0.7.4",
    "com.typesafe.sbt" % "sbt-javaversioncheck"       % "0.1.0"
  )
  implicit class ModuleIDOps(val m: ModuleID) extends AnyVal {
    def exceptScala: ModuleID = m excludeAll ExclusionRule("org.scala-lang")
    def sbtPlugin: ModuleID   = Defaults.sbtPluginExtra(m, "0.13", "2.10").exceptScala
  }
}
