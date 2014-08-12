// No thank you re: scala 2.10.2 artifacts.
def plugin(m: ModuleID) = Defaults.sbtPluginExtra(m, "0.13", "2.10") excludeAll ExclusionRule("org.scala-lang")

libraryDependencies ++= Seq(
  plugin("me.lessis"    % "bintray-sbt"     % "0.1.2"),
  plugin("com.typesafe" % "sbt-mima-plugin" % "0.1.6")
)
