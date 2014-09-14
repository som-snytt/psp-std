resolvers ++= Seq(
  Resolver.url("paulp/sbt-plugins", url("https://dl.bintray.com/paulp/sbt-plugins"))(Resolver.ivyStylePatterns),
  "paulp/maven" at "https://dl.bintray.com/paulp/maven"
)

libraryDependencies += Defaults.sbtPluginExtra("me.lessis" % "bintray-sbt" % "0.1.2", "0.13", "2.10") excludeAll ExclusionRule("org.scala-lang")
