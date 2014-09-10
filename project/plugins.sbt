resolvers ++= Seq(
  Resolver.url("paulp/sbt-plugins", url("https://dl.bintray.com/paulp/sbt-plugins"))(Resolver.ivyStylePatterns),
  "paulp/maven" at "https://dl.bintray.com/paulp/maven"
)

addSbtPlugin("org.improving" % "psp-libsbt" % "0.3.1-M12")
