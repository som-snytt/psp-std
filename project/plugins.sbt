def pspResolvers = Seq(
  Resolver.url("paulp/sbt-plugins", url("https://dl.bintray.com/paulp/sbt-plugins"))(Resolver.ivyStylePatterns),
  Resolver.url("paulp/maven", url("https://dl.bintray.com/paulp/maven"))
)

resolvers ++= pspResolvers

addSbtPlugin("org.improving" % "psp-libsbt" % "0.3.1-M9")
