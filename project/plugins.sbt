// https://issues.scala-lang.org/browse/SI-8772
resolvers += Resolver.sonatypeRepo("releases")

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.2")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.6")
