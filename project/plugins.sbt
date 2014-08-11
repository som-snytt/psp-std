// https://issues.scala-lang.org/browse/SI-8772
// resolvers += Resolver.sonatypeRepo("releases")
// Nope
//
// http://central.stage.sonatype.org/pages/consumers.html
resolvers += "Secured Central Repository" at "https://repo1.maven.org/maven2"

externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.2")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.6")
