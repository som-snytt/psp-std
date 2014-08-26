psp.std - a non-standard standard library
=========================================

[![Build Status](https://travis-ci.org/paulp/psp-std.svg?branch=master)](https://travis-ci.org/paulp/psp-std)

Background
----------

The scala standard library is lacking. This library attempts to fill some of the gaps and play defense against its hostility to correctness where possible.
See [views](views.md) for some details.

Usage
-----

Suggested contents for ```project/Build.scala```. You'll also want [sbt](https://github.com/paulp/sbt-extras).

```scala
package scratch

import sbt._, Keys._

object ScratchProject extends sbt.Build {
  lazy val scratch = project in file(".") settings (
                          name :=  "scratch-project",
                     resolvers +=  "bintray/paulp" at "https://dl.bintray.com/paulp/maven",
                  scalaVersion :=  "2.11.2",
                 scalacOptions ++= Seq("-language:_"),
    initialCommands in console :=  "import psp.std._",
           libraryDependencies +=  "org.improving" %% "psp-std" % "0.3.0-M1"
  )
}
```

Or if a self-contained project isn't your bag:
```
libraryDependencies += "org.improving" %% s"psp-std" % "0.3.0-M1"
```

Then ```sbt console``` and you can look around.
```
scala> Array(1, 2) === Array(1, 2)
res0: Boolean = true
```

Requirements
------------

scala 2.11.x, java 6+ at this moment. java 7+ is a near-inevitability.
