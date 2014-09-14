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
    initialCommands in console :=  "import psp.std._",
           libraryDependencies +=  "org.improving" %% "psp-std" % "0.4.3"
  )
}
```

Or if a self-contained project isn't your bag:
```
libraryDependencies += "org.improving" %% s"psp-std" % "0.4.3"
```

Then ```sbt console``` and you can look around.
```
scala> Array(1, 2) === Array(1, 2)
res0: Boolean = true
```

Requirements
------------

scala 2.10+, java 7+.
