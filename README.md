psp.std - a non-standard standard library
=========================================

Background
----------

The scala standard library is lacking. This tries to supplement the missing parts and to play defense against its hostility to correctness where possible.

Usage
-----

    resolvers += "bintray/paulp" at "https://dl.bintray.com/paulp/maven"

    libraryDependencies += "org.improving" %% "psp-std" % "0.1.0-M1"

    import psp.std._

Requirements
------------

scala 2.11.x, java 6+ at this moment. java 7+ is a near-inevitability.
