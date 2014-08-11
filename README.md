psp.std - a non-standard standard library
=========================================

[![Build Status](https://travis-ci.org/paulp/psp-std.svg?branch=master)](https://travis-ci.org/paulp/psp-std)

Background
----------

The scala standard library is lacking. This library attempts to fill some of the gaps and play defense against its hostility to correctness where possible.
See [psp-view](view/README.md) for some details.

Usage
-----

    resolvers += "bintray/paulp" at "https://dl.bintray.com/paulp/maven"

    scalaVersion := "2.11.2"

    // psp-view depends on psp-std
    libraryDependencies += "org.improving" %% "psp-std" % "0.2.0-M1"
    libraryDependencies += "org.improving" %% "psp-view" % "0.2.0-M1"

    initialCommands in console := "import psp.std._"

Requirements
------------

scala 2.11.x, java 6+ at this moment. java 7+ is a near-inevitability.
