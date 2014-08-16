import psp.meta._

ivyLoggingLevel := UpdateLogging.Quiet

unmanagedSourceDirectories in Compile += baseDirectory.value / "project/src/main"

libraryDependencies ++= pluginIDs map (_.sbtPlugin)
