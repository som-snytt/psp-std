import psp.meta._

unmanagedSourceDirectories in Compile += baseDirectory.value / "project/src/main"

libraryDependencies ++= pluginIDs map (_.sbtPlugin)
