import psp.meta._

scalacOptions in Compile += "-language:_"

unmanagedSourceDirectories in Compile += baseDirectory.value / "../api/src/main"

unmanagedSourceDirectories in Compile += baseDirectory.value / "project/src/main"

libraryDependencies ++= pluginIDs map (_.sbtPlugin)
