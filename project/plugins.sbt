lazy val plugin = ProjectRef(file("/r/psp/libsbt"), "libsbt")

lazy val root = project in file(".") dependsOn plugin
