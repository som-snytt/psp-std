def libsbtVersion = sys.props.getOrElse("libsbt.version", "0.4.3")

def libsbtRef = addSbtPlugin("org.improving" % "psp-libsbt" % libsbtVersion)

def add(p: Project) = sys.props get "libsbt.path" match {
  case Some(f) => println(s"Using libsbt at $f") ; p dependsOn ProjectRef(file(f), "libsbt")
  case _       => println(s"Using libsbt $libsbtVersion") ; p settings (libsbtRef: _*)
}

lazy val root = add(project in file("."))
