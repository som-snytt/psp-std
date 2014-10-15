package psp
package std

class SysPropFilter(prefix: String) extends Dynamic {
  def isRoot  = prefix == ""
  def value   = sys props prefix
  def exists  = !isRoot && (sys.props contains prefix)
  def matches = sys.props.keys.toVector filter (k => isRoot || (k startsWith prefix + "."))

  private def append(name: String): String = if (prefix == "") name else prefix + "." + name

  def | (alt: => String): String = if (exists) value else alt
  def get: String                = this | abort(s"No such key: $prefix")
  def getPath: Path              = path(get)

  def selectDynamic(name: String): SysPropFilter = new SysPropFilter(this append name)

  override def toString = if (exists) s"$prefix: $value" else s"$matches"
}

object sysprop extends SysPropFilter("") {
  object java extends SysPropFilter("java")
  object user extends SysPropFilter("user")
  object ivy extends SysPropFilter("ivy")

  private implicit def lowerToPath(s: SysPropFilter): Path     = s.getPath
  private implicit def lowerToString(s: SysPropFilter): String = s.get

  def ivyHome: Path         = ivy.default.ivy.user.dir
  def javaHome: Path        = java.home
  def userHome: Path        = user.home
  def javaVersion: String   = java.specification.version
  def javaClassPath: String = java.`class`.path

  // [awt.toolkit, file.encoding, file.encoding.pkg, file.separator, ftp.nonProxyHosts, gopherProxySet, http.nonProxyHosts, java.awt.graphicsenv, java.awt.printerjob, java.class.path, java.class.version, java.endorsed.dirs, java.ext.dirs, java.home, java.io.tmpdir, java.library.path, java.runtime.name, java.runtime.version, java.specification.name, java.specification.vendor, java.specification.version, java.vendor, java.vendor.url, java.vendor.url.bug, java.version, java.vm.info, java.vm.name, java.vm.specification.name, java.vm.specification.vendor, java.vm.specification.version, java.vm.vendor, java.vm.version, jline.esc.timeout, jline.shutdownhook, line.separator, os.arch, os.name, os.version, path.separator, socksNonProxyHosts, sun.arch.data.model, sun.boot.class.path, sun.boot.library.path, sun.cpu.endian, sun.cpu.isalist, sun.io.unicode.encoding, sun.java.command, sun.java.launcher, sun.jnu.encoding, sun.management.compiler, sun.nio.ch.bugLevel, sun.os.patch.level, user.country, user.dir, user.home, user.language, user.name, user.timezone]
}

trait StdProperties {
  def userHome    = sysprop.userHome
  def javaHome    = sysprop.javaHome
  def javaVersion = sysprop.javaVersion
  def jdkSource   = javaHome.getParent resolve "src.zip"
  def ivyHome     = Try(sysprop.ivyHome) | (userHome resolve ".ivy2")
  def m2Home      = path( (sys.env get "M2_HOME") | (userHome resolve ".m2").toString )
}
