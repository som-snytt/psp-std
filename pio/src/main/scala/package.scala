package psp
package std

import api._
import java.nio.{ file => jnf }
import java.util.jar.Attributes.Name
import StdEq._, StdShow._
import scala.sys.process.{ Process, ProcessBuilder }

package object pio {
  type jDirStreamFilter[A] = DirectoryStreamFilter[A]
  type jDirStream[A]       = DirectoryStream[A]

  implicit def opsFilesStatics(path: Path): JnfFilesStatics                        = new JnfFilesStatics(path)
  implicit def opsJarEntry(entry: JarEntry): JioJarEntryOps                        = new JioJarEntryOps(entry)
  implicit def opsManifestMap(m: jManifest): ManifestMap                           = ManifestMap(m)
  implicit def opsSeqString(xs: scSeq[String]): JioSeqStringOps                    = new JioSeqStringOps(xs)
  implicit def opsUri(uri: jUri): JioUriOps                                        = new JioUriOps(uri)
  implicit def opsFile(file: jFile): PioPathOps[Path]                              = opsPath(file.toURI.fs)
  implicit def opsPath[A <: Path](path: A): PioPathOps[A]                          = new PioPathOps[A](path)
  implicit def predicateToDirectoryFilter[A](p: Predicate[A]): jDirStreamFilter[A] = new jDirStreamFilter[A] { def accept(entry: A) = p(entry) }

  def ivyJars         = ivyHome.deepJars
  def mavenJars       = m2Home.deepJars
  def knownJars       = ivyJars ++ mavenJars
  def knownPioJars    = knownJars map (_.toPioJar)
  def knownClassNames = knownPioJars flatMap (_.classNames)

  // Operations involving external processes.
  def newProcess(line: String): ProcessBuilder        = Process(line)
  def newProcess(args: scSeq[String]): ProcessBuilder = Process(args)
  def executeLine(line: String): Int                  = Process(line).!
  def execute(args: String*): Int                     = Process(args.toSeq).!
  def openSafari(path: Path): Unit                    = open.Safari(path)
  def openChrome(path: Path): Unit                    = open.`Google Chrome`(path)

  // Filesystem.
  def newTempDir(prefix: String, attrs: AnyFileAttr*): Path                  = jnf.Files.createTempDirectory(prefix, attrs: _*)
  def newTempFile(prefix: String, suffix: String, attrs: AnyFileAttr*): Path = jnf.Files.createTempFile(prefix, suffix, attrs: _*)
  def installedProviders: View[FileSystemProvider]                           = java.nio.file.spi.FileSystemProvider.installedProviders.m

  object open extends Dynamic {
    def applyDynamic(name: String)(args: TryShown*): String = Process(sciList("open", "-a", name) ++ args.map(_.try_s)).!!
  }

  implicit class JarOps(val jar: Jar) {
    def manifestMainClass = noNull(jar.manifestMap(Name.MAIN_CLASS), "")
    def manifestClassPath = noNull(jar.manifestMap(Name.CLASS_PATH), "")

    def mapNames[A](f: String => A): View[A]                   = fview(g => jar  foreachEntry(e => g(f(e.getName))))
    def mapEntries[A](f: JarEntry => A): View[A]               = fview(g => jar  foreachEntry(f map g))
    def mapBytes[A](f: (JarEntry, Bytes) => A): View[A]        = fview(g => jar  foreachBytes(f map g))
    def mapStream[A](f: (JarEntry, InputStream) => A): View[A] = fview(g => jar foreachStream(f map g))

    def newLoader: PolicyLoader             = new PolicyLoader(newClassMap)
    def paths: View[Path]                   = mapEntries(e => path(e.getName))
    def pathsOfClasses: View[Path]          = paths filter (_.isClassFile)
    def classNames: View[String]            = classStream map (_.className)
    def classInstances: View[jClass]        = newLoader.classes filterNot (_ eq null)
    def classStream: View[JarEntryAndBytes] = mapBytes(JarEntryAndBytes) filter (_.isClass)
    def newClassMap: exMap[String, Bytes]   = classStream map (j => j.className -> j.bytes) pmap
  }
}
