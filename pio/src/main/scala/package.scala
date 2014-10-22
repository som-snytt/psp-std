package psp
package std

import api._
import java.util.jar.Attributes.Name
import StdEq._

package object pio {
  type jDirStreamFilter[A] = DirectoryStreamFilter[A]
  type jDirStream[A]       = DirectoryStream[A]

  implicit def opsFilesStatics(path: Path): JnfFilesStatics                        = new JnfFilesStatics(path)
  implicit def opsJarEntry(entry: JarEntry): JioJarEntryOps                        = new JioJarEntryOps(entry)
  implicit def opsManifestMap(m: jManifest): ManifestMap                           = ManifestMap(m)
  implicit def opsSeqString(xs: sSeq[String]): JioSeqStringOps                     = new JioSeqStringOps(xs)
  implicit def opsUri(uri: jUri): JioUriOps                                        = new JioUriOps(uri)
  implicit def opsFile(file: jFile): JioPathOps[Path]                              = opsPath(file.toURI.fs)
  implicit def opsPath[A <: Path](path: A): JioPathOps[A]                          = new JioPathOps[A](path)
  implicit def predicateToDirectoryFilter[A](p: Predicate[A]): jDirStreamFilter[A] = new jDirStreamFilter[A] { def accept(entry: A) = p(entry) }

  def ivyJars      = ivyHome.deepJars
  def mavenJars    = m2Home.deepJars
  def knownJars    = ivyJars ++ mavenJars
  def knownPioJars = knownJars map (_.toPioJar)
  def knownNames   = mapToList[String, Jar]() doto (b => knownPioJars foreach (jar => jar.classNames foreach (name => b(name) ::= jar))) toMap

  implicit class JarOps(val jar: Jar) {
    def manifestMainClass = noNull(jar.manifestMap(Name.MAIN_CLASS), "")
    def manifestClassPath = noNull(jar.manifestMap(Name.CLASS_PATH), "")

    def mapNames[A, That](f: String => A)(implicit z: Builds[A, That]): That                   = z direct (g => jar  foreachEntry(e => g(f(e.getName))))
    def mapEntries[A, That](f: JarEntry => A)(implicit z: Builds[A, That]): That               = z direct (g => jar  foreachEntry(f map g))
    def mapBytes[A, That](f: (JarEntry, Bytes) => A)(implicit z: Builds[A, That]): That        = z direct (g => jar  foreachBytes(f map g))
    def mapStream[A, That](f: (JarEntry, InputStream) => A)(implicit z: Builds[A, That]): That = z direct (g => jar foreachStream(f map g))

    def newLoader: PolicyLoader                = new PolicyLoader(newClassMap)
    def paths: pVector[Path]                   = mapEntries(e => path(e.getName))
    def pathsOfClasses: pVector[Path]          = paths filter (_.isClassFile)
    def classNames: pVector[String]            = classStream map (_.className)
    def classInstances: pVector[jClass]        = newLoader.classes filterNot (_ eq null)
    def classStream: pVector[JarEntryAndBytes] = mapBytes(JarEntryAndBytes).pvec filter (_.isClass)
    def newClassMap: pMap[String, Bytes]       = classStream map (j => j.className -> j.bytes) pmap
  }
}
