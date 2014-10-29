package psp
package std
package pio

import api._
import java.util.jar.Attributes.Name
import scala.util.Properties._
import Unsafe.universalEq

object ManifestMap {
  def initialMainAttrs: View[(Name, String)] = view(
    Name.MANIFEST_VERSION -> "1.0",
    ScalaCompilerVersion  -> versionNumberString
  )
  def apply(mainAttrs: (Name, String)*): ManifestMap = apply(new jManifest) ++= mainAttrs.toSeq
  def apply(manifest: jManifest): ManifestMap        = new ManifestMap(manifest) ++= initialMainAttrs
}

class ManifestMap private (val manifest: jManifest) extends AndThis {
  def +=(kv: (Name, String))         = andThis(javaAttrs.put(kv._1, kv._2))
  def ++=(kvs: Each[(Name, String)]) = andThis(kvs foreach +=)

  def underlying                              = manifest
  def javaAttrs: jMap[jAttributeName, String] = manifest.getMainAttributes().castTo
  def attrs: exMap[Name, String]              = javaAttrs.keySet mapOnto javaAttrs.get
  def apply(name: Name)                       = if (manifest eq null) null else attrs(name)
}
