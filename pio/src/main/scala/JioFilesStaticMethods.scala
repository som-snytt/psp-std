package psp
package std
package pio

import java.{ util => ju }
import java.nio.{ file => jnf }
import jnf.{ Files => F }

/** Wrong type of first argument:
 *    def copy(in: InputStream, target: Path, options: CopyOption*): Long
 *    def createTempDirectory(prefix: String, attrs: AnyFileAttr*): Path
 *    def createTempFile(prefix: String, suffix: String, attrs: AnyFileAttr*): Path
 */
final class JnfFilesStatics(val path: Path) extends AnyVal {
  def copy(out: OutputStream): Long                                                                        = F.copy(path, out)
  def copy(target: Path, options: CopyOption*): Path                                                       = F.copy(path, target, options: _*)
  def createDirectories(attrs: AnyFileAttr*): Path                                                         = F.createDirectories(path, attrs: _*)
  def createDirectory(attrs: AnyFileAttr*): Path                                                           = F.createDirectory(path, attrs: _*)
  def createFile(attrs: AnyFileAttr*): Path                                                                = F.createFile(path, attrs: _*)
  def createLink(existing: Path): Path                                                                     = F.createLink(path, existing)
  def createSymbolicLink(target: Path, attrs: AnyFileAttr*): Path                                          = F.createSymbolicLink(path, target, attrs: _*)
  def createTempDirectory(prefix: String, attrs: AnyFileAttr*): Path                                       = F.createTempDirectory(path, prefix, attrs: _*)
  def createTempFile(prefix: String, suffix: String, attrs: AnyFileAttr*): Path                            = F.createTempFile(path, prefix, suffix, attrs: _*)
  def delete(): Unit                                                                                       = F.delete(path)
  def deleteIfExists(): Boolean                                                                            = F.deleteIfExists(path)
  def exists(options: LinkOption*): Boolean                                                                = F.exists(path, options: _*)
  def getAttribute(attribute: String, options: LinkOption*): Object                                        = F.getAttribute(path, attribute, options: _*)
  def getFileAttributeView[V <: FileAttributeView](`type`: Class[V], options: LinkOption*): V              = F.getFileAttributeView(path, `type`, options: _*)
  def getFileStore(): FileStore                                                                            = F.getFileStore(path)
  def getLastModifiedTime(options: LinkOption*): FileTime                                                  = F.getLastModifiedTime(path, options: _*)
  def getOwner(options: LinkOption*): UserPrincipal                                                        = F.getOwner(path, options: _*)
  def getPosixFilePermissions(options: LinkOption*): jFilePermissions                                      = F.getPosixFilePermissions(path, options: _*)
  def isDirectory(options: LinkOption*): Boolean                                                           = F.isDirectory(path, options: _*)
  def isExecutable(): Boolean                                                                              = F.isExecutable(path)
  def isHidden(): Boolean                                                                                  = F.isHidden(path)
  def isReadable(): Boolean                                                                                = F.isReadable(path)
  def isRegularFile(options: LinkOption*): Boolean                                                         = F.isRegularFile(path)
  def isSameFile(path2: Path): Boolean                                                                     = F.isSameFile(path, path2)
  def isSymbolicLink(): Boolean                                                                            = F.isSymbolicLink(path)
  def isWritable(): Boolean                                                                                = F.isWritable(path)
  def move(target: Path, options: CopyOption*): Path                                                       = F.move(path, target, options: _*)
  def newBufferedReader(cs: Charset): BufferedReader                                                       = F.newBufferedReader(path, cs)
  def newBufferedWriter(cs: Charset, options: OpenOption*): BufferedWriter                                 = F.newBufferedWriter(path, cs, options: _*)
  def newByteChannel(options: OpenOption*): SeekableByteChannel                                            = F.newByteChannel(path, options: _*)
  def newByteChannel(options: ju.Set[_ <: OpenOption], attrs: AnyFileAttr*): SeekableByteChannel           = F.newByteChannel(path, options, attrs: _*)
  def newDirectoryStream(): PathDirStream                                                                  = F.newDirectoryStream(path)
  def newDirectoryStream(glob: String): PathDirStream                                                      = F.newDirectoryStream(path, glob)
  def newDirectoryStream(filter: DirectoryStreamFilter[_ >: Path]): PathDirStream                          = F.newDirectoryStream(path, filter)
  def newInputStream(options: OpenOption*): InputStream                                                    = F.newInputStream(path, options: _*)
  def newOutputStream(options: OpenOption*): OutputStream                                                  = F.newOutputStream(path, options: _*)
  def notExists(options: LinkOption*): Boolean                                                             = F.notExists(path, options: _*)
  def probeContentType(): String                                                                           = F.probeContentType(path)
  def readAllBytes(): Array[Byte]                                                                          = F.readAllBytes(path)
  def readAllLines(cs: Charset): ju.List[String]                                                           = F.readAllLines(path, cs)
  def readAttributes(attributes: String, options: LinkOption*): jMap[String, Object]                       = F.readAttributes(path, attributes, options: _*)
  def readAttributes[A <: BasicFileAttributes](`type`: Class[A], options: LinkOption*): A                  = F.readAttributes(path, `type`, options: _*)
  def readSymbolicLink(): Path                                                                             = F.readSymbolicLink(path)
  def setAttribute(attribute: String, value: Any, options: LinkOption*): Path                              = F.setAttribute(path, attribute, value, options: _*)
  def setLastModifiedTime(time: FileTime): Path                                                            = F.setLastModifiedTime(path, time)
  def setOwner(owner: UserPrincipal): Path                                                                 = F.setOwner(path, owner)
  def setPosixFilePermissions(perms: jFilePermissions): Path                                               = F.setPosixFilePermissions(path, perms)
  def size(): Long                                                                                         = F.size(path)
  def walkFileTree(visitor: FileVisitor[_ >: Path]): Path                                                  = F.walkFileTree(path, visitor)
  def walkFileTree(options: ju.Set[FileVisitOption], maxDepth: Int, visitor: FileVisitor[_ >: Path]): Path = F.walkFileTree(path, options, maxDepth, visitor)
  def write(bytes: Array[Byte], options: OpenOption*): Path                                                = F.write(path, bytes, options: _*)
  def write(lines: jLineIterable, cs: Charset, options: OpenOption*): Path                                 = F.write(path, lines, cs, options: _*)
}
