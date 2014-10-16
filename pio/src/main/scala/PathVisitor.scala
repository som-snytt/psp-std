package psp
package std
package pio

import java.nio.file.{ FileVisitResult, SimpleFileVisitor }

trait PathVisitor extends FileVisitor[Path] {
  def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult
  def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult
  def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult
  def visitFileFailed(file: Path, exc: IOException): FileVisitResult
}

object PathVisitor {
  class Simple extends SimpleFileVisitor[Path] with PathVisitor { }

  val Continue     = FileVisitResult.CONTINUE
  val SkipSiblings = FileVisitResult.SKIP_SIBLINGS
  val SkipSubtree  = FileVisitResult.SKIP_SUBTREE
  val Terminate    = FileVisitResult.TERMINATE

  def apply(f: (Path, BasicFileAttributes) => FileVisitResult): PathVisitor = new Simple {
    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = f(file, attrs)
  }
  def apply(f: Path => Unit): PathVisitor = new Simple {
    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = Continue sideEffect f(file)
  }
}
