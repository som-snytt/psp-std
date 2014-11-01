package psp
package std

import api._

trait AndThis {
  def andThis(x: Unit): this.type = this
}

trait ClassLoaderTrait {
  def loader: jClassLoader

  def parentChain: Each[jClassLoader] = {
    def loop(cl: jClassLoader): View[jClassLoader] = cl match {
      case null => exView()
      case _    => cl +: loop(cl.getParent)
    }
    loop(loader)
  }
  def uris: Each[jUri] = loader match {
    case cl: URLClassLoader => cl.getURLs.pseq map (_.toURI)
    case _                  => Nil
  }
}
