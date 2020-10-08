package javax.xml.transform.dom

import javax.xml.transform.Source
import org.w3c.dom.Node


object DOMSource {
  val FEATURE = "http://javax.xml.transform.dom.DOMSource/feature"
}

class DOMSource extends Source {

  private var node: Node = null
  private var systemID: String = null

  def this(n: Node) = {
    this()
    setNode(n)
  }

  def this(node: Node, systemID: String) = {
    this()
    setNode(node)
    setSystemId(systemID)
  }

  def setNode(node: Node): Unit =
    this.node = node

  def getNode: Node = node

  override def setSystemId(systemID: String): Unit =
    this.systemID = systemID

  override def getSystemId: String = this.systemID
}
