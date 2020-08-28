package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import net.sf.saxon.om.AbsolutePath

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import java.util.HashMap

import java.util.Stack

import scala.jdk.CollectionConverters._

class PathMaintainer(next: Receiver) extends ProxyReceiver(next) {

  private var path: Stack[AbsolutePath.PathElement] = new Stack()

  private var siblingCounters: Stack[HashMap[NodeName, Integer]] = new Stack()

  siblingCounters.push(new HashMap())

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
    val counters: HashMap[NodeName, Integer] = siblingCounters.peek()
    var index: Int = 1
    val preceding: java.lang.Integer = counters.get(elemName)
    if (preceding != null) {
      index = preceding + 1
      counters.put(elemName, index)
    } else {
      counters.put(elemName, 1)
    }
    path.push(new AbsolutePath.PathElement(Type.ELEMENT, elemName, index))
    siblingCounters.push(new HashMap())
  }

  override def endElement(): Unit = {
    nextReceiver.endElement()
    siblingCounters.pop()
    path.pop()
  }

  def getPath(useURIs: Boolean): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
    for (pe <- path.asScala) {
      fsb.cat('/')
      if (useURIs) {
        val uri: String = pe.name.getURI
        if (!uri.isEmpty) {
          fsb.cat('"')
          fsb.append(uri)
          fsb.cat('"')
        }
      } else {
        val prefix: String = pe.name.getPrefix
        if (!prefix.isEmpty) {
          fsb.append(prefix)
          fsb.cat(':')
        }
      }
      fsb.append(pe.name.getLocalPart)
      fsb.cat('[')
      fsb.append(pe.index.toString)
      fsb.cat(']')
    }
    fsb.toString
  }

  def getAbsolutePath(): AbsolutePath = new AbsolutePath(path)

}
