package org.orbeon.saxon.event

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.AbsolutePath

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.FastStringBuffer

import java.util.HashMap

import java.util.Stack

//import scala.collection.compat._
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

  def getAbsolutePath: AbsolutePath = new AbsolutePath(path)

}
