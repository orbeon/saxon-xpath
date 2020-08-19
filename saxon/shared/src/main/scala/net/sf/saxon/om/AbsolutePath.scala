package net.sf.saxon.om

import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model.Type
import net.sf.saxon.trans.Err
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.tree.util.Navigator
import java.util
import net.sf.saxon.om
import scala.jdk.CollectionConverters._

object AbsolutePath {

  def pathToNode(node: NodeInfo): AbsolutePath = {
    var nodeInfo = node
    val list = new util.LinkedList[AbsolutePath.PathElement]
    while ( {
      nodeInfo != null && nodeInfo.getNodeKind != Type.DOCUMENT
    }) {
      val pe = new AbsolutePath.PathElement(nodeInfo.getNodeKind, NameOfNode.makeName(nodeInfo), Navigator.getNumberSimple(nodeInfo, null))
      list.addFirst(pe)
      nodeInfo = nodeInfo.getParent
    }
    new AbsolutePath(list)
  }


  class PathElement(var nodeKind: Int, var name: NodeName, var index: Int) {
    def getIndex: Int = index

    def toString(fsb: FastStringBuffer, option: Char): Unit = nodeKind match {
      case Type.DOCUMENT =>
        fsb.append("(/)")
      case Type.ATTRIBUTE =>
        fsb.cat('@')
        if (!name.getURI.isEmpty) if (option == 'u') {
          fsb.append("Q{")
          fsb.append(name.getURI)
          fsb.append("}")
        }
        else if (option == 'p') {
          val prefix = name.getPrefix
          if (!prefix.isEmpty) {
            fsb.append(prefix)
            fsb.cat(':')
          }
        }
        else if (option == 's') {
          fsb.append("Q{")
          fsb.append(Err.abbreviateURI(name.getURI))
          fsb.append("}")
        }
        fsb.append(name.getLocalPart)
      case Type.ELEMENT =>
        if (option == 'u') {
          fsb.append("Q{")
          fsb.append(name.getURI)
          fsb.append("}")
        }
        else if (option == 'p') {
          val prefix = name.getPrefix
          if (!prefix.isEmpty) {
            fsb.append(prefix)
            fsb.cat(':')
          }
        }
        else if (option == 's') if (!name.getURI.isEmpty) {
          fsb.append("Q{")
          fsb.append(Err.abbreviateURI(name.getURI))
          fsb.append("}")
        }
        fsb.append(name.getLocalPart)
        appendPredicate(fsb)
      case Type.TEXT =>
        fsb.append("text()")
      case Type.COMMENT =>
        fsb.append("comment()")
        appendPredicate(fsb)
      case Type.PROCESSING_INSTRUCTION =>
        fsb.append("processing-instruction(")
        fsb.append(name.getLocalPart)
        fsb.append(")")
        appendPredicate(fsb)
      case Type.NAMESPACE =>
        fsb.append("namespace::")
        if (name.getLocalPart.isEmpty) fsb.append("*[Q{" + NamespaceConstant.FN + "}local-name()=\"\"]")
        else fsb.append(name.getLocalPart)
      case _ =>
    }

    def appendPredicate(fsb: FastStringBuffer): Unit = {
      val index = getIndex
      if (index != -1) {
        fsb.cat('[')
        fsb.append(s"$getIndex")
        fsb.cat(']')
      }
    }
  }

}


class AbsolutePath(path: util.List[AbsolutePath.PathElement]) {
  private var systemId: String = null


  def appendAttributeName(attributeName: NodeName): Unit = {
    if (!path.isEmpty) {
      val last = path.get(path.size - 1)
      if (last.nodeKind == Type.ATTRIBUTE) path.remove(path.size - 1)
    }
    val att = new AbsolutePath.PathElement(Type.ATTRIBUTE, attributeName, 1)
    path.add(att)
  }


  def getPathUsingPrefixes = {
    val fsb = new FastStringBuffer(FastStringBuffer.C256)

    for (pe <- path.asScala) {
      fsb.cat('/')
      pe.toString(fsb, 'p')
    }
    fsb.toString
  }


  def getPathUsingUris: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C256)

    for (pe <- path.asScala) {
      fsb.cat('/')
      pe.asInstanceOf[AbsolutePath.PathElement].toString(fsb, 'u')
    }
    fsb.toString
  }


  def getPathUsingAbbreviatedUris: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C256)

    for (pe <- path.asScala) {
      fsb.cat('/')
      pe.asInstanceOf[AbsolutePath.PathElement].toString(fsb, 's')
    }
    fsb.toString
  }

  override def toString = getPathUsingUris

  override def equals(obj: Any): Boolean = obj.isInstanceOf[AbsolutePath] && obj.toString == toString

  override def hashCode: Int = toString.hashCode

  def setSystemId(systemId: String): Unit = this.systemId = systemId

  def getSystemId: String = systemId
}