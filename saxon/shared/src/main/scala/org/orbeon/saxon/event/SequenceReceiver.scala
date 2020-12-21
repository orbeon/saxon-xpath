package org.orbeon.saxon.event

import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.ma.arrays.ArrayItem
import org.orbeon.saxon.ma.map.MapItem
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.Orphan
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.{AtomicValue, ExternalObject}

abstract class SequenceReceiver(pipe: PipelineConfiguration) extends Receiver {

   var previousAtomic: Boolean = false
   var pipelineConfiguration: PipelineConfiguration = pipe
   var systemId: String = null

  def getPipelineConfiguration: PipelineConfiguration = pipelineConfiguration

  def setPipelineConfiguration(pipelineConfiguration: PipelineConfiguration): Unit =
    this.pipelineConfiguration = pipelineConfiguration

  def getConfiguration: Configuration =
    pipelineConfiguration.getConfiguration

  def setSystemId(systemId: String): Unit =
    this.systemId = systemId

  def getSystemId: String = systemId

  def setUnparsedEntity(name: String,
                        systemID: String,
                        publicID: String): Unit = ()

  def open(): Unit =
    previousAtomic = false

  def append(item: Item, locationId: Location, properties: Int): Unit

  override def append(item: Item): Unit =
    append(item, Loc.NONE, ReceiverOption.ALL_NAMESPACES)

  def getNamePool: NamePool =
    pipelineConfiguration.getConfiguration.getNamePool

   def flatten(array: ArrayItem,
                        locationId: Location,
                        copyNamespaces: Int): Unit = {
    for (member <- array.members) {
      member
        .iterate()
        .forEachOrFail(it => append(it, locationId, copyNamespaces))
    }
  }

   def decompose(item: Item,
                          locationId: Location,
                          copyNamespaces: Int): Unit = {
    if (item != null) {
      if (item.isInstanceOf[AtomicValue] || item
        .isInstanceOf[ExternalObject[_]]) {
        if (previousAtomic)
          characters(" ", locationId, ReceiverOption.NONE)
        characters(item.getStringValueCS, locationId, ReceiverOption.NONE)
        previousAtomic = true
      } else item match {
        case item1: ArrayItem =>
          flatten(item1, locationId, copyNamespaces)
        case _: Function =>
          val thing: String =
            if (item.isInstanceOf[MapItem]) "map" else "function item"
          val errorCode: String = getErrorCodeForDecomposingFunctionItems
          if (errorCode.startsWith("SENR")) {
            throw new XPathException(
              "Cannot serialize a " + thing + " using this output method",
              errorCode,
              locationId)
          } else {
            throw new XPathException(
              "Cannot add a " + thing + " to an XDM node tree",
              errorCode,
              locationId)
          }
        case _ =>
          val node: NodeInfo = item.asInstanceOf[NodeInfo]
          val kind: Int = node.getNodeKind
          node match {
            case orphan: Orphan if orphan.isDisableOutputEscaping =>
              characters(item.getStringValueCS,
                locationId,
                ReceiverOption.DISABLE_ESCAPING)
              previousAtomic = false
            case _ => if (kind == Type.DOCUMENT) {
              startDocument(ReceiverOption.NONE)
              for (child <- node.children)
                append(child, locationId, copyNamespaces)
              previousAtomic = false
              endDocument()
            } else if (kind == Type.ATTRIBUTE || kind == Type.NAMESPACE) {
              val thing: String =
                if (kind == Type.ATTRIBUTE) "an attribute" else "a namespace"
              throw new XPathException(
                "Sequence normalization: Cannot process " + thing + " node",
                "SENR0001",
                locationId)
            } else {
              var copyOptions: Int = CopyOptions.TYPE_ANNOTATIONS
              if (ReceiverOption.contains(copyNamespaces,
                ReceiverOption.ALL_NAMESPACES)) {
                copyOptions |= CopyOptions.ALL_NAMESPACES
              }
              item.asInstanceOf[NodeInfo].copy(this, copyOptions, locationId)
              previousAtomic = false
            }
          }
      }
    }
  }

   def getErrorCodeForDecomposingFunctionItems: String =
    if (getPipelineConfiguration.isXSLT) "XTDE0450" else "XQTY0105"

  override def handlesAppend: Boolean = true
}
