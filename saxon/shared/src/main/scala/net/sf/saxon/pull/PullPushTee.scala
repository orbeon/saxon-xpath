package net.sf.saxon.pull

import net.sf.saxon.event.Receiver
import net.sf.saxon.event.ReceiverOption
import net.sf.saxon.event.SequenceReceiver
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.model.Type
import net.sf.saxon.om.NamespaceBinding
import net.sf.saxon.om.NamespaceMap
import net.sf.saxon.s9api.Location
import scala.jdk.CollectionConverters._
import net.sf.saxon.tree.util.Orphan
import java.util.List
import java.util.Stack

import net.sf.saxon.pull.PullProvider.Event
import net.sf.saxon.pull.PullProvider.Event._
import scala.util.control.Breaks._

class PullPushTee(base: PullProvider, private var branch: Receiver)
  extends PullFilter(base) {

  var previousAtomic: Boolean = false

  private var nsStack: Stack[NamespaceMap] = new Stack()

  def getReceiver(): Receiver = branch

  override def next(): Event = {
    currentEvent = super.next()
    copyEvent(currentEvent)
    currentEvent
  }

  private def copyEvent(event: Event): Unit = {
    val in: PullProvider = getUnderlyingProvider
    var loc: Location = in.getSourceLocator
    if (loc == null) {
      loc = Loc.NONE
    }
    val out: Receiver = branch
    event match {
      case Event.START_DOCUMENT => out.startDocument(ReceiverOption.NONE)
      case Event.START_ELEMENT =>
        var bindings: Array[NamespaceBinding] = in.getNamespaceDeclarations
        var nsMap: NamespaceMap =
          if (nsStack.isEmpty) NamespaceMap.emptyMap else nsStack.peek()
        breakable {
          for (binding <- bindings) {
            if (binding == null) {
              break
            }
            nsMap = nsMap.put(binding.getPrefix, binding.getURI)
          }
        }
        nsStack.push(nsMap)
        out.startElement(in.getNodeName,
          in.getSchemaType,
          in.getAttributes,
          nsMap,
          loc,
          ReceiverOption.NAMESPACE_OK)
      case TEXT =>
        out.characters(in.getStringValue, loc, ReceiverOption.WHOLE_TEXT_NODE)
      case COMMENT => out.comment(in.getStringValue, loc, ReceiverOption.NONE)
      case PROCESSING_INSTRUCTION =>
        out.processingInstruction(in.getNodeName.getLocalPart,
          in.getStringValue,
          loc,
          ReceiverOption.NONE)
      case END_ELEMENT =>
        out.endElement()
        nsStack.pop()
      case END_DOCUMENT =>
        var entities: List[UnparsedEntity] = in.getUnparsedEntities
        if (entities != null) {
          for (entity <- entities.asScala) {
            val ue: UnparsedEntity = entity.asInstanceOf[UnparsedEntity]
            out.setUnparsedEntity(ue.getName, ue.getSystemId, ue.getPublicId)
          }
        }
        out.endDocument()
      case END_OF_INPUT => in.close()
      case ATOMIC_VALUE =>
        if (out.isInstanceOf[SequenceReceiver]) {
          out.append(super.getAtomicValue, loc, ReceiverOption.NONE)
        } else {
          if (previousAtomic) {
            out.characters(" ", loc, ReceiverOption.NONE)
          }
          val chars: CharSequence = in.getStringValue
          out.characters(chars, loc, ReceiverOption.NONE)
        }
      case ATTRIBUTE =>
        if (out.isInstanceOf[SequenceReceiver]) {
          val o: Orphan = new Orphan(
            in.getPipelineConfiguration.getConfiguration)
          o.setNodeName(getNodeName)
          o.setNodeKind(Type.ATTRIBUTE)
          o.setStringValue(getStringValue)
          out.append(o, loc, ReceiverOption.NONE)
        }
      case NAMESPACE =>
        if (out.isInstanceOf[SequenceReceiver]) {
          val o: Orphan = new Orphan(
            in.getPipelineConfiguration.getConfiguration)
          o.setNodeName(getNodeName)
          o.setNodeKind(Type.NAMESPACE)
          o.setStringValue(getStringValue)
          out.append(o, loc, ReceiverOption.NONE)
        }
      case _ => throw new UnsupportedOperationException("" + event)

    }
    previousAtomic = event == Event.ATOMIC_VALUE
  }

}
