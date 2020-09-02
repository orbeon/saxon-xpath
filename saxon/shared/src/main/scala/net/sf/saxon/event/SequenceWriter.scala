package net.sf.saxon.event

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.linked.LinkedTreeBuilder

import net.sf.saxon.tree.util.Orphan

import scala.beans.{BeanProperty, BooleanBeanProperty}

abstract class SequenceWriter(pipe: PipelineConfiguration)
  extends SequenceReceiver(pipe) {

  @BeanProperty
  var treeModel: TreeModel = null

  private var builder: Builder = null

  private var level: Int = 0

  def write(item: Item): Unit

  override def startDocument(properties: Int): Unit = {
    if (builder == null) {
      createTree(
        ReceiverOption.contains(properties, ReceiverOption.MUTABLE_TREE))
    }
    level+=1
    if (level == 0) {
      builder.startDocument(properties)
    }
  }

  override def setUnparsedEntity(name: String,
                                 systemID: String,
                                 publicID: String): Unit = {
    if (builder != null) {
      builder.setUnparsedEntity(name, systemID, publicID)
    }
  }

  private def createTree(mutable: Boolean): Unit = {
    val pipe: PipelineConfiguration = getPipelineConfiguration
    if (treeModel != null) {
      builder = treeModel.makeBuilder(pipe)
    } else if (pipe.getController != null) {
      if (mutable) {
        val model: TreeModel = pipe.getController.getModel
        builder =
          if (model.isMutable) pipe.getController.makeBuilder
          else new LinkedTreeBuilder(pipe)
      } else {
        builder = pipe.getController.makeBuilder
      }
    } else {
      val model: TreeModel = getConfiguration.getParseOptions.getModel
      builder = model.makeBuilder(pipe)
    }
    builder.setPipelineConfiguration(pipe)
    builder.setSystemId(systemId)
    builder.setBaseURI(systemId)
    builder.setTiming(false)
    builder.setUseEventLocation(false)
    builder.open()
  }

  override def endDocument(): Unit = {
    level-=1
    if (level == 0) {
      builder.endDocument()
      val doc: NodeInfo = builder.getCurrentRoot
      append(doc, Loc.NONE, ReceiverOption.ALL_NAMESPACES)
      builder = null
      systemId = null
    }
    previousAtomic = false
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if (builder == null) {
      createTree(
        ReceiverOption.contains(properties, ReceiverOption.MUTABLE_TREE))
    }
    builder.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
    level += 1
    previousAtomic = false
  }

  override def endElement(): Unit = {
    builder.endElement()
    level -= 1
    if (level==0) {
      builder.close()
      val element: NodeInfo = builder.getCurrentRoot
      append(element, Loc.NONE, ReceiverOption.ALL_NAMESPACES)
      builder = null
      systemId = null
    }
    previousAtomic = false
  }

  override def characters(s: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (level == 0) {
      val o: Orphan = new Orphan(getConfiguration)
      o.setNodeKind(Type.TEXT)
      o.setStringValue(s.toString)
      write(o)
    } else {
      if (s.length > 0) {
        builder.characters(s, locationId, properties)
      }
    }
    previousAtomic = false
  }

  override def comment(comment: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    if (level == 0) {
      val o: Orphan = new Orphan(getConfiguration)
      o.setNodeKind(Type.COMMENT)
      o.setStringValue(comment)
      write(o)
    } else {
      builder.comment(comment, locationId, properties)
    }
    previousAtomic = false
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (level == 0) {
      val o: Orphan = new Orphan(getConfiguration)
      o.setNodeName(new NoNamespaceName(target))
      o.setNodeKind(Type.PROCESSING_INSTRUCTION)
      o.setStringValue(data)
      write(o)
    } else {
      builder.processingInstruction(target, data, locationId, properties)
    }
    previousAtomic = false
  }

  override def close(): Unit = {
    previousAtomic = false
    if (builder != null) {
      builder.close()
    }
  }

  override def append(item: Item,
                      locationId: Location,
                      copyNamespaces: Int): Unit = {
    if (item != null) {
      if (level == 0) {
        write(item)
        previousAtomic = false
      } else {
        decompose(item, locationId, copyNamespaces)
      }
    }
  }

  override def usesTypeAnnotations: Boolean =
    builder == null || builder.usesTypeAnnotations

}
