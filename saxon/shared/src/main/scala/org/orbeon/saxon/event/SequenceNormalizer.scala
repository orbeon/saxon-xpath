package org.orbeon.saxon.event

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Action

import org.orbeon.saxon.s9api.Destination

import org.orbeon.saxon.s9api.Location

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

import org.orbeon.saxon.trans.XPathException

import java.util.ArrayList

import java.util.List

abstract class SequenceNormalizer(next: Receiver) extends ProxyReceiver(next) {

  var level: Int = 0

  private var actionList: List[Action] = _

  private var failed: Boolean = false

  override def open(): Unit = {
    level = 0
    previousAtomic = false
    super.open()
    getNextReceiver.startDocument(ReceiverOption.NONE)
  }

  override def startDocument(properties: Int): Unit = {
    level += 1
    previousAtomic = false
  }

  override def endDocument(): Unit = {
    level -= 1
    previousAtomic = false
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    try {
      level += 1
      super.startElement(elemName,
        `type`,
        attributes,
        namespaces,
        location,
        properties)
      previousAtomic = false
    } catch {
      case e: XPathException => {
        failed = true
        throw e
      }

    }
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    try {
      super.characters(chars, locationId, properties)
      previousAtomic = false
    } catch {
      case e: XPathException => {
        failed = true
        throw e
      }

    }
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    try {
      super.processingInstruction(target, data, locationId, properties)
      previousAtomic = false
    } catch {
      case e: XPathException => {
        failed = true
        throw e
      }

    }
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    try {
      super.comment(chars, locationId, properties)
      previousAtomic = false
    } catch {
      case e: XPathException => {
        failed = true
        throw e
      }

    }
  }

  override def endElement(): Unit = {
    try {
      level -= 1
      super.endElement()
      previousAtomic = false
    } catch {
      case e: XPathException => {
        failed = true
        throw e
      }

    }
  }

  override def close(): Unit = {
    if (failed) {
      super.close()
    } else {
      getNextReceiver.endDocument()
      super.close()
      if (actionList != null) {
        for (action <- actionList.asScala) {
          action.act()
        }
      }
    }
  }

  def onClose(actionList: List[Action]): Unit = {
    this.actionList = actionList
  }

  def onClose(action: Action): Unit = {
    if (actionList == null) {
      actionList = new ArrayList()
    }
    actionList.add(action)
  }

}
