package org.orbeon.saxon.pull

import org.orbeon.saxon.event.PipelineConfiguration
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om.AttributeMap
import org.orbeon.saxon.om.NamespaceBinding
import org.orbeon.saxon.om.NodeName
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.AtomicValue
import java.util.List

import org.orbeon.saxon.pull.PullProvider.Event.Event


object PullProvider {

  object Event extends Enumeration {

    val START_OF_INPUT: Event = new Event()

    val ATOMIC_VALUE: Event = new Event()

    val START_DOCUMENT: Event = new Event()

    val END_DOCUMENT: Event = new Event()

    val START_ELEMENT: Event = new Event()

    val END_ELEMENT: Event = new Event()

    val ATTRIBUTE: Event = new Event()

    val NAMESPACE: Event = new Event()

    val TEXT: Event = new Event()

    val COMMENT: Event = new Event()

    val PROCESSING_INSTRUCTION: Event = new Event()

    val END_OF_INPUT: Event = new Event()

    class Event extends Val

    implicit def convertValue(v: Value): Event = v.asInstanceOf[Event]

  }

}

trait PullProvider {

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit

  def getPipelineConfiguration: PipelineConfiguration

  def next(): Event

  def current: Event

  def getAttributes: AttributeMap

  def getNamespaceDeclarations: Array[NamespaceBinding]

  def skipToMatchingEnd(): Event

  def close(): Unit

  def getNodeName: NodeName

  def getStringValue: CharSequence

  def getSchemaType: SchemaType

  def getAtomicValue: AtomicValue

  def getSourceLocator: Location

  def getUnparsedEntities: List[UnparsedEntity]

}
