package net.sf.saxon.pull

import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om.AttributeMap
import net.sf.saxon.om.NamespaceBinding
import net.sf.saxon.om.NodeName
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.AtomicValue
import java.util.List

import net.sf.saxon.pull.PullProvider.Event.Event


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

  def getPipelineConfiguration(): PipelineConfiguration

  def next(): Event

  def current(): Event

  def getAttributes(): AttributeMap

  def getNamespaceDeclarations(): Array[NamespaceBinding]

  def skipToMatchingEnd(): Event

  def close(): Unit

  def getNodeName(): NodeName

  def getStringValue(): CharSequence

  def getSchemaType(): SchemaType

  def getAtomicValue(): AtomicValue

  def getSourceLocator(): Location

  def getUnparsedEntities(): List[UnparsedEntity]

}
