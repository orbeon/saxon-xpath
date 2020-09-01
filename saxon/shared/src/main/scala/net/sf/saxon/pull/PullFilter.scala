package net.sf.saxon.pull

import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om.AttributeMap
import net.sf.saxon.om.NamePool
import net.sf.saxon.om.NamespaceBinding
import net.sf.saxon.om.NodeName
import net.sf.saxon.s9api.Location
import net.sf.saxon.value.AtomicValue
import java.util.List

import net.sf.saxon.pull.PullProvider.Event.Event

class PullFilter(private var base: PullProvider) extends PullProvider {

  private var pipe: PipelineConfiguration = _

   var currentEvent: Event = base.current()

  if (base.getPipelineConfiguration != null) {
    this.setPipelineConfiguration(base.getPipelineConfiguration)
  }

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
    base.setPipelineConfiguration(pipe)
  }

  def getPipelineConfiguration(): PipelineConfiguration = pipe

  def getNamePool: NamePool =
    getPipelineConfiguration.getConfiguration.getNamePool

  def getUnderlyingProvider: PullProvider = base

  def next(): Event = base.next()

  def current(): Event = currentEvent

  def getAttributes(): AttributeMap = base.getAttributes

  def getNamespaceDeclarations(): Array[NamespaceBinding] =
    base.getNamespaceDeclarations

  def skipToMatchingEnd(): Event = base.skipToMatchingEnd()

  def close(): Unit = {
    base.close()
  }

  def getNodeName(): NodeName = base.getNodeName

  def getStringValue: CharSequence = base.getStringValue

  def getAtomicValue(): AtomicValue = base.getAtomicValue

  def getSchemaType(): SchemaType = base.getSchemaType

  def getSourceLocator(): Location = base.getSourceLocator

  def getUnparsedEntities(): List[UnparsedEntity] = base.getUnparsedEntities

}
