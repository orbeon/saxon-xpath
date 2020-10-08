package org.orbeon.saxon.pull

import java.util.List

import org.orbeon.saxon.event.PipelineConfiguration
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om.{AttributeMap, NamePool, NamespaceBinding, NodeName}
import org.orbeon.saxon.pull.PullProvider.Event.Event
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.value.AtomicValue

class PullFilter(private var base: PullProvider) extends PullProvider {

  private var pipe: PipelineConfiguration = _

   var currentEvent: Event = base.current

  if (base.getPipelineConfiguration != null)
    this.setPipelineConfiguration(base.getPipelineConfiguration)

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
    base.setPipelineConfiguration(pipe)
  }

  def getPipelineConfiguration: PipelineConfiguration = pipe

  def getNamePool: NamePool =
    getPipelineConfiguration.getConfiguration.getNamePool

  def getUnderlyingProvider: PullProvider = base
  def next(): Event = base.next()
  def current: Event = currentEvent
  def getAttributes: AttributeMap = base.getAttributes

  def getNamespaceDeclarations: Array[NamespaceBinding] =
    base.getNamespaceDeclarations

  def skipToMatchingEnd(): Event = base.skipToMatchingEnd()

  def close(): Unit =
    base.close()

  def getNodeName: NodeName = base.getNodeName
  def getStringValue: CharSequence = base.getStringValue
  def getAtomicValue: AtomicValue = base.getAtomicValue
  def getSchemaType: SchemaType = base.getSchemaType
  def getSourceLocator: Location = base.getSourceLocator
  def getUnparsedEntities: List[UnparsedEntity] = base.getUnparsedEntities
}
