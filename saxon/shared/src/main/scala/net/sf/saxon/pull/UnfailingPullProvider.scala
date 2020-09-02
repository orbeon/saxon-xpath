package net.sf.saxon.pull

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.NamespaceBinding

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.value.AtomicValue

import java.util.List

import net.sf.saxon.pull.PullProvider.Event._

trait UnfailingPullProvider extends PullProvider {

  def next(): Event

  def current(): Event

  def getAttributes: AttributeMap

  def getNamespaceDeclarations(): Array[NamespaceBinding]

  def skipToMatchingEnd(): Event

  def close(): Unit

  def getNodeName: NodeName

  def getStringValue: CharSequence

  def getSchemaType(): SchemaType

  def getAtomicValue(): AtomicValue

  def getSourceLocator(): Location

  def getUnparsedEntities(): List[UnparsedEntity]

}
