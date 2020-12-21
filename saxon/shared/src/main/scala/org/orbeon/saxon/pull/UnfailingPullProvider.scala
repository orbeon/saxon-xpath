package org.orbeon.saxon.pull

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceBinding

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.value.AtomicValue

import java.util.List

import org.orbeon.saxon.pull.PullProvider.Event._

trait UnfailingPullProvider extends PullProvider {

  def next(): Event

  def current: Event

  def getAttributes: AttributeMap

  def getNamespaceDeclarations(): Array[NamespaceBinding]

  def skipToMatchingEnd(): Event

  def close(): Unit

  def getNodeName: NodeName

  def getStringValue: CharSequence

  def getSchemaType: SchemaType

  def getAtomicValue(): AtomicValue

  def getSourceLocator(): Location

  def getUnparsedEntities(): List[UnparsedEntity]

}
