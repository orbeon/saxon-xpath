package org.orbeon.saxon.model

import org.orbeon.saxon.om.StructuredQName


/**
  * This class represents the type of an external object returned by
  * an extension function, or supplied as an external variable/parameter.
  */
abstract class ExternalObjectType extends AnyExternalObjectType {
  def getName: String
  def getTargetNamespace: String
  def getTypeName: StructuredQName
  override def isPlainType: Boolean = false
}
