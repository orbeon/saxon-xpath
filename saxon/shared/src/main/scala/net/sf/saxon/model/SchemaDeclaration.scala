////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.model

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.pattern.NodeTest




/**
  * This is a marker interface that acts as a surrogate for an object representing
  * a global element or attribute declaration.
  * The real implementation of these declarations is available in the schema-aware
  * version of the Saxon product.
  */
trait SchemaDeclaration {

  def getFingerprint(): Int

  def getComponentName(): StructuredQName

  def getType(): SchemaType

  def makeSchemaNodeTest(): NodeTest

  def isNillable(): Boolean

  def isAbstract(): Boolean

  def hasTypeAlternatives(): Boolean

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
