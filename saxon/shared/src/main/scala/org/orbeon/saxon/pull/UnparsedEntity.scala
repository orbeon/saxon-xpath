////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.pull

import scala.beans.{BeanProperty, BooleanBeanProperty}

class UnparsedEntity {

  var name: String = _
  def getName: String = name
  def setName(name: String): Unit = this.name = name

  var systemId: String = _
  def getSystemId: String = systemId
  def setSystemId(systemId: String): Unit = this.systemId = systemId

  var publicId: String = _
  def getPublicId: String = publicId
  def setPublicId(publicId: String): Unit = this.publicId = publicId

  var baseURI: String = _
  def getBaseURI: String = baseURI
  def setBaseURI(baseURI: String): Unit = this.baseURI = baseURI

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is used to represent unparsed entities in the PullProvider interface
  */
