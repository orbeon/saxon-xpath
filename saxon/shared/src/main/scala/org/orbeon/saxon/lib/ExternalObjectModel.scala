////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.expr.JPConverter

import org.orbeon.saxon.expr.PJConverter

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.trans.XPathException

import javax.xml.transform.Result

import javax.xml.transform.Source




trait ExternalObjectModel {

  def getDocumentClassName: String

  def getIdentifyingURI: String

  def getPJConverter(targetClass: Class[_]): PJConverter

  def getJPConverter(sourceClass: Class[_], config: Configuration): JPConverter

  def getNodeListCreator(node: AnyRef): PJConverter

  def getDocumentBuilder(result: Result): Receiver

  def sendSource(source: Source, receiver: Receiver): Boolean

  def unravel(source: Source, config: Configuration): NodeInfo

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This interface must be implemented by any third-party object model that can
  * be wrapped with a wrapper that implements the Saxon Object Model (the NodeInfo interface).
  * <p>This interface is designed to enable advanced applications to implement and register
  * new object model implementations that Saxon can then use without change. Although it is intended
  * for external use, it cannot at this stage be considered part of the stable Saxon Public API.
  * In particular, it is likely that the interface will grow by the addition of new methods.</p>
  * <p>For maximum integration, an object may extend {@link org.orbeon.saxon.om.TreeModel} as well as implementing
  * this interface. To implement <code>TreeModel</code>, it must supply a Builder; in effect this
  * means that it will be possible to use the external object model for output as well as for
  * input.</p>
  */
