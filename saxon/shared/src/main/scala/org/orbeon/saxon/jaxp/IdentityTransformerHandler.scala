////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.jaxp

import javax.xml.transform.Result
import javax.xml.transform.sax.TransformerHandler
import javax.xml.transform.stream.StreamResult
import org.orbeon.saxon.event.{ReceivingContentHandler, Stripper}
import org.orbeon.saxon.om.AllElementsSpaceStrippingRule
import org.orbeon.saxon.serialize.SerializationProperties
import org.orbeon.saxon.trans.XPathException
import org.xml.sax.SAXException


/**
 * <b>IdentityTransformerHandler</b> implements the javax.xml.transform.sax.TransformerHandler
 * interface. It acts as a ContentHandler and LexicalHandler which receives a stream of
 * SAX events representing an input document, and performs an identity transformation passing
 * these events to a Result
 *
 * @author Michael H. Kay
 */
class IdentityTransformerHandler protected[jaxp] (var controller: IdentityTransformer)
  extends ReceivingContentHandler with TransformerHandler {

  /**
   * Create a IdentityTransformerHandler and initialise variables. The constructor is protected, because
   * the Filter should be created using `newTransformerHandler()` in the `SAXTransformerFactory`
   * class
   */
  setPipelineConfiguration(controller.getConfiguration.makePipelineConfiguration)

  /*@Nullable*/
  private var result: Result = null
  private var systemId: String = null

  def getTransformer: IdentityTransformer = controller

  /**
   * Set the SystemId of the document
   */
  def setSystemId(url: String): Unit =
    systemId = url

  /**
   * Get the systemId of the document
   */
  def getSystemId: String = systemId

  /**
   * Set the output destination of the transformation
   */
  def setResult(result: Result): Unit = {
    if (result == null)
      throw new IllegalArgumentException("Result must not be null")
    this.result = result
  }

  /**
   * Get the output destination of the transformation
   *
   * @return the output destination
   */
  def getResult: Result = result

  /**
   * Override the behaviour of startDocument() in ReceivingContentHandler
   */
  override def startDocument(): Unit = {
    if (result == null)
      result = new StreamResult(System.out)
    try {
      val props = controller.getOutputProperties
      val config = getConfiguration
      val sf = config.getSerializerFactory
      var out = sf.getReceiver(result, new SerializationProperties(props))
      setPipelineConfiguration(out.getPipelineConfiguration)
      if (config.isStripsAllWhiteSpace)
        out = new Stripper(AllElementsSpaceStrippingRule.getInstance, out)
      setReceiver(out)
    } catch {
      case err: XPathException =>
        throw new SAXException(err)
    }
    super.startDocument()
  }
}

