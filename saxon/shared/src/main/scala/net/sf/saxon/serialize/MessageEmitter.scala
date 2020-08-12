////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.serialize

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import javax.xml.transform.OutputKeys

import java.util.Properties




class MessageEmitter extends XMLEmitter {

  override def setPipelineConfiguration(
      pipelineConfiguration: PipelineConfiguration): Unit = {
    super.setPipelineConfiguration(pipelineConfiguration)
    if (writer == null && outputStream == null) {
      this.outputStream = getConfiguration.getStandardErrorOutput
    }
    val props: Properties = new Properties()
    props.setProperty(OutputKeys.METHOD, "xml")
    props.setProperty(OutputKeys.INDENT, "yes")
    props.setProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    this.outputProperties = props
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (!suppressProcessingInstruction(target, data, locationId, properties)) {
      super.processingInstruction(target, data, locationId, properties)
    }
  }

   def suppressProcessingInstruction(target: String,
                                              data: CharSequence,
                                              locationId: Location,
                                              properties: Int): Boolean =
    target.==("error-code")

  override def endDocument(): Unit = {
    if (writer != null) {
      writer.write('\n')
      writer.flush()
    }
    super.endDocument()
  }

  override def close(): Unit = {
    if (writer != null) {
      writer.flush()
    }
    super.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * MessageEmitter is the default Receiver for xsl:message output.
  * It is the same as XMLEmitter except that (a) it adds an extra newline at the end of the message, and
  * (b) it recognizes a processing-instruction with name "error-code" specially; this is assumed to contain
  * the error-code requested on the {@code xsl:message} instruction. These changes can be overridden
  * in a user-supplied subclass.
  */
