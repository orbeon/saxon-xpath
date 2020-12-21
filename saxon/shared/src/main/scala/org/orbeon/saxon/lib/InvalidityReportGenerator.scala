////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import javax.xml.stream.XMLStreamWriter

import InvalidityReportGenerator._




object InvalidityReportGenerator {

  val REPORT_NS: String = "http://saxon.sf.net/ns/validation"

}

class InvalidityReportGenerator(config: Configuration) extends StandardInvalidityHandler(config) {

  def this(config: Configuration, receiver: Outputter) =
    this(???) // TODO: Scala does not allow multiple super constructor calls

  def setReceiver(receiver: Outputter): Unit = ()

  def setSystemId(id: String): Unit = ()

  def setSchemaName(name: String): Unit = ()

  def getErrorCount: Int = 0

  def getWarningCount: Int = 0

  def setXsdVersion(version: String): Unit = ()

  def getWriter: XMLStreamWriter = null

  override def reportInvalidity(failure: Invalidity): Unit = ()

  override def startReporting(systemId: String): Unit = ()

  override def endReporting(): Sequence = null

  def createMetaData(): Unit = ()

}
