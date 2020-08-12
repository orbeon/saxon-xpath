////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Outputter

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import javax.xml.stream.XMLStreamWriter

import InvalidityReportGenerator._




object InvalidityReportGenerator {

  val REPORT_NS: String = "http://saxon.sf.net/ns/validation"

}

class InvalidityReportGenerator(config: Configuration) extends StandardInvalidityHandler(config) {

  def this(config: Configuration, receiver: Outputter) =
    this(???) // TODO: Scala does not allow multiple super constructor calls

  def setReceiver(receiver: Outputter): Unit = {}

  def setSystemId(id: String): Unit = {}

  def setSchemaName(name: String): Unit = {}

  def getErrorCount(): Int = 0

  def getWarningCount(): Int = 0

  def setXsdVersion(version: String): Unit = {}

  def getWriter(): XMLStreamWriter = null

  override def reportInvalidity(failure: Invalidity): Unit = {}

  override def startReporting(systemId: String): Unit = {}

  override def endReporting(): Sequence = null

  def createMetaData(): Unit = {}

}
