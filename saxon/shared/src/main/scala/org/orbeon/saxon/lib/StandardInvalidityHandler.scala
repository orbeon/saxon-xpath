////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import javax.xml.transform.dom.DOMLocator
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.model.ValidationFailure
import org.orbeon.saxon.om.{AbsolutePath, NodeInfo, Sequence}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.Navigator
import org.orbeon.saxon.value.EmptySequence

class StandardInvalidityHandler(var config: Configuration) extends StandardDiagnostics with InvalidityHandler {
  private var logger: Logger = null
  this.logger = config.getLogger

  def setLogger(logger: Logger): Unit = this.logger = logger

  def getLogger: Logger = logger

  def getConfiguration: Configuration = config

  @throws[XPathException]
  override def startReporting(systemId: String): Unit = {
  }

  @throws[XPathException]
  override def reportInvalidity(failure: Invalidity): Unit = {
    var localLogger = logger
    if (localLogger == null) localLogger = config.getLogger
    val explanation = getExpandedMessage(failure)
    val constraintReference = getConstraintReferenceMessage(failure)
    val validationLocation = failure.asInstanceOf[ValidationFailure].getValidationLocationText
    val contextLocation = failure.asInstanceOf[ValidationFailure].getContextLocationText
    val finalMessage = "Validation error " + getLocationMessage(failure) + "\n  " + wordWrap(explanation) + //wordWrap(validationLocation.isEmpty ? "" : "\n  " + validationLocation) +
      wordWrap(if (contextLocation.isEmpty) ""
      else "\n  " + contextLocation) + wordWrap(if (constraintReference == null) ""
    else "\n  " + constraintReference) + formatListOfOffendingNodes(failure.asInstanceOf[ValidationFailure])
    localLogger.error(finalMessage)
  }

  def getLocationMessage(err: Invalidity): String = {
    var locMessage: String = ""
    var systemId: String = null
    val node: NodeInfo = err.getInvalidNode
    var path: AbsolutePath = null
    var nodeMessage: String = null
    val lineNumber = err.getLineNumber
    if (err.isInstanceOf[DOMLocator]) nodeMessage = "at " + err.asInstanceOf[DOMLocator].getOriginatingNode.getNodeName + ' '
    else if ((lineNumber == -1) && ({
      path = err.getPath
      path
    } != null)) nodeMessage = "at " + path.toString + " "
    else if (node != null) nodeMessage = "at " + Navigator.getPath(node) + ' '
    val containsLineNumber = lineNumber != -1
    if (nodeMessage != null) locMessage += nodeMessage
    if (containsLineNumber) {
      locMessage += "on line " + lineNumber + ' '
      if (err.getColumnNumber != -1) locMessage += "column " + err.getColumnNumber + ' '
    }
    systemId = err.getSystemId
    if (systemId != null && systemId.length != 0) locMessage += (if (containsLineNumber) "of "
    else "in ") + abbreviateLocationURI(systemId) + ':'
    locMessage
  }

  def getExpandedMessage(err: Invalidity): String = {
    val code = err.getErrorCode
    (if (code == null) ""
    else code + ": ") + err.getMessage
  }

  def getConstraintReferenceMessage(err: Invalidity): String = {
    if (err.getSchemaPart == -1) return null
    "See http://www.w3.org/TR/xmlschema-" + err.getSchemaPart + "/#" + err.getConstraintName + " clause " + err.getConstraintClauseNumber
  }

  @throws[XPathException]
  override def endReporting: Sequence = EmptySequence.getInstance
}