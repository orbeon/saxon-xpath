////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.QNameFn._
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.om.{NameChecker, QNameException, Sequence}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{QNameValue, StringValue}


/**
  * This class supports the fn:QName() function (previously named fn:expanded-QName())
  */
object QNameFn {

  def expandedQName(namespace: StringValue, lexical: StringValue): QNameValue = {
    val uri = if (namespace == null) null else namespace.getStringValue
    try {
      val lex   = lexical.getStringValue
      val parts = NameChecker.getQNameParts(lex)
      // The QNameValue constructor does not check the prefix
      if (parts(0).nonEmpty && ! NameChecker.isValidNCName(parts(0))) {
        val err = new XPathException("Malformed prefix in QName: '" + parts(0) + '\'')
        err.setErrorCode("FOCA0002")
        throw err
      }
      new QNameValue(parts(0), uri, parts(1), BuiltInAtomicType.QNAME, true)
    } catch {
      case e: QNameException =>
        throw new XPathException(e.getMessage, "FOCA0002")
      case err: XPathException =>
        if (err.getErrorCodeLocalPart == "FORG0001")
          err.setErrorCode("FOCA0002")
        throw err
    }
  }
}

class QNameFn extends SystemFunction {

  def call(
    context   : XPathContext,
    arguments : Array[Sequence]
  ): QNameValue =
    expandedQName(
      arguments(0).head.asInstanceOf[StringValue],
      arguments(1).head.asInstanceOf[StringValue]
    )

  override def getCompilerName: String = "QNameFnCompiler"
}
