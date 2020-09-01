////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.om.NameChecker

import net.sf.saxon.om.QNameException

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.QNameValue

import net.sf.saxon.value.StringValue

import QNameFn._




object QNameFn {

  def expandedQName(namespace: StringValue, lexical: StringValue): QNameValue = {
    var uri: String = null
    uri = if (namespace == null) null else namespace.getStringValue
    try {
      val lex: String = lexical.getStringValue
      val parts: Array[String] = NameChecker.getQNameParts(lex)
// The QNameValue constructor does not check the prefix
      if (!parts(0).isEmpty && !NameChecker.isValidNCName(parts(0))) {
        val err = new XPathException(
          "Malformed prefix in QName: '" + parts(0) + '\'')
        err.setErrorCode("FOCA0002")
        throw err
      }
      new QNameValue(parts(0), uri, parts(1), BuiltInAtomicType.QNAME, true)
    } catch {
      case e: QNameException =>
        throw new XPathException(e.getMessage, "FOCA0002")

      case err: XPathException => {
        if (err.getErrorCodeLocalPart.==("FORG0001")) {
          err.setErrorCode("FOCA0002")
        }
        throw err
      }

    }
  }

}

class QNameFn extends SystemFunction {

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences
    * @return the result of the evaluation, in the form of a Sequence
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): QNameValue =
    expandedQName(arguments(0).head.asInstanceOf[StringValue],
                  arguments(1).head.asInstanceOf[StringValue])

  override def getCompilerName(): String = "QNameFnCompiler"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class supports the fn:QName() function (previously named fn:expanded-QName())
  */
