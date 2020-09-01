////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.hof

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.om.Function

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.QNameValue




class FunctionName extends SystemFunction {

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequence objects
    * @return the result of the evaluation, in the form of a Sequence
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_] = {
    val f: Function = arguments(0).head.asInstanceOf[Function]
    assert(f != null)
    val name: StructuredQName = f.getFunctionName
    if (name == null) {
      ZeroOrOne.empty()
    } else if (name.hasURI(NamespaceConstant.ANONYMOUS)) {
// Used for inline functions
      ZeroOrOne.empty()
    } else {
      val result: QNameValue = new QNameValue(name, BuiltInAtomicType.QNAME)
      new ZeroOrOne(result)
    }
  }

}

// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements the function function-name(), which is a standard function in XPath 3.0
  */
// Copyright (c) 2012-2020 Saxonica Limited
