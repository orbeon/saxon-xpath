////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.SystemFunction
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.om.{Function, Sequence, StructuredQName, ZeroOrOne}
import org.orbeon.saxon.value.QNameValue


/**
  * This class implements the function function-name(), which is a standard function in XPath 3.0
  */
class FunctionName extends SystemFunction {

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequence objects
    * @return the result of the evaluation, in the form of a Sequence
    * @throws org.orbeon.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_] = {
    val f = arguments(0).head.asInstanceOf[Function]
    assert(f != null)
    val name = f.getFunctionName
    if (name == null) {
      ZeroOrOne.empty
    } else if (name.hasURI(NamespaceConstant.ANONYMOUS)) {
      // Used for inline functions
      ZeroOrOne.empty
    } else {
      val result = new QNameValue(name, BuiltInAtomicType.QNAME)
      new ZeroOrOne(result)
    }
  }
}

