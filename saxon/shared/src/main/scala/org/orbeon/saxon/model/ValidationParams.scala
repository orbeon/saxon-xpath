////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.sxpath.XPathDynamicContext

import org.orbeon.saxon.sxpath.XPathVariable


import java.util.HashMap

import java.util.Map
//import scala.collection.compat._
import scala.jdk.CollectionConverters._






object ValidationParams {

  def setValidationParams(declaredParams: Map[StructuredQName, XPathVariable],
                          actualParams: ValidationParams,
                          context: XPathDynamicContext): Unit = {
    for (p <- declaredParams.keySet.asScala) {
      val `var`: XPathVariable = declaredParams.get(p)
      val paramValue: Sequence = actualParams.get(p)
      if (paramValue != null) {
        context.setVariable(`var`, paramValue)
      } else {
        context.setVariable(`var`, `var`.getDefaultValue)
      }
    }
  }

}

class ValidationParams extends HashMap[StructuredQName, Sequence](20)

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class represents a collection of parameter values for use in schema validation;
  * it defines values for the parameters declared using the saxon:param XSD extension.
  * <p>The implementation is just a HashMap; giving the class a name helps type safety.</p>
  */
