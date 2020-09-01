////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Controller

import net.sf.saxon.expr.Component

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.trans.XPathException




class GlobalParam extends GlobalVariable {

  private var implicitlyRequired: Boolean = _

  def setImplicitlyRequiredParam(requiredParam: Boolean): Unit = {
    this.implicitlyRequired = requiredParam
  }

  def isImplicitlyRequiredParam: Boolean = this.implicitlyRequired

  override def getTracingTag(): String = "xsl:param"

  /**
    * Evaluate the variable
    */
  override def evaluateVariable(context: XPathContext,
                                target: Component): GroundedValue = {
    val controller: Controller = context.getController
    assert(controller != null)
    val b: Bindery = controller.getBindery(getPackageData)
    var `val`: GroundedValue = b.getGlobalVariableValue(this)
    if (`val` != null) {
      if (`val`.isInstanceOf[Bindery.FailureValue]) {
        throw `val`.asInstanceOf[Bindery.FailureValue].getObject
      }
      return `val`
    }
    `val` = controller.getConvertedParameter(getVariableQName,
                                             getRequiredType,
                                             context)
    if (`val` != null) {
      b.saveGlobalVariableValue(this, `val`)
    }
    if (isRequiredParam) {
      val e: XPathException = new XPathException(
        "No value supplied for required parameter $" + getVariableQName.getDisplayName)
      e.setXPathContext(context)
      e.setLocator(this)
      e.setErrorCode(if (getPackageData.isXSLT) "XTDE0050" else "XPDY0002")
      throw e
    } else if (isImplicitlyRequiredParam) {
      val e: XPathException = new XPathException(
        "A value must be supplied for parameter $" + getVariableQName.getDisplayName +
          " because there is no default value for the required type")
      e.setXPathContext(context)
      e.setLocator(this)
      e.setErrorCode("XTDE0700")
      throw e
    }
// evaluate and save the default value
    actuallyEvaluate(context, target)
  }

  override def evaluateVariable(context: XPathContext): GroundedValue =
    evaluateVariable(context, null)

   override def getFlags(): String = {
    var f: String = super.getFlags
    if (isImplicitlyRequiredParam) {
      f += "i"
    }
    f
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * The compiled form of a global xsl:param element in an XSLT stylesheet or an
  * external variable declared in the prolog of a Query. <br>
  * The xsl:param element in XSLT has mandatory attribute name and optional attribute select. It can also
  * be specified as required="yes" or required="no". In standard XQuery 1.0 external variables are always required,
  * and no default value can be specified; but Saxon provides an extension pragma that allows a query
  * to specify a default. XQuery 1.1 adds standard syntax for defining a default value.
  */
