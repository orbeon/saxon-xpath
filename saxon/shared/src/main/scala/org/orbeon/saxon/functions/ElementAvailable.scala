////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression
import org.orbeon.saxon.expr.StaticProperty
import org.orbeon.saxon.expr.StringLiteral
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.Feature
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om.NameChecker
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.om.StandardNames._
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.BooleanValue

/**
 * This class supports the XSLT element-available function.  Note that when running in a 2.0 processor,
 * it only looks for XSLT 2.0 instructions; but when running in a 3.0 processor, it recognizes all
 * elements in the XSLT namespace whether or not they are classified as instructions.
 */
object ElementAvailable {
  def isXslt30Element(fp: Int) = fp match {
    case XSL_ACCEPT |
         XSL_ACCUMULATOR |
         XSL_ACCUMULATOR_RULE |
         XSL_ANALYZE_STRING |
         XSL_APPLY_IMPORTS |
         XSL_APPLY_TEMPLATES |
         XSL_ASSERT |
         XSL_ATTRIBUTE |
         XSL_ATTRIBUTE_SET |
         XSL_BREAK |
         XSL_CALL_TEMPLATE |
         XSL_CATCH |
         XSL_CHARACTER_MAP |
         XSL_CHOOSE |
         XSL_COMMENT |
         XSL_CONTEXT_ITEM |
         XSL_COPY |
         XSL_COPY_OF |
         XSL_DECIMAL_FORMAT |
         XSL_DOCUMENT |
         XSL_ELEMENT |
         XSL_EVALUATE |
         XSL_EXPOSE |
         XSL_FALLBACK |
         XSL_FOR_EACH |
         XSL_FOR_EACH_GROUP |
         XSL_FORK |
         XSL_FUNCTION |
         XSL_GLOBAL_CONTEXT_ITEM |
         XSL_IF |
         XSL_IMPORT |
         XSL_IMPORT_SCHEMA |
         XSL_INCLUDE |
         XSL_ITERATE |
         XSL_KEY |
         XSL_MAP |
         XSL_MAP_ENTRY |
         XSL_MATCHING_SUBSTRING |
         XSL_MERGE |
         XSL_MERGE_ACTION |
         XSL_MERGE_KEY |
         XSL_MERGE_SOURCE |
         XSL_MESSAGE |
         XSL_MODE |
         XSL_NAMESPACE |
         XSL_NAMESPACE_ALIAS |
         XSL_NEXT_ITERATION |
         XSL_NEXT_MATCH |
         XSL_NON_MATCHING_SUBSTRING |
         XSL_NUMBER |
         XSL_ON_COMPLETION |
         XSL_ON_EMPTY |
         XSL_ON_NON_EMPTY |
         XSL_OTHERWISE |
         XSL_OUTPUT |
         XSL_OUTPUT_CHARACTER |
         XSL_OVERRIDE |
         XSL_PACKAGE |
         XSL_PARAM |
         XSL_PERFORM_SORT |
         XSL_PRESERVE_SPACE |
         XSL_PROCESSING_INSTRUCTION |
         XSL_RESULT_DOCUMENT |
         XSL_SEQUENCE |
         XSL_SORT |
         XSL_SOURCE_DOCUMENT |
         XSL_STRIP_SPACE |
         XSL_STYLESHEET |
         XSL_TEMPLATE |
         XSL_TEXT |
         XSL_TRANSFORM |
         XSL_TRY |
         XSL_USE_PACKAGE |
         XSL_VALUE_OF |
         XSL_VARIABLE |
         XSL_WHEN |
         XSL_WHERE_POPULATED |
         XSL_WITH_PARAM =>
      true
    case _ =>
      false
  }
}

class ElementAvailable extends SystemFunction {
  /**
   * Special-case for element-available('xsl:evaluate') which may be dynamically-disabled,
   * and the spec says that this should be assessed at run-time.  By indicating that the
   * effect of the function depends on the run-time environment, early evaluation at compile
   * time is suppressed.
   */
  override def getSpecialProperties(arguments: Array[Expression]): Int = {
    try if (arguments(0).isInstanceOf[StringLiteral]) {
      val arg = arguments(0).asInstanceOf[StringLiteral].getStringValue
      val elem = getElementName(arg)
      if (elem.hasURI(NamespaceConstant.XSLT) && elem.getLocalPart == "evaluate") return super.getSpecialProperties(arguments) | StaticProperty.DEPENDS_ON_RUNTIME_ENVIRONMENT
    }
    catch {
      case e: XPathException =>

      // drop through
    }
    super.getSpecialProperties(arguments)
  }

  /**
   * Determine whether a particular instruction is available. Returns true
   * for XSLT instructions, Saxon extension instructions, and registered
   * user-defined extension instructions. In XSLT 3.0 all XSLT elements are recognized.
   * This is a change from XSLT 2.0 where only elements classified as instructions
   * were recognized.
   *
   * @param lexicalName the lexical QName of the element
   * @param edition     the target edition that the stylesheet is to run under, e.g. "HE", "JS"
   * @param context     the XPath evaluation context
   * @return true if the instruction is available, in the sense of the XSLT element-available() function
   * @throws XPathException if a dynamic error occurs (e.g., a bad QName)
   */
  @throws[XPathException]
  private def isElementAvailable(lexicalName: String, edition: String, context: XPathContext): Boolean = {
    val qName = getElementName(lexicalName)
    if (qName.hasURI(NamespaceConstant.XSLT)) {
      val fp = context.getConfiguration.getNamePool.getFingerprint(NamespaceConstant.XSLT, qName.getLocalPart)
      var known = ElementAvailable.isXslt30Element(fp).asInstanceOf[Boolean]
      if (fp == XSL_EVALUATE) known = known && !context.getConfiguration.getBooleanProperty(Feature.DISABLE_XSL_EVALUATE)
      return known
    }
    else if (qName.hasURI(NamespaceConstant.IXSL) && !(edition == "JS")) return false
    context.getConfiguration.isExtensionElementAvailable(qName)
  }

  @throws[XPathException]
  private def getElementName(lexicalName: String) = try if (lexicalName.indexOf(':') < 0 && NameChecker.isValidNCName(lexicalName)) {
    val uri = getRetainedStaticContext.getURIForPrefix("", useDefault = true)
    new StructuredQName("", uri, lexicalName)
  }
  else StructuredQName.fromLexicalQName(lexicalName, useDefault = false, allowEQName = true, getRetainedStaticContext)
  catch {
    case e: XPathException =>
      val err = new XPathException("Invalid element name passed to element-available(): " + e.getMessage)
      err.setErrorCode("XTDE1440")
      throw err
  }

  /**
   * Evaluate the expression
   *
   * @param context   the dynamic evaluation context
   * @param arguments the values of the arguments, supplied as SequenceIterators
   * @return the result of the evaluation, in the form of a SequenceIterator
   * @throws XPathException
   * if a dynamic error occurs during the evaluation of the expression
   */
  @throws[XPathException]
  override def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val lexicalQName = arguments(0).head.getStringValue
    val b = isElementAvailable(lexicalQName, getRetainedStaticContext.getPackageData.getTargetEdition, context)
    BooleanValue.get(b)
  }
}