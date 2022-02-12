package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, Literal, XPathContext}
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{JavaExternalObjectType, SchemaType}
import org.orbeon.saxon.om.{Sequence, StructuredQName}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.BooleanValue


class TypeAvailable extends SystemFunction {

  private def typeAvailable(lexicalName: String, config: Configuration): Boolean = {
    var qName =
      try
        if (lexicalName.indexOf(':') < 0 && ! lexicalName.startsWith("Q{")) {
          val uri = getRetainedStaticContext.getURIForPrefix("", useDefault = true)
          new StructuredQName("", uri, lexicalName)
        } else {
          StructuredQName.fromLexicalQName(
            lexicalName,
            useDefault = false,
            allowEQName = true,
            getRetainedStaticContext
          )
        }
      catch {
        case e: XPathException =>
          e.setErrorCode("XTDE1428")
          throw e
      }
    val uri = qName.getURI
    if (uri == NamespaceConstant.JAVA_TYPE) {
      try {
        val className = JavaExternalObjectType.localNameToClassName(qName.getLocalPart)
        config.getConfClass(className, tracing = false)
        true
      } catch {
        case _: XPathException =>
          false
      }
    } else {
      val `type` = config.getSchemaType(qName)
      `type` != null
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val lexicalQName = arguments(0).head.getStringValue
    BooleanValue.get(typeAvailable(lexicalQName, context.getConfiguration))
  }

  def makeFunctionCall(arguments: Array[Expression]): Expression = {
    try
      arguments(0) match {
        case literal: Literal =>
          val b = typeAvailable(literal.value.getStringValue, getRetainedStaticContext.getConfiguration)
          return Literal.makeLiteral(BooleanValue.get(b))
        case _                =>
      }
    catch {
      case _: XPathException =>
    }
    super.makeFunctionCall(arguments.toIndexedSeq: _*)
  }
}
