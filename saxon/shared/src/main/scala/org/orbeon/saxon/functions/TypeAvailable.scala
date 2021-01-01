package org.orbeon.saxon.functions

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.Literal

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.model.JavaExternalObjectType

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.BooleanValue

class TypeAvailable extends SystemFunction {

  private def typeAvailable(lexicalName: String,
                            config: Configuration): Boolean = {
    var qName: StructuredQName = null
    try if (lexicalName.indexOf(':') < 0 && !lexicalName.startsWith("Q{")) {
      val uri: String = getRetainedStaticContext.getURIForPrefix("", useDefault = true)
      qName = new StructuredQName("", uri, lexicalName)
    } else {
      qName = StructuredQName.fromLexicalQName(lexicalName,
        useDefault = false,
        allowEQName = true,
        getRetainedStaticContext)
    } catch {
      case e: XPathException => {
        e.setErrorCode("XTDE1428")
        throw e
      }

    }
    val uri = qName.getURI
    if (uri == NamespaceConstant.JAVA_TYPE) {
      try {
        val className = JavaExternalObjectType.localNameToClassName(qName.getLocalPart)
        config.getConfClass(className, tracing = false)
        true
      } catch {
        case _: XPathException => false
      }
    } else {
      val `type`: SchemaType = config.getSchemaType(qName)
      `type` != null
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val lexicalQName: String = arguments(0).head.getStringValue
    BooleanValue.get(typeAvailable(lexicalQName, context.getConfiguration))
  }

  def makeFunctionCall(arguments: Array[Expression]): Expression = {
    try if (arguments(0).isInstanceOf[Literal]) {
      val b: Boolean = typeAvailable(
        arguments(0).asInstanceOf[Literal].value.getStringValue,
        getRetainedStaticContext.getConfiguration)
      return Literal.makeLiteral(BooleanValue.get(b))
    } catch {
      case e: XPathException => {}

    }
    super.makeFunctionCall(arguments.toIndexedSeq: _*)
  }

}
