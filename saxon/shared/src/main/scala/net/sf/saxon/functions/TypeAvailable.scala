package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Literal

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.JavaExternalObjectType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

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
    val uri: String = qName.getURI
    if (uri == NamespaceConstant.JAVA_TYPE) {
      try {
        val className: String =
          JavaExternalObjectType.localNameToClassName(qName.getLocalPart)
        config.getConfClass(className, tracing = false, null)
        true
      } catch {
        case err: XPathException => false

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
      Literal.makeLiteral(BooleanValue.get(b))
    } catch {
      case e: XPathException => {}

    }
    super.makeFunctionCall(arguments.toIndexedSeq: _*)
  }

}
