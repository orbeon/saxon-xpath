package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Literal

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import java.net.URI

import java.net.URISyntaxException

import CollatingFunctionFree._

object CollatingFunctionFree {

  def expandCollationURI(collationName: String,
                         expressionBaseURI: URI): String = {
    var collName = collationName
    var collationURI: URI = new URI(collName)
    if (!collationURI.isAbsolute) {
      if (expressionBaseURI == null) {
        throw new XPathException(
          "Cannot resolve relative collation URI '" + collName +
            "': unknown or invalid base URI",
          "FOCH0002")
      }
      collationURI = expressionBaseURI.resolve(collationURI)
      collName = collationURI.toString
    }
    collName
  }

}

class CollatingFunctionFree extends SystemFunction {

  private def getCollationArgument(): Int = getArity - 1

  override def makeOptimizedFunctionCall(
                                          visitor: ExpressionVisitor,
                                          contextInfo: ContextItemStaticInfo,
                                          arguments: Expression*): Expression = {
    val c: Expression = arguments(arguments.length - 1)
    if (c.isInstanceOf[Literal]) {
      var coll: String = c.asInstanceOf[Literal].value.getStringValue
      try {
        var collUri: URI = new URI(coll)
        if (!collUri.isAbsolute) {
          collUri = ResolveURI.makeAbsolute(coll, getStaticBaseUriString)
          coll = collUri.toASCIIString()
        }
      } catch {
        case e: URISyntaxException =>
          visitor.getStaticContext.issueWarning(
            "Cannot resolve relative collation URI " + coll,
            c.getLocation)

      }
      val fn: CollatingFunctionFixed = bindCollation(coll)
      val newArgs: Array[Expression] =
        Array.ofDim[Expression](arguments.length - 1)
      System.arraycopy(arguments, 0, newArgs, 0, newArgs.length)
      fn.makeFunctionCall(newArgs: _*)
    }
    null
  }

  def bindCollation(collationName: String): CollatingFunctionFixed = {
    val config: Configuration = getRetainedStaticContext.getConfiguration
    val fixed: CollatingFunctionFixed = new CollationKeyFn().asInstanceOf[CollatingFunctionFixed]
    fixed.setRetainedStaticContext(getRetainedStaticContext)
    fixed.setCollationName(collationName)
    fixed
  }

  def call(context: XPathContext, args: Array[Sequence]): Sequence = {
    val c: Int = getCollationArgument
    var collation: String = args(c).head().getStringValue
    collation =
      expandCollationURI(collation, getRetainedStaticContext.getStaticBaseUri)
    val fixed: CollatingFunctionFixed = bindCollation(collation)
    val retainedArgs: Array[Sequence] = Array.ofDim[Sequence](args.length - 1)
    System.arraycopy(args, 0, retainedArgs, 0, c)
    if (c + 1 < getArity) {
      System.arraycopy(args, c + 1, retainedArgs, c, getArity - c)
    }
    fixed.call(context, retainedArgs)
  }

  override def getStreamerName(): String =
    bindCollation(NamespaceConstant.CODEPOINT_COLLATION_URI).getStreamerName

}
