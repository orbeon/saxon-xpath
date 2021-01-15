package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor}
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.om.{NameChecker, QNameParser, Sequence, StructuredQName}
import org.orbeon.saxon.style.StylesheetPackage
import org.orbeon.saxon.trans.{SymbolicName, XPathException}
import org.orbeon.saxon.value.{BooleanValue, NumericValue}

import scala.util.control.Breaks._


class FunctionAvailable extends SystemFunction {

  override def makeFunctionCall(arguments: Expression*): Expression = {
    val pack = getRetainedStaticContext.getPackageData
    pack match {
      case stylesheetPackage: StylesheetPackage =>
        stylesheetPackage.setRetainUnusedFunctions()
      case _                                    =>
    }
    super.makeFunctionCall(arguments: _*)
  }

  override def makeOptimizedFunctionCall(
    visitor     : ExpressionVisitor,
    contextInfo : ContextItemStaticInfo,
    arguments   : Expression*
  ): Expression =
    arguments(0) match {
      case literal: Literal if arguments.length == 1 || arguments(1).isInstanceOf[Literal] =>
        val lexicalQName = literal
          .value
          .getStringValue
        val env          = visitor.getStaticContext
        var b = false
        val qp = new QNameParser(getRetainedStaticContext)
          .withAcceptEQName(true)
          .withErrorOnBadSyntax("XTDE1400")
          .withErrorOnUnresolvedPrefix("XTDE1400")
        val functionName                  =
          qp.parse(lexicalQName, env.getDefaultFunctionNamespace)
        var minArity = 0
        var maxArity                      = 20
        if (getArity == 2) {
          minArity = arguments(1)
            .evaluateItem(env.makeEarlyEvaluationContext())
            .asInstanceOf[NumericValue]
            .longValue
            .toInt
          maxArity = minArity
        }
        var i = minArity
        breakable {
          while (i <= maxArity) {
            val sn = new SymbolicName.F(functionName, i)
            if (env.getFunctionLibrary.isAvailable(sn)) {
              b = true
              break()
            }
            i += 1
          }
        }
        Literal.makeLiteral(BooleanValue.get(b))
      case _                                                                               =>
        null
    }

  private def isFunctionAvailable(
    lexicalName : String,
    edition     : String,
    arity       : Int,
    context     : XPathContext
  ): Boolean =
    if (arity == -1) {
      0.until(20).exists(isFunctionAvailable(lexicalName, edition, _, context))
    } else {
      val qName =
        try
          if (NameChecker.isValidNCName(lexicalName)) {
            val uri = NamespaceConstant.FN
            new StructuredQName("", uri, lexicalName)
          } else {
            StructuredQName.fromLexicalQName(
              lexicalName,
              useDefault = false,
              allowEQName = true,
              getRetainedStaticContext
            )
        } catch {
          case e: XPathException =>
            e.setErrorCode("XTDE1400")
            e.setXPathContext(context)
            throw e
        }
      val lib = context.getController.getExecutable.getFunctionLibrary
      val sn  = new SymbolicName.F(qName, arity)
      lib.isAvailable(sn)
    }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val lexicalQName = arguments(0).head.getStringValue
    var arity = -1
    if (arguments.length == 2)
      arity = arguments(1).head.asInstanceOf[NumericValue].longValue.toInt
    BooleanValue.get(
      isFunctionAvailable(
        lexicalQName,
        getRetainedStaticContext.getPackageData.getTargetEdition,
        arity,
        context
      )
    )
  }
}
