package net.sf.saxon.functions

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.om.NameChecker

import net.sf.saxon.om.QNameParser

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.style.StylesheetPackage

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.NumericValue
import scala.util.control.Breaks._

class FunctionAvailable extends SystemFunction {

  override def makeFunctionCall(arguments: Expression*): Expression = {
    val pack: PackageData = getRetainedStaticContext.getPackageData
    if (pack.isInstanceOf[StylesheetPackage]) {
      pack.asInstanceOf[StylesheetPackage].setRetainUnusedFunctions()
    }
    super.makeFunctionCall(arguments: _*)
  }

  override def makeOptimizedFunctionCall(visitor: ExpressionVisitor,
                                         contextInfo: ContextItemStaticInfo,
                                         arguments: Expression*): Expression =
    if (arguments(0).isInstanceOf[Literal] &&
      (arguments.length == 1 || arguments(1).isInstanceOf[Literal])) {
      val lexicalQName: String = arguments(0)
        .asInstanceOf[Literal]
        .value
        .getStringValue
      val env: StaticContext = visitor.getStaticContext
      var b: Boolean = false
      val qp: QNameParser = new QNameParser(getRetainedStaticContext)
        .withAcceptEQName(true)
        .withErrorOnBadSyntax("XTDE1400")
        .withErrorOnUnresolvedPrefix("XTDE1400")
      val functionName: StructuredQName =
        qp.parse(lexicalQName, env.getDefaultFunctionNamespace)
      var minArity: Int = 0
      var maxArity: Int = 20
      if (getArity == 2) {
        minArity = arguments(1)
          .evaluateItem(env.makeEarlyEvaluationContext())
          .asInstanceOf[NumericValue]
          .longValue()
          .toInt
        maxArity = minArity
      }
      var i: Int = minArity
      breakable {
        while (i <= maxArity) {
          val sn: SymbolicName.F = new SymbolicName.F(functionName, i)
          if (env.getFunctionLibrary.isAvailable(sn)) {
            b = true
            break()
          }
          {
            i += 1;
            i - 1
          }
        }
      }
      Literal.makeLiteral(BooleanValue.get(b))
    } else {
      null
    }

  private def isFunctionAvailable(lexicalName: String,
                                  edition: String,
                                  arity: Int,
                                  context: XPathContext): Boolean = {
    if (arity == -1) {
      (0.until(20))
        .find(isFunctionAvailable(lexicalName, edition, _, context))
        .map(_ => true)
        .getOrElse(false)
    }
    var qName: StructuredQName = null
    try if (NameChecker.isValidNCName(lexicalName)) {
      val uri: String = NamespaceConstant.FN
      qName = new StructuredQName("", uri, lexicalName)
    } else {
      qName = StructuredQName.fromLexicalQName(lexicalName,
        useDefault = false,
        allowEQName = true,
        getRetainedStaticContext)
    } catch {
      case e: XPathException => {
        e.setErrorCode("XTDE1400")
        e.setXPathContext(context)
        throw e
      }

    }
    val lib: FunctionLibrary =
      context.getController.getExecutable.getFunctionLibrary
    val sn: SymbolicName.F = new SymbolicName.F(qName, arity)
    lib.isAvailable(sn)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val lexicalQName: String = arguments(0).head().getStringValue
    var arity: Int = -1
    if (arguments.length == 2) {
      arity = arguments(1).head().asInstanceOf[NumericValue].longValue().toInt
    }
    BooleanValue.get(
      isFunctionAvailable(
        lexicalQName,
        getRetainedStaticContext.getPackageData.getTargetEdition,
        arity,
        context))
  }

}
