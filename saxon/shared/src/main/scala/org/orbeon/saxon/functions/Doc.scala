package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionTool, ExpressionVisitor}
import org.orbeon.saxon.functions.Doc._
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet
import org.orbeon.saxon.lib.{Feature, ParseOptions}
import org.orbeon.saxon.om.{GroundedValue, NodeInfo, Sequence, ZeroOrOne}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.{Configuration, Controller}
import org.orbeon.saxon.value.AtomicValue

import scala.beans.BeanProperty

object Doc {

  def maybePreEvaluate(sf: SystemFunction,
                       arguments: Array[Expression]): Expression =
    if (arguments.length > 1 ||
      !sf.getRetainedStaticContext.getConfiguration.getBooleanProperty(
        Feature.PRE_EVALUATE_DOC_FUNCTION)) {
      sf.getDetails.properties = sf.getDetails.properties | BuiltInFunctionSet.LATE
      null
    } else {
      new SystemFunctionCall(sf, arguments) {
        override def preEvaluate(visitor: ExpressionVisitor): Expression = {
          val config: Configuration = visitor.getConfiguration
          try {
            val firstArg: GroundedValue = getArg(0)
              .asInstanceOf[Literal]
              .value
            if (firstArg.getLength == 0) {
              return null
            } else if (firstArg.getLength > 1) {
              return this
            }
            val href: String = firstArg.head.getStringValue
            if (href.indexOf('#') >= 0) {
              return this
            }
            val item: NodeInfo = DocumentFn.preLoadDoc(
              href,
              sf.getStaticBaseUriString,
              config,
              getLocation)
            if (item != null) {
              val constant: Expression = Literal.makeLiteral(item)
              ExpressionTool.copyLocationInfo(getArg(0), constant)
              return constant
            }
          } catch {
            case err: Exception => return this

          }
          this
        }

        override def optimize(
                               visitor: ExpressionVisitor,
                               contextItemType: ContextItemStaticInfo): Expression = {
          optimizeChildren(visitor, contextItemType)
          if (getArg(0).isInstanceOf[StringLiteral]) {
            preEvaluate(visitor)
          }
          this
        }
      }
    }

}

class Doc extends SystemFunction with Callable {

  @BeanProperty
  var parseOptions: ParseOptions = _

  override def getCardinality(arguments: Array[Expression]): Int =
    arguments(0).getCardinality & ~StaticProperty.ALLOWS_MANY

  override def makeFunctionCall(arguments: Expression*): Expression = {
    val expr: Expression = maybePreEvaluate(this, arguments.toArray)
    if (expr == null) super.makeFunctionCall(arguments: _*) else expr
  }

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[NodeInfo] = {
    val hrefVal: AtomicValue = arguments(0).head.asInstanceOf[AtomicValue]
    if (hrefVal == null) {
      return ZeroOrOne.empty().asInstanceOf[ZeroOrOne[NodeInfo]]
    }
    val href: String = hrefVal.getStringValue
    val packageData: PackageData = getRetainedStaticContext.getPackageData
    val item: NodeInfo = DocumentFn.makeDoc(
      href,
      getRetainedStaticContext.getStaticBaseUriString,
      packageData,
      getParseOptions,
      context,
      null,
      silent = false)
    if (item == null) {
      throw new XPathException("Failed to load document " + href,
        "FODC0002",
        context)
    }
    val controller: Controller = context.getController
//    if (parseOptions != null && controller.isInstanceOf[XsltController]) {
//      controller
//        .asInstanceOf[XsltController]
//        .getAccumulatorManager
//        .setApplicableAccumulators(item.getTreeInfo,
//          parseOptions.getApplicableAccumulators)
//    }
    new ZeroOrOne(item)
  }

  override def getSpecialProperties(arguments: Array[Expression]): Int =
    StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET |
      StaticProperty.NO_NODES_NEWLY_CREATED |
      StaticProperty.SINGLE_DOCUMENT_NODESET

}
