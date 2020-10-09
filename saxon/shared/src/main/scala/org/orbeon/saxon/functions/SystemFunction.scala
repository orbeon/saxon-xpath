package org.orbeon.saxon.functions

import java.util.Properties

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.registry.BuiltInFunctionSet
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{FunctionItemType, ItemType, SpecificFunctionType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{Cardinality, IntegerValue, SequenceType}

import scala.beans.BeanProperty

object SystemFunction {

  def makeCall(name: String,
               rsc: RetainedStaticContext,
               arguments: Expression*): Expression = {
    val f = makeFunction(name, rsc, arguments.length)
    if (f == null)
      return null
    val expr = f.makeFunctionCall(arguments: _*)
    expr.setRetainedStaticContext(rsc)
    expr
  }

  def makeFunction(name: String,
                   rsc: RetainedStaticContext,
                   arity: Int): SystemFunction = {
    if (rsc == null) {
      throw new NullPointerException()
    }
    val fn = rsc.getConfiguration.makeSystemFunction(name, arity)
    if (fn == null) {
      rsc.getConfiguration.makeSystemFunction(name, arity)
      throw new IllegalStateException(name)
    }
    fn.setRetainedStaticContext(rsc)
    fn
  }

  def dynamicCall(f: Function,
                  context: XPathContext,
                  args: Array[Sequence]): Sequence = {
    var xPathCont = context
    xPathCont = f.makeNewContext(xPathCont, null)
    xPathCont.setCurrentOutputUri(null)
    f.call(xPathCont, args)
  }

}

abstract class SystemFunction extends AbstractFunction {

  @BeanProperty
  var arity: Int = _

  @BeanProperty
  var details: BuiltInFunctionSet.Entry = _

  @BeanProperty
  var retainedStaticContext: RetainedStaticContext = _

  def makeFunctionCall(arguments: Expression*): Expression = {
    val e: Expression = new SystemFunctionCall(this, arguments.toArray)
    e.setRetainedStaticContext(getRetainedStaticContext)
    e
  }

  def getNetCost: Int = 1

  def makeOptimizedFunctionCall(visitor: ExpressionVisitor,
                                contextInfo: ContextItemStaticInfo,
                                arguments: Expression*): Expression = {
    val opt: Optimizer = visitor.obtainOptimizer()
    if (opt.isOptionSet(OptimizerOptions.CONSTANT_FOLDING)) {
      fixArguments(arguments: _*)
    } else {
      null
    }
  }

  def fixArguments(arguments: Expression*): Expression = {

    for (i <- 0 until getArity) {
      if (Literal.isEmptySequence(arguments(i)) && resultIfEmpty(i) != null) return Literal.makeLiteral(details.resultIfEmpty(i).materialize)
    }
    null
  }

  def resultIfEmpty(arg: Int): Sequence = details.resultIfEmpty(arg)

  def dependsOnContextItem: Boolean =
    (details.properties & (BuiltInFunctionSet.CITEM | BuiltInFunctionSet.CDOC)) != 0

  def getFunctionName: StructuredQName = details.name

  def getDescription: String = details.name.getDisplayName

  override def getOperandRoles: Array[OperandRole] = {
    val roles: Array[OperandRole] = Array.ofDim[OperandRole](getArity)
    val usages: Array[OperandUsage.OperandUsage] = details.usage
    try for (i <- 0 until getArity) {
      roles(i) = new OperandRole(0, usages(i), getRequiredType(i))
    } catch {
      case e: ArrayIndexOutOfBoundsException => e.printStackTrace()

    }
    roles
  }

  def getIntegerBounds: Array[IntegerValue] = null

  def supplyTypeInformation(visitor: ExpressionVisitor,
                            contextItemType: ContextItemStaticInfo,
                            arguments: Array[Expression]): Unit = ()

  override def equals(o: Any): Boolean =
    o.isInstanceOf[SystemFunction] && this == o

  def getErrorCodeForTypeErrors: String = "XPTY0004"

  def getRequiredType(arg: Int): SequenceType = {
    if (details == null) {
      SequenceType.ANY_SEQUENCE
    }
    details.argumentTypes(arg)
  }

  def getResultItemType: ItemType = details.itemType

  def getFunctionItemType: FunctionItemType = {
    val resultType: SequenceType =
      SequenceType.makeSequenceType(getResultItemType, details.cardinality)
    new SpecificFunctionType(details.argumentTypes, resultType)
  }

  def getResultItemType(args: Array[Expression]): ItemType =
    if ((details.properties & BuiltInFunctionSet.AS_ARG0) != 0) {
      args(0).getItemType
    } else if ((details.properties & BuiltInFunctionSet.AS_PRIM_ARG0) !=
      0) {
      args(0).getItemType.getPrimitiveItemType
    } else {
      details.itemType
    }

  def getCardinality(args: Array[Expression]): Int = {
    val c: Int = details.cardinality
    if (c == BuiltInFunctionSet.OPT &&
      (details.properties & BuiltInFunctionSet.CARD0) != 0 &&
      !Cardinality.allowsZero(args(0).getCardinality)) {
      StaticProperty.EXACTLY_ONE
    } else {
      c
    }
  }

  def getSpecialProperties(arguments: Array[Expression]): Int = {
    if ((details.properties & BuiltInFunctionSet.NEW) != 0) {
      StaticProperty.ALL_NODES_NEWLY_CREATED
    }
    var p: Int = StaticProperty.NO_NODES_NEWLY_CREATED
    if ((details.properties & BuiltInFunctionSet.SIDE) != 0) {
      p |= StaticProperty.HAS_SIDE_EFFECTS
    }
    p
  }

   def getContextNode(context: XPathContext): NodeInfo = {
    val item: Item = context.getContextItem
    if (item == null) {
      val err = new XPathException(
        "Context item for " + getFunctionName + "() is absent",
        "XPDY0002")
      err.maybeSetContext(context)
      throw err
    } else if (! item.isInstanceOf[NodeInfo]) {
      val err = new XPathException(
        "Context item for " + getFunctionName + "() is not a node",
        "XPTY0004")
      err.maybeSetContext(context)
      throw err
    } else {
      item.asInstanceOf[NodeInfo]
    }
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("fnRef")
    val qName: StructuredQName = getFunctionName
    val name: String =
      if (qName.hasURI(NamespaceConstant.FN)) qName.getLocalPart
      else qName.getEQName
    out.emitAttribute("name", name)
    out.emitAttribute("arity", getArity.toString)
    if ((getDetails.properties & BuiltInFunctionSet.DEPENDS_ON_STATIC_CONTEXT) !=
      0) {
      out.emitRetainedStaticContext(getRetainedStaticContext, null)
    }
    out.endElement()
  }

  def typeCheckCaller(caller: FunctionCall,
                      visitor: ExpressionVisitor,
                      contextInfo: ContextItemStaticInfo): Expression = caller

  override def isTrustedResultType: Boolean = true

  def getStaticBaseUriString: String =
    getRetainedStaticContext.getStaticBaseUriString

  def exportAttributes(out: ExpressionPresenter): Unit = ()

  def exportAdditionalArguments(call: SystemFunctionCall,
                                out: ExpressionPresenter): Unit = ()

  def importAttributes(attributes: Properties): Unit = ()

  def getCompilerName: String = null

  def getStreamerName: String = null

  override def toShortString: String =
    getFunctionName.getDisplayName + '#' + getArity

  override def toString: String =
    getFunctionName.getDisplayName + '#' + getArity

}
