package net.sf.saxon.functions

import net.sf.saxon.expr.StaticContext

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.RetainedStaticContext

import net.sf.saxon.expr.sort.AtomicComparer

import net.sf.saxon.expr.sort.EqualityComparer

import net.sf.saxon.expr.sort.GenericAtomicComparer

import net.sf.saxon.expr.sort.SimpleCollation

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.lib.SubstringMatcher

import net.sf.saxon.model.AtomicType

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ErrorType

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import java.util.Properties

import scala.beans.{BeanProperty, BooleanBeanProperty}

abstract class CollatingFunctionFixed
  extends SystemFunction
    with StatefulSystemFunction {

  private var collationName: String = _

  @BeanProperty
  var stringCollator: StringCollator = null

  private var atomicComparer: AtomicComparer = null

  def isSubstringMatchingFunction(): Boolean = false

  override def setRetainedStaticContext(retainedStaticContext: RetainedStaticContext): Unit = {
    super.setRetainedStaticContext(retainedStaticContext)
    if (collationName == null) {
      collationName = retainedStaticContext.getDefaultCollationName
      try allocateCollator()
      catch {
        case e: XPathException => {}
      }
    }
  }

  def setCollationName(collationName: String): Unit = {
    this.collationName = collationName
    allocateCollator()
  }

  private def allocateCollator(): Unit = {
    stringCollator =
      getRetainedStaticContext.getConfiguration.getCollation(collationName)
    if (stringCollator == null) {
      throw new XPathException("Unknown collation " + collationName,
        "FOCH0002")
    }
    if (isSubstringMatchingFunction) {
      if (stringCollator.isInstanceOf[SimpleCollation]) {
        stringCollator =
          stringCollator.asInstanceOf[SimpleCollation].getSubstringMatcher
      }
      if (!(stringCollator.isInstanceOf[SubstringMatcher])) {
        throw new XPathException(
          "The collation requested for " + getFunctionName.getDisplayName +
            " does not support substring matching",
          "FOCH0004")
      }
    }
  }

   def preAllocateComparer(type0: AtomicType,
                                    type1: AtomicType,
                                    env: StaticContext): Unit = {
    val collation: StringCollator = getStringCollator
    if (type0 == ErrorType || type1 == ErrorType) {
      atomicComparer = EqualityComparer.getInstance
      return
    }
    atomicComparer = GenericAtomicComparer.makeAtomicComparer(
      type0.getBuiltInBaseType.asInstanceOf[BuiltInAtomicType],
      type1.getBuiltInBaseType.asInstanceOf[BuiltInAtomicType],
      stringCollator,
      env.makeEarlyEvaluationContext()
    )
  }

  def getPreAllocatedAtomicComparer(): AtomicComparer = atomicComparer

  def getAtomicComparer(context: XPathContext): AtomicComparer =
    if (atomicComparer != null) {
      atomicComparer.provideContext(context)
    } else {
      new GenericAtomicComparer(getStringCollator, context)
    }

  override def exportAttributes(out: ExpressionPresenter): Unit = {
    if (collationName != NamespaceConstant.CODEPOINT_COLLATION_URI) {
      out.emitAttribute("collation", collationName)
    }
  }

  override def importAttributes(attributes: Properties): Unit = {
    val collationName: String = attributes.getProperty("collation")
    if (collationName != null) {
      this.collationName = collationName
    }
  }

  override def copy(): CollatingFunctionFixed = {
    var copy: SystemFunction = SystemFunction.makeFunction(
      getFunctionName.getLocalPart,
      getRetainedStaticContext,
      getArity)
    if (copy.isInstanceOf[CollatingFunctionFree]) {
      copy =
        copy.asInstanceOf[CollatingFunctionFree].bindCollation(collationName)
    }
    if (copy.isInstanceOf[CollatingFunctionFixed]) {
      copy.asInstanceOf[CollatingFunctionFixed].collationName = collationName
      copy.asInstanceOf[CollatingFunctionFixed].atomicComparer = atomicComparer
      copy.asInstanceOf[CollatingFunctionFixed].stringCollator = stringCollator
      copy.asInstanceOf[CollatingFunctionFixed]
    }
    throw new IllegalStateException()
  }

}
