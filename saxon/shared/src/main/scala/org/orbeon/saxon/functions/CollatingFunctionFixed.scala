package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.StaticContext

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.RetainedStaticContext

import org.orbeon.saxon.expr.sort.AtomicComparer

import org.orbeon.saxon.expr.sort.EqualityComparer

import org.orbeon.saxon.expr.sort.GenericAtomicComparer

import org.orbeon.saxon.expr.sort.SimpleCollation

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.lib.SubstringMatcher

import org.orbeon.saxon.model.AtomicType

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.ErrorType

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import java.util.Properties

import scala.beans.{BeanProperty, BooleanBeanProperty}

abstract class CollatingFunctionFixed
  extends SystemFunction
    with StatefulSystemFunction {

  private var collationName: String = _

  @BeanProperty
  var stringCollator: StringCollator = null

  private var atomicComparer: AtomicComparer = null

  def isSubstringMatchingFunction: Boolean = false

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

  def getPreAllocatedAtomicComparer: AtomicComparer = atomicComparer

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
