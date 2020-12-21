package org.orbeon.saxon.functions

import java.util.Properties

import org.orbeon.saxon.expr.parser.RetainedStaticContext
import org.orbeon.saxon.expr.sort.{AtomicComparer, EqualityComparer, GenericAtomicComparer}
import org.orbeon.saxon.expr.{StaticContext, XPathContext}
import org.orbeon.saxon.lib.{NamespaceConstant, StringCollator, SubstringMatcher}
import org.orbeon.saxon.model.{AtomicType, BuiltInAtomicType, ErrorType}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException

import scala.beans.BeanProperty

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
      // ORBEON: Collations
//      stringCollator match {
//        case simpleCollation: SimpleCollation =>
//          stringCollator = simpleCollation.getSubstringMatcher
//        case _ =>
//      }
      if (! stringCollator.isInstanceOf[SubstringMatcher]) {
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
    copy match {
      case free: CollatingFunctionFree =>
        copy = free.bindCollation(collationName)
      case _ =>
    }
    copy match {
      case fixed: CollatingFunctionFixed =>
        fixed.collationName = collationName
        fixed.atomicComparer = atomicComparer
        fixed.stringCollator = stringCollator
        fixed
      case _ =>
    }
    throw new IllegalStateException()
  }

}
