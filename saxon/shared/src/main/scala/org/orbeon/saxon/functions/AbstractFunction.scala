package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.ContextOriginator
import org.orbeon.saxon.expr.OperandRole
import org.orbeon.saxon.expr.OperandUsage
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.parser.ContextItemStaticInfo
import org.orbeon.saxon.expr.parser.ExpressionVisitor
import org.orbeon.saxon.expr.sort.AtomicComparer
import org.orbeon.saxon.om.AtomicSequence
import org.orbeon.saxon.om.Function
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import java.util.Arrays

import org.orbeon.saxon.query.AnnotationList

abstract class AbstractFunction extends Function {

  def getOperandRoles: Array[OperandRole] = {
    val roles: Array[OperandRole] = Array.ofDim[OperandRole](getArity)
    Arrays.fill(roles.asInstanceOf[Array[AnyRef]], new OperandRole(0, OperandUsage.NAVIGATION).asInstanceOf[AnyRef])
    roles
  }

  def atomize(): AtomicSequence =
    throw new XPathException("Function items (other than arrays) cannot be atomized", "FOTY0013")

  def isArray: Boolean = false
  def isMap  : Boolean = false

  def getStringValue: String =
    throw new UnsupportedOperationException("The string value of a function is not defined")

  def getStringValueCS: CharSequence =
    throw new UnsupportedOperationException("The string value of a function is not defined")

  override def getAnnotations: AnnotationList = AnnotationList.EMPTY

 override def effectiveBooleanValue: Boolean =
    throw new XPathException("A function has no effective boolean value", "XPTY0004")

  def simplify(): Unit = ()

  def typeCheck(visitor: ExpressionVisitor,
                contextItemType: ContextItemStaticInfo): Unit = ()

  def makeNewContext(callingContext: XPathContext,
                     originator: ContextOriginator): XPathContext =
    callingContext

  def deepEquals(
    other   : Function,
    context : XPathContext,
    comparer: AtomicComparer,
    flags   : Int
  ): Boolean = throw new XPathException(
    "Argument to deep-equal() contains a function item", "FOTY0015"
  )

  def export(
    out: ExpressionPresenter
  ): Unit = throw new UnsupportedOperationException("export() not implemented for " + this.getClass)

  def isTrustedResultType: Boolean = false
}
