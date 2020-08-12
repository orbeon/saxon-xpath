
package net.sf.saxon.functions.hof

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser._
import net.sf.saxon.model.FunctionItemType
import net.sf.saxon.model.SpecificFunctionType
import net.sf.saxon.model.UType
import net.sf.saxon.om.Function
import net.sf.saxon.om.Item
import net.sf.saxon.s9api.Location
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.SequenceType
import scala.jdk.CollectionConverters._

/**
 * An FunctionSequenceCoercer is an expression that performs function coercion on a sequence of function items:
 * it takes a sequence of supplied items as input, and wraps each one in a CoercedFunction value, which dynamically
 * converts the supplied arguments to the required type, and converts the result in the opposite direction, or
 * throws a type error if conversion is not possible.
 */
object FunctionSequenceCoercer {
  @throws[XPathException]
  private def checkAnnotations(item: Function, requiredItemType: FunctionItemType, config: Configuration) = {

    for (ann <- requiredItemType.getAnnotationAssertions.asScala) {
      val handler = config.getFunctionAnnotationHandler(ann.getAnnotationQName.getURI)
      if (handler != null && !handler.satisfiesAssertion(ann, item.getAnnotations))
        throw new XPathException("Supplied function does not satisfy the annotation assertions of the required function type", "XPTY0004")
    }
  }

  class Coercer(var requiredItemType: SpecificFunctionType, var config: Configuration, var locator: Location) extends ItemMappingFunction {
    @throws[XPathException]
    override def mapItem(item: Item) = {
      if (!item.isInstanceOf[Function])
        throw new XPathException("Function coercion attempted on an item which is not a function", "XPTY0004", locator)
      try {
        checkAnnotations(item.asInstanceOf[Function], requiredItemType, config)
        new CoercedFunction(item.asInstanceOf[Function], requiredItemType)
      } catch {
        case err: XPathException =>
          err.maybeSetLocation(locator)
          throw err
      }
    }
  }

}

final class FunctionSequenceCoercer(val sequence: Expression, var requiredItemType: SpecificFunctionType, var role: RoleDiagnostic)

/**
 * Constructor
 *
 * @param sequence         this must be a sequence of function item values. This is not checked; a ClassCastException
 *                         will occur if the precondition is not satisfied.
 * @param requiredItemType the function item type to which all items in the sequence should be converted,
 */
  extends UnaryExpression(sequence) {
  ExpressionTool.copyLocationInfo(sequence, this)

  override  def getOperandRole = OperandRole.INSPECT

  @throws[XPathException]
  override def simplify: Expression = {
    setBaseExpression(getBaseExpression.simplify)
    if (getBaseExpression.isInstanceOf[Literal]) {
      val `val` = iterate(new EarlyEvaluationContext(getConfiguration)).materialize
      return Literal.makeLiteral(`val`, this)
    }
    this
  }

  /**
   * Type-check the expression
   */
  @throws[XPathException]
  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo) = {
    getOperand.typeCheck(visitor, contextInfo)
    val th = visitor.getConfiguration.getTypeHierarchy
    if (th.isSubType(getBaseExpression.getItemType, requiredItemType)) getBaseExpression
    else this
  }

  /**
   * Determine the special properties of this expression
   *
   * @return { @link net.sf.saxon.expr.StaticProperty#NO_NODES_NEWLY_CREATED}.
   */
  override def computeSpecialProperties = {
    val p = super.computeSpecialProperties
    p | StaticProperty.NO_NODES_NEWLY_CREATED
  }

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   * @param rebindings variables that need to be re-bound
   */
  override def copy(rebindings: RebindingMap) = {
    val fsc2 = new FunctionSequenceCoercer(getBaseExpression.copy(rebindings), requiredItemType, role)
    ExpressionTool.copyLocationInfo(this, fsc2)
    fsc2
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided directly. The other methods will always be available
   * indirectly, using an implementation that relies on one of the other methods.
   *
   * @return the implementation method, for example { @link #ITERATE_METHOD} or { @link #EVALUATE_METHOD} or
   *         { @link #PROCESS_METHOD}
   */
  override def getImplementationMethod = Expression.ITERATE_METHOD

  /**
   * Iterate over the sequence of functions, wrapping each one in a CoercedFunction object
   */
  @throws[XPathException]
  override def iterate(context: XPathContext) = {
    val base = getBaseExpression.iterate(context)
    val coercer = new FunctionSequenceCoercer.Coercer(requiredItemType, context.getConfiguration, getLocation)
    new ItemMappingIterator(base, coercer, true)
  }

  /**
   * Evaluate as an Item. This should only be called if the FunctionSequenceCoercer has cardinality zero-or-one
   */
  /*@Nullable*/ @throws[XPathException]
  override def evaluateItem(context: XPathContext): Function = {
    val item = getBaseExpression.evaluateItem(context)
    if (item == null) return null
    if (!item.isInstanceOf[Function]) {
      val itemType = UType.getUType(item)
      throw new XPathException(role.composeErrorMessage(requiredItemType, itemType), "XPTY0004")
    }
    try FunctionSequenceCoercer.checkAnnotations(item.asInstanceOf[Function], requiredItemType, context.getConfiguration)
    catch {
      case err: XPathException =>
        err.maybeSetLocation(getLocation)
        err.maybeSetContext(context)
        throw err
    }
    new CoercedFunction(item.asInstanceOf[Function], requiredItemType)
  }

  /**
   * Determine the data type of the items returned by the expression, if possible
   *
   * @return a value such as Type.STRING, Type.BOOLEAN, Type.NUMBER, Type.NODE,
   *         or Type.ITEM (meaning not known in advance)
   */
  override def getItemType = requiredItemType

  /**
   * Determine the static cardinality of the expression
   */
  override def computeCardinality = getBaseExpression.getCardinality

  /**
   * @return the role locator
   */
  def getRole = role

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any) = super.equals(other) && requiredItemType == other.asInstanceOf[FunctionSequenceCoercer].requiredItemType

  override def computeHashCode = super.computeHashCode ^ requiredItemType.hashCode

  override def getExpressionName = "fnCoercer"

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   */
  @throws[XPathException]
  override def `export`(destination: ExpressionPresenter) = {
    destination.startElement("fnCoercer", this)
    val st = SequenceType.makeSequenceType(requiredItemType, StaticProperty.EXACTLY_ONE)
    destination.emitAttribute("to", st.toAlphaCode)
    destination.emitAttribute("diag", role.save)
    getBaseExpression.`export`(destination)
    destination.endElement
  }
}

// Copyright (c) 2009-2020 Saxonica Limited