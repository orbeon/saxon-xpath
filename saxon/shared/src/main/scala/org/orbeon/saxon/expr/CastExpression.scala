////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.String_1
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value._

/**
 * Cast Expression: implements "cast as data-type ( expression )". It also allows an internal
 * cast, which has the same semantics as a user-requested cast, but maps an empty sequence to
 * an empty sequence.
 * <p>This expression class does not handle casting to a list or union type.</p>
 */
class CastExpression(val source: Expression, val target: AtomicType, val allowEmpty: Boolean)

/**
 * Create a cast expression
 *
 * @param source     expression giving the value to be converted
 * @param target     the type to which the value is to be converted
 * @param allowEmpty true if the expression allows an empty sequence as input, producing
 *                   an empty sequence as output. If false, an empty sequence is a type error.
 */
  extends CastingExpression(source, target, allowEmpty) with Callable {
  /**
   * Type-check the expression
   */
  /*@NotNull*/ @throws[XPathException]
  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    val atomicType = SequenceType.makeSequenceType(BuiltInAtomicType.ANY_ATOMIC, getCardinality)
    val config = visitor.getConfiguration
    val th = config.getTypeHierarchy
    val role = new RoleDiagnostic(RoleDiagnostic.TYPE_OP, "cast as", 0)
    var sourceItemType: ItemType = null
    val tc = config.getTypeChecker(false)
    val operand = tc.staticTypeCheck(getBaseExpression, atomicType, role, visitor)
    setBaseExpression(operand)

    sourceItemType = operand.getItemType
    if (sourceItemType eq ErrorType)
      if (allowsEmpty)
        return Literal.makeEmptySequence
      else {
        val err = new XPathException("Cast does not allow an empty sequence as input")
        err.setErrorCode("XPTY0004")
        err.setLocation(getLocation)
        err.setIsTypeError(true)
        throw err
      }

    val sourceType = sourceItemType.asInstanceOf[PlainType]
    val r = th.relationship(sourceType, getTargetType)
    if (r eq Affinity.SAME_TYPE) return operand
    else if (r eq Affinity.SUBSUMED_BY) { // It's generally true that any expression defined to return an X is allowed to return a subtype of X.
      // However, people seem to get upset if we treat the cast as a no-op.
      converter = new Converter.UpCastingConverter(getTargetType)
    }
    else {
      val rules = visitor.getConfiguration.getConversionRules
      if (sourceType.isAtomicType && (sourceType ne BuiltInAtomicType.ANY_ATOMIC)) {
        converter = rules.getConverter(sourceType.asInstanceOf[AtomicType], getTargetType)
        if (converter == null) {
          val err = new XPathException("Casting from " + sourceType + " to " + getTargetType + " can never succeed")
          err.setErrorCode("XPTY0004")
          err.setLocation(getLocation)
          err.setIsTypeError(true)
          throw err
        }
        else if (getTargetType.isNamespaceSensitive) converter = converter.setNamespaceResolver(getRetainedStaticContext)
      }
    }
    if (operand.isInstanceOf[Literal]) return preEvaluate
    this
  }

  /**
   * Perform optimisation of an expression and its subexpressions.
   * <p>This method is called after all references to functions and variables have been resolved
   * to the declaration of the function or variable, and after all type checking has been done.</p>
   *
   * @param visitor     an expression visitor
   * @param contextInfo the static type of "." at the point where this expression is invoked.
   *                    The parameter is set to null if it is known statically that the context item will be undefined.
   *                    If the type of the context item is not known statically, the argument is set to
   *                    { @link org.orbeon.saxon.model.Type#ITEM_TYPE}
   * @return the original expression, rewritten if appropriate to optimize execution
   * @throws XPathException
   * if an error is discovered during this phase
   * (typically a type error)
   */
  @throws[XPathException]
  override def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    val th = visitor.getConfiguration.getTypeHierarchy
    val e2 = super.optimize(visitor, contextInfo)
    if (e2 ne this) return e2
    // Eliminate pointless casting between untypedAtomic and string
    var operand = getBaseExpression
    if (getTargetType eq BuiltInAtomicType.UNTYPED_ATOMIC) if (operand.isCallOn(classOf[String_1])) {
      val e = operand.asInstanceOf[SystemFunctionCall].getArg(0)
      if (e.getItemType.isInstanceOf[AtomicType] && e.getCardinality == StaticProperty.EXACTLY_ONE) operand = e
    }
    else if (operand.isInstanceOf[CastExpression]) if (operand.asInstanceOf[CastExpression].getTargetType eq BuiltInAtomicType.UNTYPED_ATOMIC) return operand
    else if (operand.asInstanceOf[CastExpression].getTargetType eq BuiltInAtomicType.STRING) {
      operand.asInstanceOf[CastExpression].setTargetType(BuiltInAtomicType.UNTYPED_ATOMIC)
      return operand
    }
    else if (operand.isInstanceOf[AtomicSequenceConverter]) if (operand.getItemType eq BuiltInAtomicType.UNTYPED_ATOMIC) return operand
    else if (operand.getItemType eq BuiltInAtomicType.STRING) {
      val old = operand.asInstanceOf[AtomicSequenceConverter]
      val asc = new AtomicSequenceConverter(old.getBaseExpression, BuiltInAtomicType.UNTYPED_ATOMIC)
      return asc.typeCheck(visitor, contextInfo).optimize(visitor, contextInfo)
    }
    // avoid converting anything to a string and then back again
    if (operand.isCallOn(classOf[String_1])) {
      val e = operand.asInstanceOf[SystemFunctionCall].getArg(0)
      val et = e.getItemType
      if (et.isInstanceOf[AtomicType] && e.getCardinality == StaticProperty.EXACTLY_ONE && th.isSubType(et, getTargetType)) return e
    }
    // avoid converting anything to untypedAtomic and then back again
    if (operand.isInstanceOf[CastExpression]) {
      val it = operand.asInstanceOf[CastExpression].getTargetType
      if (th.isSubType(it, BuiltInAtomicType.STRING) || th.isSubType(it, BuiltInAtomicType.UNTYPED_ATOMIC)) {
        val e = operand.asInstanceOf[CastExpression].getBaseExpression
        val et = e.getItemType
        if (et.isInstanceOf[AtomicType] && e.getCardinality == StaticProperty.EXACTLY_ONE && th.isSubType(et, getTargetType)) return e
      }
    }
    if (operand.isInstanceOf[AtomicSequenceConverter]) {
      val it = operand.getItemType
      if (th.isSubType(it, BuiltInAtomicType.STRING) || th.isSubType(it, BuiltInAtomicType.UNTYPED_ATOMIC)) {
        val e = operand.asInstanceOf[AtomicSequenceConverter].getBaseExpression
        val et = e.getItemType
        if (et.isInstanceOf[AtomicType] && e.getCardinality == StaticProperty.EXACTLY_ONE && th.isSubType(et, getTargetType)) return e
      }
    }
    // if the operand can't be empty, then set allowEmpty to false to provide more information for analysis
    if (!Cardinality.allowsZero(operand.getCardinality)) {
      setAllowEmpty(false)
      resetLocalStaticProperties()
    }
    if (operand.isInstanceOf[Literal]) return preEvaluate
    this
  }

  /**
   * Perform early (compile-time) evaluation, if possible
   *
   * @return the result of pre-evaluation, or the original expression unchanged
   * @throws XPathException if an error is found
   */
  @throws[XPathException]
   def preEvaluate: Expression = {
    val literalOperand = getBaseExpression.asInstanceOf[Literal].getValue
    if (literalOperand.isInstanceOf[AtomicValue] && converter != null) {
      val result = converter.convert(literalOperand.asInstanceOf[AtomicValue])
      if (result.isInstanceOf[ValidationFailure]) {
        val err = result.asInstanceOf[ValidationFailure]
        var code = err.getErrorCode
        if (code == null) code = "FORG0001"
        throw new XPathException(err.getMessage, code, this.getLocation)
      }
      else return Literal.makeLiteral(result.asInstanceOf[AtomicValue], this)
    }
    if (literalOperand.getLength == 0) if (allowsEmpty) return getBaseExpression
    else {
      val err = new XPathException("Cast can never succeed: the operand must not be an empty sequence", "XPTY0004", this.getLocation)
      err.setIsTypeError(true)
      throw err
    }
    this
  }

  /**
   * Get the static cardinality of the expression
   */
  override def computeCardinality = if (allowsEmpty && Cardinality.allowsZero(getBaseExpression.getCardinality)) StaticProperty.ALLOWS_ZERO_OR_ONE
  else StaticProperty.EXACTLY_ONE

  /**
   * Get the static type of the expression
   */
  override def getItemType = getTargetType

  /**
   * Get the static type of the expression as a UType, following precisely the type
   * inference rules defined in the XSLT 3.0 specification.
   *
   * @return the static item type of the expression according to the XSLT 3.0 defined rules
   * @param contextItemType static information about the context item
   */
  override def getStaticUType(contextItemType: UType) = getTargetType.getUType

  /**
   * Determine the special properties of this expression
   *
   * @return the expression properties
   */
  override def computeSpecialProperties = {
    var p = super.computeSpecialProperties
    if (getTargetType eq BuiltInAtomicType.UNTYPED_ATOMIC) p = p & ~StaticProperty.NOT_UNTYPED_ATOMIC
    p
  }

  /**
   * For an expression that returns an integer or a sequence of integers, get
   * a lower and upper bound on the values of the integers that may be returned, from
   * static analysis. The default implementation returns null, meaning "unknown" or
   * "not applicable". Other implementations return an array of two IntegerValue objects,
   * representing the lower and upper bounds respectively. The values
   * UNBOUNDED_LOWER and UNBOUNDED_UPPER are used by convention to indicate that
   * the value may be arbitrarily large. The values MAX_STRING_LENGTH and MAX_SEQUENCE_LENGTH
   * are used to indicate values limited by the size of a string or the size of a sequence.
   *
   * @return the lower and upper bounds of integer values in the result, or null to indicate
   *         unknown or not applicable.
   */
  /*@Nullable*/ override def getIntegerBounds = if (converter eq Converter.BooleanToInteger.INSTANCE) Array[IntegerValue](Int64Value.ZERO, Int64Value.PLUS_ONE)
  else null

  /**
   * Copy an expression. This makes a deep copy.
   *
   * @return the copy of the original expression
   * @param rebindings variables that need to be rebound
   */
  override def copy(rebindings: RebindingMap) = {
    val c2 = new CastExpression(getBaseExpression.copy(rebindings), getTargetType, allowsEmpty)
    ExpressionTool.copyLocationInfo(this, c2)
    c2.converter = converter
    c2.setRetainedStaticContext(getRetainedStaticContext)
    c2.setOperandIsStringLiteral(isOperandIsStringLiteral)
    c2
  }

  /**
   * An implementation of Expression must provide at least one of the methods evaluateItem(), iterate(), or process().
   * This method indicates which of these methods is provided directly. The other methods will always be available
   * indirectly, using an implementation that relies on one of the other methods.
   *
   * @return the implementation method, for example { @link #ITERATE_METHOD} or { @link #EVALUATE_METHOD} or
   *                                                        { @link #PROCESS_METHOD}
   */
  override def getImplementationMethod = Expression.EVALUATE_METHOD

  @throws[XPathException]
  override def call(context: XPathContext, arguments: Array[Sequence]) = {
    val result = doCast(arguments(0).head.asInstanceOf[AtomicValue], context)
    if (result == null) EmptySequence.getInstance
    else result
  }

  @throws[XPathException]
  private def doCast(value: AtomicValue, context: XPathContext): AtomicValue = {
    if (value == null)
      if (allowsEmpty) {
        return null
      } else {
        val e = new XPathException("Cast does not allow an empty sequence")
        e.setXPathContext(context)
        e.setLocation(getLocation)
        e.setErrorCode("XPTY0004")
        throw e
      }
    var converter = this.converter
    if (converter == null) {
      val rules = context.getConfiguration.getConversionRules
      converter = rules.getConverter(value.getPrimitiveType, getTargetType)
      if (converter == null) {
        val e = new XPathException("Casting from " + value.getPrimitiveType + " to " + getTargetType + " is not permitted")
        e.setXPathContext(context)
        e.setLocation(getLocation)
        e.setErrorCode("XPTY0004")
        throw e
      }
      if (getTargetType.isNamespaceSensitive)
        converter = converter.setNamespaceResolver(getRetainedStaticContext)
    }
    converter.convert(value) match {
      case err: ValidationFailure =>
        val xe = err.makeException
        xe.maybeSetErrorCode("FORG0001")
        xe.maybeSetLocation(getLocation)
        throw xe
      case result =>
        result.asInstanceOf[AtomicValue]
    }
  }

  /**
   * Evaluate the expression
   */
  @throws[XPathException]
  override def evaluateItem(context: XPathContext) = try {
    val value = getBaseExpression.evaluateItem(context).asInstanceOf[AtomicValue]
    doCast(value, context)
  } catch {
    case e: ClassCastException =>
      e.printStackTrace()
      throw e
  }

  /**
   * Is this expression the same as another expression?
   */
  override def equals(other: Any) = other.isInstanceOf[CastExpression] && getBaseExpression.isEqual(other.asInstanceOf[CastExpression].getBaseExpression) && (getTargetType eq other.asInstanceOf[CastExpression].getTargetType) && allowsEmpty == other.asInstanceOf[CastExpression].allowsEmpty

  /**
   * get HashCode for comparing two expressions. Note that this hashcode gives the same
   * result for (A op B) and for (B op A), whether or not the operator is commutative.
   */
  override def computeHashCode = super.computeHashCode ^ getTargetType.hashCode

  /**
   * Represent the expression as a string. The resulting string will be a valid XPath 3.0 expression
   * with no dependencies on namespace bindings.
   *
   * @return the expression as a string in XPath 3.0 syntax
   */
  override def toString = getTargetType.getEQName + "(" + getBaseExpression.toString + ")"

  override def toShortString = getTargetType.getDisplayName + "(" + getBaseExpression.toShortString + ")"

  /**
   * Diagnostic print of expression structure. The abstract expression tree
   * is written to the supplied output destination.
   */
  @throws[XPathException]
  override def `export`(out: ExpressionPresenter) = `export`(out, "cast")

  /**
   * Get a name identifying the kind of expression, in terms meaningful to a user.
   *
   * @return a name identifying the kind of expression, in terms meaningful to a user.
   *         The name will always be in the form of a lexical XML QName, and should match the name used
   *         in export() output displaying the expression.
   */
  override def getExpressionName = "cast"
}