package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser._

import org.orbeon.saxon.model.AnyFunctionType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.SpecificFunctionType

import org.orbeon.saxon.om.Function

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trace.ExpressionPresenter

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.SequenceType

import java.util.ArrayList

import java.util.List

class PartialApply(base: Expression, boundArguments: Array[Expression])
  extends Expression {

  private val baseOp: Operand = new Operand(this, base, OperandRole.INSPECT)

  // contains null where the question marks appear
  private val boundArgumentsOp: Array[Operand] =
    new Array[Operand](boundArguments.length)

  adoptChildExpression(base)

  for (i <- boundArguments.indices if boundArguments(i) != null) {
    boundArgumentsOp(i) = new Operand(this, boundArguments(i), OperandRole.NAVIGATE)
    adoptChildExpression(boundArguments(i))
  }

  def getBaseExpression: Expression = baseOp.getChildExpression

  def setBaseExpression(base: Expression): Unit = {
    baseOp.setChildExpression(base)
  }

  def getNumberOfPlaceHolders: Int = {
    var n: Int = 0
    for (o <- boundArgumentsOp if o == null) {
      {
        n += 1
        n - 1
      }
    }
    n
  }

  /*@NotNull*/
  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    val baseType: ItemType = getBaseExpression.getItemType
    var requiredFunctionType: SequenceType = null
    val argTypes: Array[SequenceType] = Array.fill[SequenceType](boundArgumentsOp.length)(SequenceType.ANY_SEQUENCE)

    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(false)
    for (i <- 0 until boundArgumentsOp.length) {
      val op: Operand = boundArgumentsOp(i)
      if (op != null) {
        val arg: Expression = op.getChildExpression
        if (baseType.isInstanceOf[SpecificFunctionType] &&
          i <
            baseType.asInstanceOf[SpecificFunctionType].getArity) {
          val role: RoleDiagnostic =
            new RoleDiagnostic(RoleDiagnostic.FUNCTION, "saxon:call", i)
          val requiredArgType: SequenceType =
            baseType.asInstanceOf[SpecificFunctionType].getArgumentTypes(i)
          argTypes(i) = requiredArgType
          val a3: Expression =
            tc.staticTypeCheck(arg, requiredArgType, role, visitor)
          if (a3 != arg) {
            op.setChildExpression(a3)
          }
        }
      }
    }
    requiredFunctionType = SequenceType.makeSequenceType(
      new SpecificFunctionType(
        argTypes,
        if ((baseType.isInstanceOf[AnyFunctionType]))
          baseType.asInstanceOf[AnyFunctionType].getResultType
        else SequenceType.ANY_SEQUENCE),
      StaticProperty.EXACTLY_ONE
    )
    val role: RoleDiagnostic =
      new RoleDiagnostic(RoleDiagnostic.FUNCTION, "saxon:call", 0)
    setBaseExpression(tc.staticTypeCheck(getBaseExpression,
      requiredFunctionType,
      role,
      visitor))
    this
  }

  /*@NotNull*/

  override def getItemType: ItemType = {
    val baseItemType: ItemType = getBaseExpression.getItemType
    var resultType: SequenceType = SequenceType.ANY_SEQUENCE
    if (baseItemType.isInstanceOf[SpecificFunctionType]) {
      resultType =
        baseItemType.asInstanceOf[SpecificFunctionType].getResultType
    }
    val placeholders: Int = getNumberOfPlaceHolders
    var argTypes: Array[SequenceType] = Array.ofDim[SequenceType](placeholders)
    if (baseItemType.isInstanceOf[SpecificFunctionType]) {
      var j = 0
      for (i <- 0 until boundArgumentsOp.length
           if boundArgumentsOp(i) == null) {
        argTypes({
          j
        }) =
          baseItemType.asInstanceOf[SpecificFunctionType].getArgumentTypes(i)
        j = j + 1
      }
    } else {
      argTypes = Array.fill[SequenceType](placeholders)(SequenceType.ANY_SEQUENCE)
    }
    new SpecificFunctionType(argTypes, resultType)
  }

  override def operands: java.lang.Iterable[Operand] = {
    val operanda: List[Operand] =
      new ArrayList[Operand](boundArgumentsOp.length + 1)
    operanda.add(baseOp)
    for (o <- boundArgumentsOp if o != null) {
      operanda.add(o)
    }
    operanda
  }

  def getNumberOfArguments: Int = boundArgumentsOp.length

  def getArgument(n: Int): Expression = {
    val o: Operand = boundArgumentsOp(n)
    if (o == null) null else o.getChildExpression
  }

  override def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  /**
   * Is this expression the same as another expression?
   *
   * @param other the expression to be compared with this one
   * @return true if the two expressions are statically equivalent
   */
  override def equals(other: Any): Boolean =
    if (!(other.isInstanceOf[PartialApply])) {
      false
    } else {
      val pa2: PartialApply = other.asInstanceOf[PartialApply]
      if (!getBaseExpression.isEqual(pa2.getBaseExpression)) {
        return false
      }
      if (boundArgumentsOp.length != pa2.boundArgumentsOp.length) {
        return false
      }
      for (i <- 0 until boundArgumentsOp.length) {
        if ((boundArgumentsOp(i) == null) != (pa2.boundArgumentsOp(i) == null)) {
          return false
        }
        if (boundArgumentsOp(i) != null && boundArgumentsOp(i) != pa2
          .boundArgumentsOp(i)) {
          false
        }
      }
      true
    }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("partialApply", this)
    getBaseExpression.export(out)
    for (o <- boundArgumentsOp) {
      if (o == null) {
        out.startElement("null", this)
        out.endElement()
      } else {
        o.getChildExpression.export(out)
      }
    }
    out.endElement()
  }

   override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  /*@NotNull*/

  override def copy(rebindings: RebindingMap): Expression = {
    val boundArgumentsCopy: Array[Expression] =
      Array.ofDim[Expression](boundArgumentsOp.length)
    for (i <- 0 until boundArgumentsOp.length) {
      boundArgumentsCopy(i) =
        if (boundArgumentsOp(i) == null) null
        else boundArgumentsOp(i).getChildExpression.copy(rebindings)
    }
    val exp: PartialApply =
      new PartialApply(getBaseExpression.copy(rebindings), boundArgumentsCopy)
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  /**
   * <p>The toString() method for an expression attempts to give a representation of the expression
   * in an XPath-like form.</p>
   * <p>For subclasses of Expression that represent XPath expressions, the result should always be a string that
   * parses as an XPath 3.0 expression. The expression produced should be equivalent to the original making certain
   * assumptions about the static context. In general the expansion will make no assumptions about namespace bindings,
   * except that (a) the prefix "xs" is used to refer to the XML Schema namespace, and (b) the default funcion namespace
   * is assumed to be the "fn" namespace.</p>
   * <p>In the case of XSLT instructions and XQuery expressions, the toString() method gives an abstracted view of the syntax
   * that is not designed in general to be parseable.</p>
   *
   * @return a representation of the expression as a string
   */
  override def toString: String = {
    val buff = new FastStringBuffer(FastStringBuffer.C64)
    val par: Boolean = getBaseExpression.operands.iterator.hasNext
    if (par) {
      buff.append("(" + getBaseExpression.toString + ")")
    } else {
      buff.append(getBaseExpression.toString)
    }
    buff.append("(")
    for (i <- 0 until boundArgumentsOp.length) {
      if (boundArgumentsOp(i) == null) {
        buff.append("?")
      } else {
        buff.append(boundArgumentsOp(i).getChildExpression.toString)
      }
      if (i != boundArgumentsOp.length - 1) {
        buff.append(", ")
      }
    }
    buff.append(")")
    buff.toString
  }

  override def evaluateItem(context: XPathContext): Function = {
    val f: Function =
      getBaseExpression.evaluateItem(context).asInstanceOf[Function]
    assert(f != null)
    val values: Array[Sequence] =
      Array.ofDim[Sequence](boundArgumentsOp.length)
    for (i <- 0 until boundArgumentsOp.length) {
      values(i) =
        if (boundArgumentsOp(i) == null) null
        else
          boundArgumentsOp(i).getChildExpression.iterate(context).materialize
    }
    new CurriedFunction(f, values)
  }

  /**
   * Get a name identifying the kind of expression, in terms meaningful to a user.
   *
   * @return a name identifying the kind of expression, in terms meaningful to a user.
   *         The name will always be in the form of a lexical XML QName, and should match the name used
   *         in export() output displaying the expression.
   */
  override def getExpressionName: String = "partialApply"


  /**
   * Hashcode supporting equals()
   */
  override def computeHashCode = {
    var h = 0x836b92a0
    var i = 0
    for (o <- operands.asScala) {
      if (o == null) {
        h ^= i + 1
      }
      else
        h ^= o.getChildExpression.hashCode
    }
    h
  }
}

