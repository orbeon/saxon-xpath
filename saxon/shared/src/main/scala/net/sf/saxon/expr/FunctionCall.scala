package net.sf.saxon.expr

import net.sf.saxon.expr.oper.OperandArray

import net.sf.saxon.expr.parser._

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.JavaExternalObjectType

import net.sf.saxon.om.Function

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.NoDynamicContextException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.SequenceType

import java.util.Collections

import FunctionCall._

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object FunctionCall {

  def pluralArguments(num: Int): String =
    if (num == 1) "one argument" else s"$num arguments"

}

abstract class FunctionCall extends Expression {

  @BeanProperty
  var operanda: OperandArray = _

  override def operands(): java.lang.Iterable[Operand] =
    if (operanda != null) {
      operanda.operands()
    } else {
      Collections.emptyList()
    }

  def getTargetFunction(context: XPathContext): Function

  def getFunctionName(): StructuredQName

  def getArity(): Int = getOperanda.getNumberOfOperands

  def setArguments(args: Array[Expression]): Unit = {
    this.operanda = new OperandArray(this, args)
  }

  def setOperanda(args: Array[Expression],
                  roles: Array[OperandRole]): Unit = {
    this.operanda = new OperandArray(this, args, roles)
  }

  def getArguments(): Array[Expression] = {
    val result: Array[Expression] = Array.ofDim[Expression](getArity)
    var i: Int = 0
    for (o <- operands().asScala) {
      result(i) = o.getChildExpression
      i += 1
    }
    result
  }

  def getArg(n: Int): Expression = getOperanda.getOperandExpression(n)

  def setArg(n: Int, child: Expression): Unit = {
    getOperanda.setOperand(n, child)
    adoptChildExpression(child)
  }

  def simplifyArguments(env: StaticContext): Expression = {
    for (i <- 0 until getArguments.length) {
      val exp: Expression = getArg(i).simplify()
      if (exp != getArg(i)) {
        adoptChildExpression(exp)
        setArg(i, exp)
      }
    }
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    checkArguments(visitor)
    preEvaluateIfConstant(visitor)
  }

  def preEvaluateIfConstant(visitor: ExpressionVisitor): Expression = {
    val opt: Optimizer = visitor.obtainOptimizer()
    if (opt.isOptionSet(OptimizerOptions.CONSTANT_FOLDING)) {
      var fixed: Boolean = true
      for (o <- operands().asScala if !(o.getChildExpression.isInstanceOf[Literal])) {
        fixed = false
      }
      if (fixed) {
        try preEvaluate(visitor)
        catch {
          case err: NoDynamicContextException => return this

        }
      }
    }
    this
  }

  def checkFunctionCall(target: Function, visitor: ExpressionVisitor): Unit = {
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(
      visitor.getStaticContext.isInBackwardsCompatibleMode)
    val argTypes: Array[SequenceType] =
      target.getFunctionItemType.getArgumentTypes
    val n: Int = target.getArity
    for (i <- 0 until n) {
      val name: String =
        if (getFunctionName == null) "" else getFunctionName.getDisplayName
      val role: RoleDiagnostic =
        new RoleDiagnostic(RoleDiagnostic.FUNCTION, name, i)
      setArg(i, tc.staticTypeCheck(getArg(i), argTypes(i), role, visitor))
    }
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextItemType)
    val opt: Optimizer = visitor.obtainOptimizer()
    if (opt.isOptionSet(OptimizerOptions.CONSTANT_FOLDING)) {
      var fixed: Boolean = true
      breakable {
        for (o <- operands().asScala if !(o.getChildExpression.isInstanceOf[Literal])) {
          fixed = false
          break()
        }
      }
      if (fixed) {
        preEvaluate(visitor)
      }
    }
    this
  }

  override def getNetCost(): Int = 5

  def preEvaluate(visitor: ExpressionVisitor): Expression = {
    if ((getIntrinsicDependencies & ~StaticProperty.DEPENDS_ON_STATIC_CONTEXT) !=
      0) {
      return this
    }
    try {
      val lit: Literal = Literal.makeLiteral(
        iterate(visitor.getStaticContext.makeEarlyEvaluationContext())
          .materialize(),
        this)
      Optimizer.trace(visitor.getConfiguration,
        "Pre-evaluated function call " + toShortString(),
        lit)
      lit
    } catch {
      case e: NoDynamicContextException => this

      case e: UnsupportedOperationException =>
        if (e.getCause.isInstanceOf[NoDynamicContextException]) {
          this
        } else {
          throw e
        }

    }
  }

  def checkArguments(visitor: ExpressionVisitor): Unit = ()

  def checkArgumentCount(min: Int, max: Int): Int = {
    val numArgs: Int = getArity
    var msg: String = null
    if (min == max && numArgs != min) {
      msg = "Function " + getDisplayName + " must have " + pluralArguments(min)
    } else if (numArgs < min) {
      msg = "Function " + getDisplayName + " must have at least " +
        pluralArguments(min)
    } else if (numArgs > max) {
      msg = "Function " + getDisplayName + " must have no more than " +
        pluralArguments(max)
    }
    if (msg != null) {
      val err = new XPathException(msg, "XPST0017")
      err.setIsStaticError(true)
      err.setLocation(getLocation)
      throw err
    }
    numArgs
  }

  override def getImplementationMethod(): Int = Expression.ITERATE_METHOD

  def addExternalFunctionCallToPathMap(
                                        pathMap: PathMap,
                                        pathMapNodes: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val result: PathMap.PathMapNodeSet = new PathMap.PathMapNodeSet()
    for (o <- operands().asScala) {
      result.addNodeSet(
        o.getChildExpression.addToPathMap(pathMap, pathMapNodes))
    }
    result.setHasUnknownDependencies()
    result
  }

  override def getExpressionName(): String = "functionCall"

  def getDisplayName(): String = {
    val fName: StructuredQName = getFunctionName
    if (fName == null) "(anonymous)" else fName.getDisplayName
  }

  override def toString(): String = {
    val buff: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    val fName: StructuredQName = getFunctionName
    var f: String = null
    f =
      if (fName == null) "$anonymousFunction"
      else if (fName.hasURI(NamespaceConstant.FN)) fName.getLocalPart
      else fName.getEQName
    buff.append(f)
    var first: Boolean = true
    for (o <- operands().asScala) {
      buff.append(if (first) "(" else ", ")
      buff.append(o.getChildExpression.toString)
      first = false
    }
    buff.append(if (first) "()" else ")")
    buff.toString
  }

  override def toShortString(): String = {
    val fName: StructuredQName = getFunctionName
    (if (fName == null) "$anonFn" else fName.getDisplayName) +
      "(" +
      (if (getArity == 0) "" else "...") +
      ")"
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("functionCall", this)
    if (getFunctionName == null) {
      throw new AssertionError("Exporting call to anonymous function")
    } else {
      out.emitAttribute("name", getFunctionName.getDisplayName)
    }
    for (o <- operands().asScala) {
      o.getChildExpression.export(out)
    }
    out.endElement()
  }

  override def equals(o: Any): Boolean = {
    if (!(o.isInstanceOf[FunctionCall])) {
      return false
    }
    if (getFunctionName == null) {
      return this == o
    }
    val f: FunctionCall = o.asInstanceOf[FunctionCall]
    if (getFunctionName != f.getFunctionName) {
      return false
    }
    if (getArity != f.getArity) {
      return false
    }
    for (i <- 0 until getArity if !getArg(i).isEqual(f.getArg(i))) {
      false
    }
    true
  }

  override def computeHashCode(): Int = {
    if (getFunctionName == null) {
      super.computeHashCode()
    }
    var h: Int = getFunctionName.hashCode
    for (i <- 0 until getArity) {
      h ^= getArg(i).hashCode
    }
    h
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val target: Function = getTargetFunction(context)
    val actualArgs: Array[Sequence] = evaluateArguments(context)
    try target.call(context, actualArgs).iterate()
    catch {
      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        e.maybeSetContext(context)
        e.maybeSetFailingExpression(this)
        throw e
      }

    }
  }

  def evaluateArguments(context: XPathContext): Array[Sequence] = {
    val numArgs: Int = getArity
    val actualArgs: Array[Sequence] = Array.ofDim[Sequence](numArgs)
    for (i <- 0 until numArgs) {
      actualArgs(i) = ExpressionTool.lazyEvaluate(getArg(i), context, repeatable = false)
    }
    actualArgs
  }

  def adjustRequiredType(requiredType: JavaExternalObjectType): Boolean = false

}
