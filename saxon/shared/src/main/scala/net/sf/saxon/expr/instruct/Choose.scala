package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.BooleanFn

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.model._

import scala.util.control.Breaks._

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.StandardNames

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.XmlProcessingException

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.Cardinality

import net.sf.saxon.value.SequenceType

import java.util.ArrayList

import java.util.Arrays

import java.util.List

import Choose._

import scala.jdk.CollectionConverters._

object Choose {

  val CHOICE_ACTION: OperandRole = new OperandRole(OperandRole.IN_CHOICE_GROUP,
    OperandUsage.TRANSMISSION,
    SequenceType.ANY_SEQUENCE)

  def makeConditional(condition: Expression,
                      thenExp: Expression,
                      elseExp: Expression): Expression =
    if (Literal.isEmptySequence(elseExp)) {
      val conditions: Array[Expression] = Array(condition)
      val actions: Array[Expression] = Array(thenExp)
      new Choose(conditions, actions)
    } else {
      val conditions: Array[Expression] =
        Array(condition, Literal.makeLiteral(BooleanValue.TRUE, condition))
      val actions: Array[Expression] = Array(thenExp, elseExp)
      new Choose(conditions, actions)
    }

  def makeConditional(condition: Expression, thenExp: Expression): Expression = {
    val conditions: Array[Expression] = Array(condition)
    val actions: Array[Expression] = Array(thenExp)
    new Choose(conditions, actions)
  }

  def isSingleBranchChoice(exp: Expression): Boolean =
    exp.isInstanceOf[Choose] && exp.asInstanceOf[Choose].size == 1

}

class Choose(conditions: Array[Expression], actions: Array[Expression])
  extends Instruction
    with ConditionalInstruction {

  private var conditionOps: Array[Operand] =
    new Array[Operand](conditions.length)

  private var actionOps: Array[Operand] = new Array[Operand](actions.length)

  var isInstruct: Boolean = _

  for (i <- 0 until conditions.length) {
    conditionOps(i) = new Operand(this, conditions(i), OperandRole.INSPECT)
  }

  for (i <- 0 until actions.length) {
    actionOps(i) = new Operand(this, actions(i), CHOICE_ACTION)
  }

  def setInstruction(inst: Boolean): Unit = {
    isInstruct = inst
  }

  def size(): Int = conditionOps.length

  def getNumberOfConditions(): Int = size()

  def getCondition(i: Int): Expression = conditionOps(i).getChildExpression

  def setCondition(i: Int, condition: Expression): Unit = {
    conditionOps(i).setChildExpression(condition)
  }

  def conditions(): java.lang.Iterable[Operand] =
    Arrays.asList(conditionOps: _*)

  def getActionOperand(i: Int): Operand = actionOps(i)

  def getAction(i: Int): Expression = actionOps(i).getChildExpression

  def setAction(i: Int, action: Expression): Unit = {
    actionOps(i).setChildExpression(action)
  }

  def actions(): java.lang.Iterable[Operand] = Arrays.asList(actionOps: _*)

  override def operands(): java.lang.Iterable[Operand] = {
    val operanda: List[Operand] = new ArrayList[Operand](size * 2)
    for (i <- 0 until size) {
      operanda.add(conditionOps(i))
      operanda.add(actionOps(i))
    }
    operanda
  }

  override def allowExtractingCommonSubexpressions(): Boolean = false

  def atomizeActions(): Unit = {
    for (i <- 0 until size) {
      setAction(i, Atomizer.makeAtomizer(getAction(i), null))
    }
  }

  override def getInstructionNameCode(): Int =
    if (size == 1) StandardNames.XSL_IF else StandardNames.XSL_CHOOSE

  override def simplify(): Expression = {
    for (i <- 0 until size) {
      setCondition(i, getCondition(i).simplify())
      try setAction(i, getAction(i).simplify())
      catch {
        case err: XPathException =>
          if (err.isTypeError) {
            throw err
          } else {
            setAction(i, new ErrorExpression(new XmlProcessingException(err)))
          }

      }
    }
    this
  }

  private def removeRedundantBranches(visitor: ExpressionVisitor): Expression = {
    val result: Expression = removeRedundantBranches0(visitor)
    if (result != this) {
      ExpressionTool.copyLocationInfo(this, result)
    }
    result
  }

  private def removeRedundantBranches0(visitor: ExpressionVisitor): Expression = {
    var compress: Boolean = false
    breakable {
      for (i <- 0 until size()) {
        val condition: Expression = getCondition(i)
        if (condition.isInstanceOf[Literal]) {
          compress = true
          break()
        }
      }
    }
    val localSize: Int = size
    val changed: Boolean = false
    if (compress) {
      val conditions: List[Expression] = new ArrayList[Expression](localSize)
      val actions: List[Expression] = new ArrayList[Expression](localSize)
      breakable {
        for (i <- 0 until localSize) {
          val condition: Expression = getCondition(i)
          if (!Literal.hasEffectiveBooleanValue(condition, value = false)) {
            conditions.add(condition)
            actions.add(getAction(i))
          }
          if (Literal.hasEffectiveBooleanValue(condition, value = true)) {
            break()
          }
        }
      }
      if (conditions.isEmpty) {
        val lit: Literal = Literal.makeEmptySequence()
        ExpressionTool.copyLocationInfo(this, lit)
        return lit
      } else if (conditions.size == 1 &&
        Literal.hasEffectiveBooleanValue(conditions.get(0), value = true)) {
        actions.get(0)
      } else if (conditions.size != localSize) {
        val c: Array[Expression] =
          conditions.toArray(Array.ofDim[Expression](conditions.size))
        val a: Array[Expression] =
          actions.toArray(Array.ofDim[Expression](actions.size))
        val result: Choose = new Choose(c, a)
        result.setRetainedStaticContext(getRetainedStaticContext)
        return result
      }
    }
    if (localSize == 1 &&
      Literal.hasEffectiveBooleanValue(getCondition(0), value = true)) {
      getAction(0)
    }
    if (Literal.isEmptySequence(getAction(localSize - 1))) {
      if (localSize == 1) {
        val lit: Literal = Literal.makeEmptySequence()
        ExpressionTool.copyLocationInfo(this, lit)
        return lit
      } else {
        val conditions: Array[Expression] = Array.ofDim[Expression](localSize - 1)
        val actions: Array[Expression] = Array.ofDim[Expression](localSize - 1)
        for (i <- 0 until localSize - 1) {
          conditions(i) = getCondition(i)
          actions(i) = getAction(i)
        }
        new Choose(conditions, actions)
      }
    }
    if (Literal.hasEffectiveBooleanValue(getCondition(localSize - 1), value = true) &&
      getAction(localSize - 1).isInstanceOf[Choose]) {
      val choose2: Choose = getAction(localSize - 1).asInstanceOf[Choose]
      val newLen: Int = localSize + choose2.size - 1
      val c2: Array[Expression] = Array.ofDim[Expression](newLen)
      val a2: Array[Expression] = Array.ofDim[Expression](newLen)
      for (i <- 0 until localSize - 1) {
        c2(i) = getCondition(i)
        a2(i) = getAction(i)
      }
      for (i <- 0 until choose2.size) {
        c2(i + localSize - 1) = choose2.getCondition(i)
        a2(i + localSize - 1) = choose2.getAction(i)
      }
      new Choose(c2, a2)
    }
    if (localSize == 2 && Literal.isConstantBoolean(getAction(0), value = true) &&
      Literal.isConstantBoolean(getAction(1), value = false) &&
      Literal.hasEffectiveBooleanValue(getCondition(1), value = true)) {
      val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
      if (th.isSubType(getCondition(0).getItemType, BuiltInAtomicType.BOOLEAN) &&
        getCondition(0).getCardinality == StaticProperty.EXACTLY_ONE) {
        getCondition(0)
      } else {
        SystemFunction.makeCall("boolean",
          getRetainedStaticContext,
          getCondition(0))
      }
    }
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
    for (i <- 0 until size) {
      conditionOps(i).typeCheck(visitor, contextInfo)
      val err = TypeChecker.ebvError(getCondition(i), th)
      if (err != null) {
        err.setLocator(getCondition(i).getLocation)
        err.maybeSetFailingExpression(getCondition(i))
        throw err
      }
    }
    // Check that each of the action branches satisfies the expected type. This is a stronger check than checking the
    // type of the top-level expression. It's important with tail recursion not to wrap a tail call in a type checking
    // expression just because a dynamic type check is needed on a different branch of the choice.
    breakable {
      for (i <- 0 until size) {
        if (Literal.hasEffectiveBooleanValue(getCondition(i), value = false)) {
          // Don't do any checking if we know statically the condition will be false, because it could
          // result in spurious warnings: bug 4537
        } else {
          try actionOps(i).typeCheck(visitor, contextInfo)
          catch {
            case err: XPathException => {
              err.maybeSetLocation(getLocation)
              err.maybeSetFailingExpression(getAction(i))
              if (err.isStaticError) {
                throw err
              } else if (err.isTypeError) {
                if (Literal.isEmptySequence(getAction(i)) ||
                  Literal.hasEffectiveBooleanValue(getCondition(i), value = false)) {
                  setAction(i,
                    new ErrorExpression(new XmlProcessingException(err)))
                } else {
                  throw err
                }
              } else {
                setAction(i, new ErrorExpression(new XmlProcessingException(err)))
              }
            }
          }
          if (Literal.hasEffectiveBooleanValue(getCondition(i), value = true))
            break()
        }
      }
    }
    val opt: Optimizer = visitor.obtainOptimizer()
    if (opt.isOptionSet(OptimizerOptions.CONSTANT_FOLDING)) {
      val reduced: Expression = removeRedundantBranches(visitor)
      if (reduced != this) {
        reduced.typeCheck(visitor, contextInfo)
      }
      return reduced
    }
    this
  }

  override def implementsStaticTypeCheck(): Boolean = true

  override def staticTypeCheck(req: SequenceType,
                               backwardsCompatible: Boolean,
                               role: RoleDiagnostic,
                               visitor: ExpressionVisitor): Expression = {
    val sizeInt: Int = size
    val tc: TypeChecker = getConfiguration.getTypeChecker(backwardsCompatible)
    for (i <- 0 until sizeInt) {
      try setAction(i, tc.staticTypeCheck(getAction(i), req, role, visitor))
      catch {
        case err: XPathException => {
          if (err.isStaticError) {
            throw err
          }
          val ee: ErrorExpression = new ErrorExpression(
            new XmlProcessingException(err))
          ExpressionTool.copyLocationInfo(getAction(i), ee)
          setAction(i, ee)
        }

      }
    }
    if (!Literal.hasEffectiveBooleanValue(getCondition(sizeInt - 1), value = true) &&
      !Cardinality.allowsZero(req.getCardinality)) {
      val c: Array[Expression] = Array.ofDim[Expression](sizeInt + 1)
      val a: Array[Expression] = Array.ofDim[Expression](sizeInt + 1)
      for (i <- 0 until sizeInt) {
        c(i) = getCondition(i)
        a(i) = getAction(i)
      }
      c(sizeInt) = Literal.makeLiteral(BooleanValue.TRUE, this)
      val cond: String =
        if (sizeInt == 1) "The condition is not" else "None of the conditions is"
      val message: String = "Conditional expression: " + cond + " satisfied, so an empty sequence is returned, " +
        "but this is not allowed as the " +
        role.getMessage
      val errExp: ErrorExpression =
        new ErrorExpression(message, role.getErrorCode, true)
      ExpressionTool.copyLocationInfo(this, errExp)
      a(sizeInt) = errExp
      new Choose(c, a)
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val sizeInt: Int = size
    for (i <- 0 until sizeInt) {
      conditionOps(i).optimize(visitor, contextItemType)
      val ebv: Expression = BooleanFn.rewriteEffectiveBooleanValue(
        getCondition(i),
        visitor,
        contextItemType)
      if (ebv != null && ebv != getCondition(i)) {
        setCondition(i, ebv)
      }
      if (getCondition(i).isInstanceOf[Literal] &&
        !(getCondition(i)
          .asInstanceOf[Literal]
          .getValue
          .isInstanceOf[BooleanValue])) {
        var b: Boolean = false
        try b = getCondition(i)
          .asInstanceOf[Literal]
          .getValue
          .effectiveBooleanValue()
        catch {
          case err: XPathException => {
            err.setLocation(getLocation)
            throw err
          }

        }
        setCondition(i, Literal.makeLiteral(BooleanValue.get(b), this))
      }
    }
    for (i <- 0 until sizeInt) {
      if (Literal.hasEffectiveBooleanValue(getCondition(i), value = false)) {
      }
      try actionOps(i).optimize(visitor, contextItemType)
      catch {
        case err: XPathException =>
          if (err.isTypeError) {
            throw err
          } else {
            val ee: ErrorExpression = new ErrorExpression(
              new XmlProcessingException(err))
            ExpressionTool.copyLocationInfo(actionOps(i).getChildExpression,
              ee)
            setAction(i, ee)
          }

      }
      if (getAction(i).isInstanceOf[ErrorExpression] &&
        getAction(i).asInstanceOf[ErrorExpression].isTypeError &&
        !Literal.isConstantBoolean(getCondition(i), value = false) &&
        !Literal.isConstantBoolean(getCondition(i), value = true)) {
        visitor.issueWarning(
          "Branch " + (i + 1) +
            " of conditional will fail with a type error if executed. " +
            getAction(i).asInstanceOf[ErrorExpression].getMessage,
          getAction(i).getLocation
        )
      }
      breakable {
        if (Literal.hasEffectiveBooleanValue(getCondition(i), value = true)) {
          break()
        }
      }
    }
    if (sizeInt == 0) {
      Literal.makeEmptySequence()
    }
    val opt: Optimizer = visitor.obtainOptimizer()
    if (opt.isOptionSet(OptimizerOptions.CONSTANT_FOLDING)) {
      val e: Expression = removeRedundantBranches(visitor)
      if (e.isInstanceOf[Choose]) {
        visitor.obtainOptimizer().trySwitch(e.asInstanceOf[Choose], visitor)
      } else {
        return e
      }
    }
    this
  }

  def copy(rebindings: RebindingMap): Expression = {
    val sizeInt: Int = size
    val c2: Array[Expression] = Array.ofDim[Expression](sizeInt)
    val a2: Array[Expression] = Array.ofDim[Expression](sizeInt)
    for (c <- 0 until sizeInt) {
      c2(c) = getCondition(c).copy(rebindings)
      a2(c) = getAction(c).copy(rebindings)
    }
    val ch2: Choose = new Choose(c2, a2)
    ExpressionTool.copyLocationInfo(this, ch2)
    ch2.setInstruction(isInstruction)
    ch2
  }

  override def checkForUpdatingSubexpressions(): Unit = {
    for (o <- conditions().asScala) {
      val condition: Expression = o.getChildExpression
      condition.checkForUpdatingSubexpressions()
      if (condition.isUpdatingExpression) {
        val err = new XPathException(
          "Updating expression appears in a context where it is not permitted",
          "XUST0001")
        err.setLocator(condition.getLocation)
        throw err
      }
    }
    var updating: Boolean = false
    var nonUpdating: Boolean = false
    for (o <- actions().asScala) {
      val act: Expression = o.getChildExpression
      act.checkForUpdatingSubexpressions()
      if (ExpressionTool.isNotAllowedInUpdatingContext(act)) {
        if (updating) {
          val err = new XPathException(
            "If any branch of a conditional is an updating expression, then all must be updating expressions (or vacuous)",
            "XUST0001")
          err.setLocator(act.getLocation)
          throw err
        }
        nonUpdating = true
      }
      if (act.isUpdatingExpression) {
        if (nonUpdating) {
          val err = new XPathException(
            "If any branch of a conditional is an updating expression, then all must be updating expressions (or vacuous)",
            "XUST0001")
          err.setLocator(act.getLocation)
          throw err
        }
        updating = true
      }
    }
  }

  override def isUpdatingExpression(): Boolean =
    actions().asScala
      .find(_.getChildExpression.isUpdatingExpression)
      .map(_ => true)
      .getOrElse(false)

  override def isVacuousExpression(): Boolean =
    actions().asScala
      .find(!_.getChildExpression.isVacuousExpression)
      .map(_ => false)
      .getOrElse(true)

  override def getImplementationMethod(): Int = {
    var m: Int = Expression.PROCESS_METHOD | Expression.ITERATE_METHOD |
      Expression.WATCH_METHOD
    if (!Cardinality.allowsMany(getCardinality)) {
      m |= Expression.EVALUATE_METHOD
    }
    m
  }

  override def markTailFunctionCalls(qName: StructuredQName, arity: Int): Int = {
    var result: Int = UserFunctionCall.NOT_TAIL_CALL
    for (action <- actions().asScala) {
      result = Math.max(
        result,
        action.getChildExpression.markTailFunctionCalls(qName, arity))
    }
    result
  }

  override def getItemType(): ItemType = {
    val th: TypeHierarchy = getConfiguration.getTypeHierarchy
    var `type`: ItemType = getAction(0).getItemType
    for (i <- 1 until size) {
      `type` = Type.getCommonSuperType(`type`, getAction(i).getItemType, th)
    }
    `type`
  }

  override def getStaticUType(contextItemType: UType): UType =
    if (isInstruction) {
      super.getStaticUType(contextItemType)
    } else {
      var `type`: UType = getAction(0).getStaticUType(contextItemType)
      for (i <- 1 until size) {
        `type` = `type`.union(getAction(i).getStaticUType(contextItemType))
      }
      `type`
    }

  override def computeCardinality(): Int = {
    var card: Int = 0
    var includesTrue: Boolean = false
    for (i <- 0 until size) {
      card = Cardinality.union(card, getAction(i).getCardinality)
      if (Literal.hasEffectiveBooleanValue(getCondition(i), value = true)) {
        includesTrue = true
      }
    }
    if (!includesTrue) {
      card = Cardinality.union(card, StaticProperty.ALLOWS_ZERO)
    }
    card
  }

  override def computeSpecialProperties(): Int = {
    var props: Int = getAction(0).getSpecialProperties
    for (i <- 1 until size) {
      props &= getAction(i).getSpecialProperties
    }
    props
  }

  override def mayCreateNewNodes(): Boolean = {
    for (action <- actions().asScala) {
      val props: Int = action.getChildExpression.getSpecialProperties
      if ((props & StaticProperty.NO_NODES_NEWLY_CREATED) == 0) {
        true
      }
    }
    false
  }

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    for (i <- 0 until size) {
      setAction(i, getAction(i).unordered(retainAllNodes, forStreaming))
    }
    this
  }

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    for (action <- actions().asScala) {
      action.getChildExpression.checkPermittedContents(parentType, whole)
    }
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    for (condition <- conditions().asScala) {
      condition.getChildExpression.addToPathMap(pathMap, pathMapNodeSet)
    }
    val result: PathMap.PathMapNodeSet = new PathMap.PathMapNodeSet()
    for (action <- actions().asScala) {
      val temp: PathMap.PathMapNodeSet =
        action.getChildExpression.addToPathMap(pathMap, pathMapNodeSet)
      result.addNodeSet(temp)
    }
    result
  }

  override def toString(): String = {
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    sb.append("if (")
    for (i <- 0 until size) {
      sb.append(getCondition(i).toString)
      sb.append(") then (")
      sb.append(getAction(i).toString)
      if (i == size - 1) {
        sb.append(")")
      } else {
        sb.append(") else if (")
      }
    }
    sb.toString
  }

  override def toShortString(): String =
    "if(" + getCondition(0).toShortString() + ") then ... else ..."

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("choose", this)
    for (i <- 0 until size) {
      getCondition(i).export(out)
      getAction(i).export(out)
    }
    out.endElement()
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val i: Int = choose(context)
    if (i >= 0) {
      val action: Expression = getAction(i)
      if (action.isInstanceOf[TailCallReturner]) {
        action
          .asInstanceOf[TailCallReturner]
          .processLeavingTail(output, context)
      } else {
        action.process(output, context)
        return null
      }
    }
    null
  }

  private def choose(context: XPathContext): Int = {
    val sizeInt: Int = size
    for (i <- 0 until sizeInt) {
      var b: Boolean = false
      try b = getCondition(i).effectiveBooleanValue(context)
      catch {
        case e: XPathException => {
          e.maybeSetFailingExpression(getCondition(i))
          throw e
        }

      }
      if (b) {
        i
      }
    }
    -1
  }

  override def evaluateItem(context: XPathContext): Item = {
    val i: Int = choose(context)
    if (i < 0) null else getAction(i).evaluateItem(context)
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val i: Int = choose(context)
    if (i < 0) EmptyIterator.emptyIterator() else getAction(i).iterate(context)
  }

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit = {
    val i: Int = choose(context)
    if (i >= 0) {
      getAction(i).evaluatePendingUpdates(context, pul)
    }
  }

  override def getExpressionName(): String = "choose"

  override def getStreamerName(): String = "Choose"

}
