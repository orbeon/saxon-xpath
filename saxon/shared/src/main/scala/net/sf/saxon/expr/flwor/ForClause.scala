package net.sf.saxon.expr.flwor

import net.sf.saxon.utils.Configuration

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.KeyFn

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.lib.Feature

import net.sf.saxon.model._

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.Cardinality

import net.sf.saxon.value.SequenceType

import java.util.ArrayList

import java.util.List

import net.sf.saxon.expr.flwor.Clause.ClauseName.FOR

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._

import Clause.ClauseName.ClauseName
import scala.util.control.Breaks._

class ForClause extends Clause {

  @BeanProperty
  var rangeVariable: LocalVariableBinding = _

  @BeanProperty
  var positionVariable: LocalVariableBinding = _

  private var sequenceOp: Operand = _

  private var allowsEmpty: Boolean = _

  override def getClauseKey(): ClauseName = FOR

  def copy(flwor: FLWORExpression, rebindings: RebindingMap): ForClause = {
    val f2: ForClause = new ForClause()
    f2.setLocation(getLocation)
    f2.setPackageData(getPackageData)
    f2.rangeVariable = rangeVariable.copy()
    if (positionVariable != null) {
      f2.positionVariable = positionVariable.copy()
    }
    f2.initSequence(flwor, getSequence.copy(rebindings))
    f2.allowsEmpty = allowsEmpty
    f2
  }

  def initSequence(flwor: FLWORExpression, sequence: Expression): Unit = {
    sequenceOp = new Operand(
      flwor,
      sequence,
      if (isRepeated) OperandRole.REPEAT_NAVIGATE else OperandRole.NAVIGATE)
  }

  def setSequence(sequence: Expression): Unit = {
    sequenceOp.setChildExpression(sequence)
  }

  def getSequence(): Expression = sequenceOp.getChildExpression

  override def getRangeVariables(): Array[LocalVariableBinding] =
    if (positionVariable == null) {
      Array(rangeVariable)
    } else {
      Array(rangeVariable, positionVariable)
    }

  def setAllowingEmpty(option: Boolean): Unit = {
    allowsEmpty = option
  }

  def isAllowingEmpty(): Boolean = allowsEmpty

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Unit = {
    val decl: SequenceType = rangeVariable.getRequiredType
    if (allowsEmpty && !Cardinality.allowsZero(decl.getCardinality)) {
      val role: RoleDiagnostic = new RoleDiagnostic(
        RoleDiagnostic.VARIABLE,
        rangeVariable.getVariableQName.getDisplayName,
        0)
      val checker: Expression = CardinalityChecker.makeCardinalityChecker(
        getSequence,
        StaticProperty.ALLOWS_ONE_OR_MORE,
        role)
      this.setSequence(checker)
    }
    val sequenceType: SequenceType = SequenceType.makeSequenceType(
      decl.getPrimaryType,
      StaticProperty.ALLOWS_ZERO_OR_MORE)
    val role: RoleDiagnostic = new RoleDiagnostic(
      RoleDiagnostic.VARIABLE,
      rangeVariable.getVariableQName.getDisplayName,
      0)
    this.setSequence(TypeChecker.strictTypeCheck(getSequence,
      sequenceType,
      role,
      visitor.getStaticContext))
  }

  override def getPullStream(base: TuplePull,
                             context: XPathContext): TuplePull =
    if (allowsEmpty) {
      new ForClauseOuterPull(base, this)
    } else {
      new ForClausePull(base, this)
    }

  override def getPushStream(destination: TuplePush,
                             output: Outputter,
                             context: XPathContext): TuplePush =
    if (allowsEmpty) {
      new ForClauseOuterPush(output, destination, this)
    } else {
      new ForClausePush(output, destination, this)
    }

  def addPredicate(flwor: FLWORExpression,
                   visitor: ExpressionVisitor,
                   contextItemType: ContextItemStaticInfo,
                   condition: Expression): Boolean = {
    val config: Configuration = getConfiguration
    val opt: Optimizer = visitor.obtainOptimizer()
    val debug: Boolean =
      config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)
    val th: TypeHierarchy = config.getTypeHierarchy
    var head: Expression = null
    var selection: Expression = getSequence
    var selectionContextItemType: ItemType =
      if (contextItemType == null) null else contextItemType.getItemType
    if (getSequence.isInstanceOf[SlashExpression]) {
      if (getSequence.asInstanceOf[SlashExpression].isAbsolute) {
        head = getSequence.asInstanceOf[SlashExpression].getFirstStep
        selection = getSequence.asInstanceOf[SlashExpression].getRemainingSteps
        selectionContextItemType = head.getItemType
      } else {
        val p: SlashExpression =
          getSequence.asInstanceOf[SlashExpression].tryToMakeAbsolute()
        if (p != null) {
          this.setSequence(p)
          head = p.getFirstStep
          selection = p.getRemainingSteps
          selectionContextItemType = head.getItemType
        }
      }
    }
    var changed: Boolean = false
    if (positionVariable != null &&
      (condition.isInstanceOf[ValueComparison] || condition
        .isInstanceOf[GeneralComparison] ||
        condition.isInstanceOf[CompareToIntegerConstant]) &&
      ExpressionTool.dependsOnVariable(condition, Array(positionVariable))) {
      val comp: ComparisonExpression =
        condition.asInstanceOf[ComparisonExpression]
      val operands: Array[Expression] =
        Array(comp.getLhsExpression, comp.getRhsExpression)
      if (ExpressionTool.dependsOnVariable(flwor, Array(positionVariable))) {
        return false
      }
      breakable {
        for (op <- 0.until(2)) {
          val thisVar: Array[Binding] = Array(this.getRangeVariable)
          if (positionVariable != null && operands(op)
            .isInstanceOf[VariableReference] &&
            !changed) {
            val varRefs: List[VariableReference] =
              new ArrayList[VariableReference]()
            ExpressionTool.gatherVariableReferences(condition,
              positionVariable,
              varRefs)
            if (varRefs.size == 1 && varRefs.get(0) == operands(op) &&
              !ExpressionTool.dependsOnFocus(operands(1 - op)) &&
              !ExpressionTool.dependsOnVariable(operands(1 - op), thisVar)) {
              val rsc: RetainedStaticContext = new RetainedStaticContext(
                visitor.getStaticContext)
              val position: Expression = SystemFunction.makeCall("position", rsc)
              val predicate: Expression = condition.copy(new RebindingMap())
              val child: Operand =
                if (op == 0) predicate.asInstanceOf[ComparisonExpression].getLhs
                else predicate.asInstanceOf[ComparisonExpression].getRhs
              child.setChildExpression(position)
              if (debug) {
                opt.trace(
                  "Replaced positional variable in predicate by position()",
                  predicate)
              }
              selection = new FilterExpression(selection, predicate)
              ExpressionTool.copyLocationInfo(predicate, selection)
              val cit: ContextItemStaticInfo =
                config.makeContextItemStaticInfo(selectionContextItemType, true)
              selection = selection.typeCheck(visitor, cit)
              if (!ExpressionTool.dependsOnVariable(flwor,
                Array(positionVariable))) {
                positionVariable = null
              }
              changed = true
              break
            }
          }
        }
      }
    }
    if (positionVariable == null) {
      val thisVar: Array[Binding] = Array(this.getRangeVariable)
      if (opt.isVariableReplaceableByDot(condition, thisVar)) {
        val replacement: Expression = new ContextItemExpression()
        val found: Boolean = ExpressionTool.inlineVariableReferences(
          condition,
          this.getRangeVariable,
          replacement)
        if (found) {
          var cit: ContextItemStaticInfo =
            config.makeContextItemStaticInfo(getSequence.getItemType, true)
          var predicate: Expression = condition.typeCheck(visitor, cit)
          val rel: Affinity.Affinity =
            th.relationship(predicate.getItemType, BuiltInAtomicType.INTEGER)
          if (rel != Affinity.DISJOINT) {
            val rsc: RetainedStaticContext = new RetainedStaticContext(
              visitor.getStaticContext)
            predicate = SystemFunction.makeCall("boolean", rsc, predicate)
            assert(predicate != null)
          }
          selection = new FilterExpression(selection, predicate)
          ExpressionTool.copyLocationInfo(predicate, selection)
          cit =
            config.makeContextItemStaticInfo(selectionContextItemType, true)
          selection = selection.typeCheck(visitor, cit)
          changed = true
        }
      }
    }
    if (changed) {
      if (head == null) {
        this.setSequence(selection)
      } else if (head.isInstanceOf[RootExpression] && selection.isCallOn(
        classOf[KeyFn])) {
        this.setSequence(selection)
      } else {
        val path: Expression =
          ExpressionTool.makePathExpression(head, selection)
        if (path.isInstanceOf[SlashExpression]) {
          ExpressionTool.copyLocationInfo(condition, path)
          val k: Expression = visitor
            .obtainOptimizer()
            .convertPathExpressionToKey(path.asInstanceOf[SlashExpression],
              visitor)
          if (k == null) {
            this.setSequence(path)
          } else {
            this.setSequence(k)
          }
          sequenceOp.typeCheck(visitor, contextItemType)
          sequenceOp.optimize(visitor, contextItemType)
        }
      }
    }
    changed
  }

  override def processOperands(processor: OperandProcessor): Unit = {
    processor.processOperand(sequenceOp)
  }

  override def gatherVariableReferences(
                                         visitor: ExpressionVisitor,
                                         binding: Binding,
                                         references: List[VariableReference]): Unit = {
    ExpressionTool.gatherVariableReferences(getSequence, binding, references)
  }

  override def refineVariableType(visitor: ExpressionVisitor,
                                  references: List[VariableReference],
                                  returnExpr: Expression): Unit = {
    var actualItemType: ItemType = getSequence.getItemType
    if (actualItemType.isInstanceOf[ErrorType]) {
      actualItemType = AnyItemType.getInstance
    }
    for (ref <- references.asScala) {
      ref.refineVariableType(actualItemType,
        if (allowsEmpty) StaticProperty.ALLOWS_ZERO_OR_ONE
        else StaticProperty.EXACTLY_ONE,
        null,
        getSequence.getSpecialProperties)
    }
  }

  override def addToPathMap(pathMap: PathMap,
                            pathMapNodeSet: PathMap.PathMapNodeSet): Unit = {
    val varPath: PathMap.PathMapNodeSet =
      getSequence.addToPathMap(pathMap, pathMapNodeSet)
    pathMap.registerPathForVariable(rangeVariable, varPath)
  }

  override def explain(out: ExpressionPresenter): Unit = {
    out.startElement("for")
    out.emitAttribute("var", getRangeVariable.getVariableQName)
    out.emitAttribute("slot", getRangeVariable.getLocalSlotNumber.toString)
    val posVar: LocalVariableBinding = getPositionVariable
    if (posVar != null) {
      out.emitAttribute("at", posVar.getVariableQName)
      out.emitAttribute("at-slot", posVar.getLocalSlotNumber.toString)
    }
    getSequence.export(out)
    out.endElement()
  }

  override def toShortString(): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    fsb.append("for $")
    fsb.append(rangeVariable.getVariableQName.getDisplayName)
    fsb.cat(' ')
    val posVar: LocalVariableBinding = getPositionVariable
    if (posVar != null) {
      fsb.append("at $")
      fsb.append(posVar.getVariableQName.getDisplayName)
      fsb.cat(' ')
    }
    fsb.append("in ")
    fsb.append(getSequence.toShortString())
    fsb.toString
  }

  override def toString(): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
    fsb.append("for $")
    fsb.append(rangeVariable.getVariableQName.getDisplayName)
    fsb.cat(' ')
    val posVar: LocalVariableBinding = getPositionVariable
    if (posVar != null) {
      fsb.append("at $")
      fsb.append(posVar.getVariableQName.getDisplayName)
      fsb.cat(' ')
    }
    fsb.append("in ")
    fsb.append(getSequence.toString)
    fsb.toString
  }

}
