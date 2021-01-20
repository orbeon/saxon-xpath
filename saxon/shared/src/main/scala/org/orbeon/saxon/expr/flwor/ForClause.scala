package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter
import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.flwor.Clause.ClauseName.{ClauseName, FOR}
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.{KeyFn, SystemFunction}
import org.orbeon.saxon.lib.Feature
import org.orbeon.saxon.model._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.{Cardinality, SequenceType}

import java.util.{ArrayList, List}
import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._


class ForClause extends Clause {

  @BeanProperty
  var rangeVariable: LocalVariableBinding = _

  @BeanProperty
  var positionVariable: LocalVariableBinding = _

  private var sequenceOp: Operand = _
  private var allowsEmpty: Boolean = _

  def getClauseKey: ClauseName = FOR

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

  def initSequence(flwor: FLWORExpression, sequence: Expression): Unit =
    sequenceOp = new Operand(
      flwor,
      sequence,
      if (isRepeated) OperandRole.REPEAT_NAVIGATE else OperandRole.NAVIGATE
    )

  def setSequence(sequence: Expression): Unit =
    sequenceOp.setChildExpression(sequence)

  def getSequence: Expression = sequenceOp.getChildExpression

  override def getRangeVariables: Array[LocalVariableBinding] =
    if (positionVariable == null)
      Array(rangeVariable)
    else
      Array(rangeVariable, positionVariable)

  def setAllowingEmpty(option: Boolean): Unit =
    allowsEmpty = option

  def isAllowingEmpty: Boolean = allowsEmpty

  override def typeCheck(
    visitor     : ExpressionVisitor,
    contextInfo : ContextItemStaticInfo
  ): Unit = {
    val decl = rangeVariable.getRequiredType
    if (allowsEmpty && !Cardinality.allowsZero(decl.getCardinality)) {
      val role    = new RoleDiagnostic(
        RoleDiagnostic.VARIABLE,
        rangeVariable.getVariableQName.getDisplayName,
        0)
      val checker = CardinalityChecker.makeCardinalityChecker(
        getSequence,
        StaticProperty.ALLOWS_ONE_OR_MORE,
        role
      )
      checker.adoptChildExpression(getSequence)
      this.setSequence(checker)
    }
    val sequenceType = SequenceType.makeSequenceType(
      decl.getPrimaryType,
      StaticProperty.ALLOWS_ZERO_OR_MORE
    )
    val role = new RoleDiagnostic(
      RoleDiagnostic.VARIABLE,
      rangeVariable.getVariableQName.getDisplayName,
      0
    )
    this.setSequence(TypeChecker.strictTypeCheck(getSequence,
      sequenceType,
      role,
      visitor.getStaticContext))
  }

  override def getPullStream(base: TuplePull, context: XPathContext): TuplePull =
    if (allowsEmpty)
      new ForClauseOuterPull(base, this)
    else
      new ForClausePull(base, this)

  override def getPushStream(destination: TuplePush,
                             output: Outputter,
                             context: XPathContext): TuplePush =
    if (allowsEmpty)
      new ForClauseOuterPush(output, destination, this)
    else
      new ForClausePush(output, destination, this)

  def addPredicate(flwor: FLWORExpression,
                   visitor: ExpressionVisitor,
                   contextItemType: ContextItemStaticInfo,
                   condition: Expression): Boolean = {
    val config = getConfiguration
    val opt    = visitor.obtainOptimizer()
    val debug  = config.getBooleanProperty(Feature.TRACE_OPTIMIZER_DECISIONS)
    val th     = config.getTypeHierarchy
    var head: Expression = null
    var selection  = getSequence
    var selectionContextItemType: ItemType =
      if (contextItemType == null)
        null
      else
        contextItemType.getItemType
    if (getSequence.isInstanceOf[SlashExpression]) {
      if (getSequence.asInstanceOf[SlashExpression].isAbsolute) {
        head = getSequence.asInstanceOf[SlashExpression].getFirstStep
        selection = getSequence.asInstanceOf[SlashExpression].getRemainingSteps
        selectionContextItemType = head.getItemType
      } else {
        val p = getSequence.asInstanceOf[SlashExpression].tryToMakeAbsolute()
        if (p != null) {
          this.setSequence(p)
          head = p.getFirstStep
          selection = p.getRemainingSteps
          selectionContextItemType = head.getItemType
        }
      }
    }
    var changed = false
    if (positionVariable != null &&
      (condition.isInstanceOf[ValueComparison] || condition
        .isInstanceOf[GeneralComparison] ||
        condition.isInstanceOf[CompareToIntegerConstant]) &&
      ExpressionTool.dependsOnVariable(condition, Array(positionVariable))) {
      val comp                        = condition.asInstanceOf[ComparisonExpression]
      val operands: Array[Expression] =
        Array(comp.getLhsExpression, comp.getRhsExpression)
      if (ExpressionTool.dependsOnVariable(flwor, Array(positionVariable)))
        return false
      breakable {
        for (op <- 0 until 2) {
          val thisVar: Array[Binding] = Array(this.getRangeVariable)
          if (positionVariable != null && operands(op).isInstanceOf[VariableReference] && ! changed) {
            val varRefs = new ArrayList[VariableReference]()
            ExpressionTool.gatherVariableReferences(condition, positionVariable, varRefs)
            if (varRefs.size == 1 && (varRefs.get(0) eq operands(op)) &&
              !ExpressionTool.dependsOnFocus(operands(1 - op)) &&
              !ExpressionTool.dependsOnVariable(operands(1 - op), thisVar)) {
              val rsc                   = new RetainedStaticContext(visitor.getStaticContext)
              val position = SystemFunction.makeCall("position", rsc)
              val predicate      = condition.copy(new RebindingMap())
              val child          =
                if (op == 0)
                  predicate.asInstanceOf[ComparisonExpression].getLhs
                else
                  predicate.asInstanceOf[ComparisonExpression].getRhs
              child.setChildExpression(position)
              if (debug)
                opt.trace(
                  "Replaced positional variable in predicate by position",
                  predicate
                )
              selection = new FilterExpression(selection, predicate)
              ExpressionTool.copyLocationInfo(predicate, selection)
              val cit = config.makeContextItemStaticInfo(selectionContextItemType, maybeUndefined = true)
              selection = selection.typeCheck(visitor, cit)
              if (! ExpressionTool.dependsOnVariable(flwor,
                Array(positionVariable))) {
                positionVariable = null
              }
              changed = true
              break()
            }
          }
        }
      }
    }
    if (positionVariable == null) {
      val thisVar: Array[Binding] = Array(this.getRangeVariable)
      if (opt.isVariableReplaceableByDot(condition, thisVar)) {
        val replacement = new ContextItemExpression()
        val found       = ExpressionTool.inlineVariableReferences(
          condition,
          this.getRangeVariable,
          replacement)
        if (found) {
          var cit       = config.makeContextItemStaticInfo(getSequence.getItemType, maybeUndefined = true)
          var predicate = condition.typeCheck(visitor, cit)
          val rel = th.relationship(predicate.getItemType, BuiltInAtomicType.INTEGER)
          if (rel != Affinity.DISJOINT) {
            val rsc = new RetainedStaticContext(visitor.getStaticContext)
            predicate = SystemFunction.makeCall("boolean", rsc, predicate)
            assert(predicate != null)
          }
          selection = new FilterExpression(selection, predicate)
          ExpressionTool.copyLocationInfo(predicate, selection)
          cit = config.makeContextItemStaticInfo(selectionContextItemType, maybeUndefined = true)
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
        val path = ExpressionTool.makePathExpression(head, selection)
        if (path.isInstanceOf[SlashExpression]) {
          ExpressionTool.copyLocationInfo(condition, path)
          val k: Expression = visitor
            .obtainOptimizer()
            .convertPathExpressionToKey(path.asInstanceOf[SlashExpression],
              visitor)
          if (k == null)
            this.setSequence(path)
          else
            this.setSequence(k)
          sequenceOp.typeCheck(visitor, contextItemType)
          sequenceOp.optimize(visitor, contextItemType)
        }
      }
    }
    changed
  }

  def processOperands(processor: OperandProcessor): Unit =
    processor.processOperand(sequenceOp)

  override def gatherVariableReferences(
                                         visitor: ExpressionVisitor,
                                         binding: Binding,
                                         references: List[VariableReference]): Unit = {
    ExpressionTool.gatherVariableReferences(getSequence, binding, references)
  }

  override def refineVariableType(visitor: ExpressionVisitor,
                                  references: List[VariableReference],
                                  returnExpr: Expression): Unit = {

    var actualItemType = getSequence.getItemType
    if (actualItemType eq ErrorType)
      actualItemType = AnyItemType

    for (ref <- references.asScala) {
      ref.refineVariableType(actualItemType,
        if (allowsEmpty) StaticProperty.ALLOWS_ZERO_OR_ONE
        else StaticProperty.EXACTLY_ONE,
        null,
        getSequence.getSpecialProperties
      )
    }
  }

  def addToPathMap(pathMap: PathMap,
                            pathMapNodeSet: PathMap.PathMapNodeSet): Unit = {
    val varPath = getSequence.addToPathMap(pathMap, pathMapNodeSet)
    pathMap.registerPathForVariable(rangeVariable, varPath)
  }

  def explain(out: ExpressionPresenter): Unit = {
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

  override def toShortString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
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
    fsb.append(getSequence.toShortString)
    fsb.toString
  }

  override def toString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
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
