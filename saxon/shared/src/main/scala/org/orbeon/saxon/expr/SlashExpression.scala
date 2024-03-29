package org.orbeon.saxon.expr


import java.{util => ju}

import org.orbeon.saxon.expr.instruct.CopyOf
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.expr.sort.DocumentSorter
import org.orbeon.saxon.functions.{Doc, DocumentFn, KeyFn}
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{AxisInfo, NodeInfo, SequenceIterator}
import org.orbeon.saxon.pattern._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.{Cardinality, IntegerValue, SequenceType}


class SlashExpression(start: Expression, step: Expression)
  extends BinaryExpression(start, Token.SLASH, step)
    with ContextSwitchingExpression {

  var contextFree: Boolean = _

  override def getOperandRole(arg: Int): OperandRole =
    if (arg == 0)
      OperandRole.FOCUS_CONTROLLING_SELECT
    else
      OperandRole.FOCUS_CONTROLLED_ACTION

  def getStart: Expression = getLhsExpression

  def setStart(start: Expression): Unit =
    this.setLhsExpression(start)

  def getStep: Expression = getRhsExpression

  def setStep(step: Expression): Unit =
    this.setRhsExpression(step)

  override def getExpressionName: String = "pathExpression"

  def getSelectExpression: Expression = getStart
  def getActionExpression: Expression = getStep

  def getItemType: ItemType = getStep.getItemType

  override def getStaticUType(contextItemType: UType): UType =
    getStep.getStaticUType(getStart.getStaticUType(contextItemType))

  override def getIntegerBounds: Array[IntegerValue] =
    getStep.getIntegerBounds

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {

    getLhs.typeCheck(visitor, contextInfo)

    if (Literal.isEmptySequence(getStart))
      return getStart

    val config = visitor.getConfiguration
    val tc = config.getTypeChecker(false)

    val role0 = new RoleDiagnostic(RoleDiagnostic.BINARY_EXPR, "/", 0)
    role0.setErrorCode("XPTY0019")
    setStart(tc.staticTypeCheck(getStart, SequenceType.NODE_SEQUENCE, role0, visitor))

    val startType = getStart.getItemType
    if (startType == ErrorType)
      return Literal.makeEmptySequence

    val cit = config.makeContextItemStaticInfo(startType, maybeUndefined = false)
    cit.setContextSettingExpression(getStart)

    getRhs.typeCheck(visitor, cit)

    val e2 = simplifyDescendantPath(visitor.getStaticContext)
    if (e2 != null)
      return e2.typeCheck(visitor, contextInfo)
    if (getStart.isInstanceOf[ContextItemExpression] && getStep.hasSpecialProperty(StaticProperty.ORDERED_NODESET))
      return getStep
    if (getStep.isInstanceOf[ContextItemExpression] && getStart.hasSpecialProperty(StaticProperty.ORDERED_NODESET))
      return getStart

    getStep match {
      case expression: AxisExpression if config.getTypeHierarchy.isSubType(startType, getStep.getItemType) && expression.getAxis == AxisInfo.SELF =>
        getStart
      case _ =>
        this
    }
  }

  def simplifyDescendantPath(env: StaticContext): SlashExpression = {
    var underlyingStep = getStep
    while (underlyingStep.isInstanceOf[FilterExpression]) {
      if (underlyingStep.asInstanceOf[FilterExpression].isPositional(env.getConfiguration.getTypeHierarchy))
        return null
      underlyingStep = underlyingStep.asInstanceOf[FilterExpression].getSelectExpression
    }

    if (! underlyingStep.isInstanceOf[AxisExpression])
      return null

    var st: Expression = getStart
    st match {
      case axisExpression: AxisExpression =>
        if (axisExpression.getAxis != AxisInfo.DESCENDANT_OR_SELF)
          return null
        val cie = new ContextItemExpression
        ExpressionTool.copyLocationInfo(this, cie)
        st = ExpressionTool.makePathExpression(cie, axisExpression.copy(new RebindingMap()))
        ExpressionTool.copyLocationInfo(this, st)
      case _ =>
    }

    if (! st.isInstanceOf[SlashExpression])
      return null

    val startPath = st.asInstanceOf[SlashExpression]
    if (! startPath.getStep.isInstanceOf[AxisExpression])
      return null

    val mid = startPath.getStep.asInstanceOf[AxisExpression]
    if (mid.getAxis != AxisInfo.DESCENDANT_OR_SELF)
      return null

    val test = mid.getNodeTest
    if (!(test == null || test.isInstanceOf[AnyNodeTest.type]))
      return null

    val underlyingAxis = underlyingStep.asInstanceOf[AxisExpression].getAxis
    if (underlyingAxis == AxisInfo.CHILD || underlyingAxis == AxisInfo.DESCENDANT || underlyingAxis == AxisInfo.DESCENDANT_OR_SELF) {

      val newAxis =
        if (underlyingAxis == AxisInfo.DESCENDANT_OR_SELF)
          AxisInfo.DESCENDANT_OR_SELF
        else
          AxisInfo.DESCENDANT

      var newStep: Expression = new AxisExpression(
        newAxis,
        underlyingStep.asInstanceOf[AxisExpression].getNodeTest)
      ExpressionTool.copyLocationInfo(this, newStep)
      underlyingStep = getStep
      var filters: List[Expression] = Nil
      while (underlyingStep.isInstanceOf[FilterExpression]) {
        filters ::= underlyingStep.asInstanceOf[FilterExpression].getFilter
        underlyingStep = underlyingStep.asInstanceOf[FilterExpression].getSelectExpression
      }
      while (filters.nonEmpty) {
        newStep = new FilterExpression(newStep, { val r = filters.head; filters = filters.tail; r })
        ExpressionTool.copyLocationInfo(getStep, newStep)
      }

      val newPath = ExpressionTool.makePathExpression(startPath.getStart, newStep)

      if (! newPath.isInstanceOf[SlashExpression])
        return null

      ExpressionTool.copyLocationInfo(this, newPath)
      newPath.asInstanceOf[SlashExpression]
    } else if (underlyingAxis == AxisInfo.ATTRIBUTE) {
      val newStep = new AxisExpression(AxisInfo.DESCENDANT_OR_SELF, NodeKindTest.ELEMENT)
      ExpressionTool.copyLocationInfo(this, newStep)
      val e2 = ExpressionTool.makePathExpression(startPath.getStart, newStep)
      val e3 = ExpressionTool.makePathExpression(e2, getStep)
      if (! e3.isInstanceOf[SlashExpression])
        return null

      ExpressionTool.copyLocationInfo(this, e3)
      e3.asInstanceOf[SlashExpression]
    } else
      null
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {

    val config = visitor.getConfiguration
    val th = config.getTypeHierarchy
    val opt = visitor.obtainOptimizer()
    getLhs.optimize(visitor, contextItemType)
    val cit = visitor.getConfiguration.makeContextItemStaticInfo(getStart.getItemType, maybeUndefined = false)
    cit.setContextSettingExpression(getStart)
    getRhs.optimize(visitor, cit)

    if (Literal.isEmptySequence(getStart) || Literal.isEmptySequence(getStep))
      return Literal.makeEmptySequence

    if (getStart.isInstanceOf[RootExpression] && th.isSubType(contextItemType.getItemType, NodeKindTest.DOCUMENT))
      return getStep

    var e2: Expression = simplifyDescendantPath(visitor.getStaticContext)
    if (e2 != null)
      return e2.optimize(visitor, contextItemType)

    val firstStep = getFirstStep
    if (! (firstStep.isCallOn(classOf[Doc]) || firstStep.isCallOn(classOf[DocumentFn]))) {
      getLastStep match {
        case expression: FilterExpression if ! expression.isPositional(th) =>
          val leading = getLeadingSteps
          val p2 = ExpressionTool.makePathExpression(leading, expression.getSelectExpression)
          val f2 = new FilterExpression(p2, expression.getFilter)
          ExpressionTool.copyLocationInfo(this, f2)
          return f2.optimize(visitor, contextItemType)
        case _ =>
      }
    }
    if (! visitor.isOptimizeForStreaming) {
      val k = opt.convertPathExpressionToKey(this, visitor)
      if (k != null)
        return k.typeCheck(visitor, contextItemType).optimize(visitor, contextItemType)
    }

    e2 = tryToMakeSorted(visitor, contextItemType)
    if (e2 != null)
      return e2

    if (getStep.isInstanceOf[AxisExpression]) {
      if (! Cardinality.allowsMany(getStart.getCardinality)) {
        val sse = new SimpleStepExpression(getStart, getStep)
        ExpressionTool.copyLocationInfo(this, sse)
        sse.setParentExpression(getParentExpression)
        return sse
      } else {
        contextFree = true
      }
    }
    if (getStart.isInstanceOf[RootExpression] && getStep.isCallOn(classOf[KeyFn])) {
      val keyCall = getStep.asInstanceOf[SystemFunctionCall]
      if (keyCall.getArity == 3 &&
        keyCall.getArg(2).isInstanceOf[ContextItemExpression]) {
        keyCall.setArg(2, new RootExpression())
        keyCall.setParentExpression(getParentExpression)
        ExpressionTool.resetStaticProperties(keyCall)
        return keyCall
      }
    }
    val k = promoteFocusIndependentSubexpressions(visitor, contextItemType)
    if (k != this)
      return k
    if (visitor.isOptimizeForStreaming) {
      val rawStep = ExpressionTool.unfilteredExpression(getStep, allowPositional = true)
      rawStep match {
        case copyOf: CopyOf if copyOf.getSelect.isInstanceOf[ContextItemExpression] =>
          copyOf.setSelect(getStart)
          rawStep.resetLocalStaticProperties()
          getStep.resetLocalStaticProperties()
          return getStep
        case _ =>
      }
    }
    this
  }

  def tryToMakeAbsolute(): SlashExpression = {

    val first = getFirstStep
    if (first.getItemType.getPrimitiveType == Type.DOCUMENT)
      return this

    first match {
      case axisExpression: AxisExpression =>
        val contextItemType = axisExpression.getContextItemType
        if (contextItemType != null && contextItemType.getPrimitiveType == Type.DOCUMENT) {
          val root = new RootExpression
          ExpressionTool.copyLocationInfo(this, root)
          val path = ExpressionTool.makePathExpression(root, this.copy(new RebindingMap()))
          if (! path.isInstanceOf[SlashExpression])
            return null
          ExpressionTool.copyLocationInfo(this, path)
          return path.asInstanceOf[SlashExpression]
        }
      case _ =>
    }
    first match {
      case sorter: DocumentSorter if sorter.getBaseExpression.isInstanceOf[SlashExpression] =>
        val se = sorter.getBaseExpression.asInstanceOf[SlashExpression]
        val se2 = se.tryToMakeAbsolute()
        if (se2 != null) {
          if (se2 == se) {
            return this
          } else {
            val rest: Expression = getRemainingSteps
            val ds: DocumentSorter = new DocumentSorter(se2)
            return new SlashExpression(ds, rest)
          }
        }
      case _ =>
    }
    null
  }

  override def getCost: Double = {
    val factor = if (Cardinality.allowsMany(getLhsExpression.getCardinality)) 5 else 1
    val lh = getLhsExpression.getCost + 1
    val rh = getRhsExpression.getCost
    val product = lh + factor * rh
    Math.max(product, Expression.MAX_COST)
  }

  def tryToMakeSorted(visitor: ExpressionVisitor,
                      contextItemType: ContextItemStaticInfo): Expression = {
    val config = visitor.getConfiguration
    val th = config.getTypeHierarchy
    val opt = visitor.obtainOptimizer()
    val s1 = ExpressionTool.unfilteredExpression(getStart, allowPositional = false)
    if (! (s1.isInstanceOf[AxisExpression] &&
      s1.asInstanceOf[AxisExpression].getAxis == AxisInfo.DESCENDANT)) {
      return null
    }
    val s2 = ExpressionTool.unfilteredExpression(getStep, allowPositional = false)
    if (!(s2.isInstanceOf[AxisExpression] &&
      s2.asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD)) {
      return null
    }
    val x = getStart.copy(new RebindingMap())
    val ax = ExpressionTool
      .unfilteredExpression(x, allowPositional = false)
      .asInstanceOf[AxisExpression]
    ax.setAxis(AxisInfo.PARENT)
    val y = getStep.copy(new RebindingMap())
    val ay = ExpressionTool
      .unfilteredExpression(y, allowPositional = false)
      .asInstanceOf[AxisExpression]
    ay.setAxis(AxisInfo.DESCENDANT)
    var k: Expression = new FilterExpression(y, x)
    if (! th.isSubType(contextItemType.getItemType, NodeKindTest.DOCUMENT)) {
      k = new SlashExpression(new AxisExpression(AxisInfo.CHILD, NodeKindTest.ELEMENT), k)
      ExpressionTool.copyLocationInfo(this, k)
      opt.trace(
        "Rewrote descendant::X/child::Y as child::*/descendant::Y[parent::X]",
        k)
    } else {
      ExpressionTool.copyLocationInfo(this, k)
      opt.trace("Rewrote descendant::X/child::Y as descendant::Y[parent::X]",
        k)
    }
    k
  }

  def promoteFocusIndependentSubexpressions(
                                             visitor: ExpressionVisitor,
                                             contextItemType: ContextItemStaticInfo): Expression = this

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    this.setStart(getStart.unordered(retainAllNodes, forStreaming))
    this.setStep(getStep.unordered(retainAllNodes, forStreaming))
    this
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val target = getStart.addToPathMap(pathMap, pathMapNodeSet)
    getStep.addToPathMap(pathMap, target)
  }

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  def copy(rebindings: RebindingMap): Expression = {
    val exp = ExpressionTool.makePathExpression(
      getStart.copy(rebindings),
      getStep.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override def computeSpecialProperties(): Int = {

    var startProperties = getStart.getSpecialProperties
    var stepProperties = getStep.getSpecialProperties

    if ((stepProperties & StaticProperty.ALL_NODES_NEWLY_CREATED) != 0)
      return StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET | StaticProperty.NO_NODES_NEWLY_CREATED

    var p = 0
    if (!Cardinality.allowsMany(getStart.getCardinality))
      startProperties |= StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET
    if (!Cardinality.allowsMany(getStep.getCardinality))
      stepProperties |= StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET
    if ((startProperties & stepProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0)
      p |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    if (((startProperties & StaticProperty.SINGLE_DOCUMENT_NODESET) != 0) && ((stepProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0))
      p |= StaticProperty.SINGLE_DOCUMENT_NODESET
    if ((startProperties & stepProperties & StaticProperty.PEER_NODESET) != 0)
      p |= StaticProperty.PEER_NODESET
    if ((startProperties & stepProperties & StaticProperty.SUBTREE_NODESET) != 0)
      p |= StaticProperty.SUBTREE_NODESET
    if (testNaturallySorted(startProperties, stepProperties))
      p |= StaticProperty.ORDERED_NODESET
    if (testNaturallyReverseSorted())
      p |= StaticProperty.REVERSE_DOCUMENT_ORDER
    if ((startProperties & stepProperties & StaticProperty.NO_NODES_NEWLY_CREATED) != 0)
      p |= StaticProperty.NO_NODES_NEWLY_CREATED
    p
  }

  private def testNaturallySorted(startProperties: Int,
                                  stepProperties: Int): Boolean = {
    if ((stepProperties & StaticProperty.ORDERED_NODESET) == 0)
      return false
    if (Cardinality.allowsMany(getStart.getCardinality)) {
      if ((startProperties & StaticProperty.ORDERED_NODESET) == 0)
        return false
    } else {
      return true
    }
    if ((stepProperties & StaticProperty.ATTRIBUTE_NS_NODESET) != 0)
      return true
    if ((stepProperties & StaticProperty.ALL_NODES_NEWLY_CREATED) != 0)
      return true
    ((startProperties & StaticProperty.PEER_NODESET) != 0) &&
      ((stepProperties & StaticProperty.SUBTREE_NODESET) != 0)
  }

  private def testNaturallyReverseSorted(): Boolean =
    if (! Cardinality.allowsMany(getStart.getCardinality) && getStep.isInstanceOf[AxisExpression]) {
      ! AxisInfo.isForwards(getStep.asInstanceOf[AxisExpression].getAxis)
    } else {
      ! Cardinality.allowsMany(getStep.getCardinality) &&
        getStart.isInstanceOf[AxisExpression] &&
        ! AxisInfo.isForwards(getStart.asInstanceOf[AxisExpression].getAxis)
    }

  override def computeCardinality(): Int = {
    val c1 = getStart.getCardinality
    val c2 = getStep.getCardinality
    Cardinality.multiply(c1, c2)
  }

  override def toPattern(config: Configuration): Pattern = {
    val head = getLeadingSteps
    val tail = getLastStep
    head match {
      case checker: ItemChecker =>
        if (checker.getBaseExpression.isInstanceOf[ContextItemExpression])
          return tail.toPattern(config)
      case _ =>
    }

    val tailPattern = tail.toPattern(config)
    tailPattern match {
      case _: NodeTestPattern =>
        if (tailPattern.getItemType eq ErrorType)
          return tailPattern
      case _: GeneralNodePattern =>
        return new GeneralNodePattern(this, tailPattern.getItemType.asInstanceOf[NodeTest])
      case _ =>
    }

    var axis = AxisInfo.PARENT
    var headPattern: Pattern = null
    head match {
      case start: SlashExpression =>
        start.getActionExpression match {
          case mid: AxisExpression =>
            if (mid.getAxis == AxisInfo.DESCENDANT_OR_SELF && (mid.getNodeTest == null || mid.getNodeTest.isInstanceOf[AnyNodeTest.type])) {
              axis = AxisInfo.ANCESTOR
              headPattern = start.getSelectExpression.toPattern(config)
            }
          case _ =>
        }
      case _ =>
    }
    if (headPattern == null) {
      axis = PatternMaker.getAxisForPathStep(tail)
      headPattern = head.toPattern(config)
    }
    new AncestorQualifiedPattern(tailPattern, headPattern, axis)
  }

  def isContextFree: Boolean = contextFree

  def setContextFree(free: Boolean): Unit =
    this.contextFree = free

  override def equals(other: Any): Boolean = {
    if (! other.isInstanceOf[SlashExpression])
      return false
    val p = other.asInstanceOf[SlashExpression]
    getStart.isEqual(p.getStart) && getStep.isEqual(p.getStep)
  }

  override def computeHashCode(): Int =
    "SlashExpression".hashCode + getStart.hashCode + getStep.hashCode

  override def iterate(context: XPathContext): SequenceIterator = {
    if (contextFree) {
      return new MappingIterator(getStart.iterate(context),
        item =>
          getRhsExpression
            .asInstanceOf[AxisExpression]
            .iterate(item.asInstanceOf[NodeInfo]))
    }
    val context2 = context.newMinorContext()
    context2.trackFocus(getStart.iterate(context))
    new ContextMappingIterator(c1 => getStep.iterate(c1), context2)
  }

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("slash", this)
    if (this.isInstanceOf[SimpleStepExpression]) {
      destination.emitAttribute("simple", "1")
    } else if (isContextFree) {
      destination.emitAttribute("simple", "2")
    }
    getStart.export(destination)
    getStep.export(destination)
    destination.endElement()
  }

  override def toString: String =
    ExpressionTool.parenthesize(getStart) + "/" + ExpressionTool.parenthesize(getStep)

  override def toShortString: String =
    ExpressionTool.parenthesizeShort(getStart) + "/" + ExpressionTool.parenthesizeShort(getStep)

  def getFirstStep: Expression =
    getStart match {
      case slashExpression: SlashExpression => slashExpression.getFirstStep
      case _                                => getStart
    }

  def getRemainingSteps: Expression =
    if (getStart.isInstanceOf[SlashExpression]) {
      val list = new ju.ArrayList[Expression](8)
      gatherSteps(list)
      val rem = rebuildSteps(list.subList(1, list.size))
      ExpressionTool.copyLocationInfo(this, rem)
      rem
    } else {
      getStep
    }

  private def gatherSteps(list: ju.List[Expression]): Unit = {
    getStart match {
      case slashExpression: SlashExpression => slashExpression.gatherSteps(list)
      case _                                => list.add(getStart)
    }
    getStep match {
      case slashExpression: SlashExpression => slashExpression.gatherSteps(list)
      case _                                => list.add(getStep)
    }
  }

  private def rebuildSteps(list: ju.List[Expression]): Expression =
    if (list.size == 1)
      list.get(0).copy(new RebindingMap())
    else
      new SlashExpression(list.get(0).copy(new RebindingMap()), rebuildSteps(list.subList(1, list.size)))

  def getLastStep: Expression =
    getStep match {
      case expression: SlashExpression => expression.getLastStep
      case _                           => getStep
    }

  def getLeadingSteps: Expression =
    if (getStep.isInstanceOf[SlashExpression]) {
      val list = new ju.ArrayList[Expression](8)
      gatherSteps(list)
      val rem = rebuildSteps(list.subList(0, list.size - 1))
      ExpressionTool.copyLocationInfo(this, rem)
      rem
    } else {
      getStart
    }

  def isAbsolute: Boolean =
    getFirstStep.getItemType.getPrimitiveType == Type.DOCUMENT

  override def getStreamerName: String = "ForEach"
}
