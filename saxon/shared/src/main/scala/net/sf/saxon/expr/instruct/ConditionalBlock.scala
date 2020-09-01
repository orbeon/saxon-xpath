package net.sf.saxon.expr.instruct

import java.util.{ArrayList, Arrays, List}

import net.sf.saxon.event.{Outputter, SignificantItemDetector}
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser.{ContextItemStaticInfo, ExpressionTool, ExpressionVisitor, RebindingMap}
import net.sf.saxon.model._
import net.sf.saxon.om.{Action, AxisInfo}
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.Cardinality

import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

class ConditionalBlock(children: Array[Expression]) extends Instruction {

  private val operanda: Array[Operand] = new Array[Operand](children.length)

  private var allNodesUntyped: Boolean = _

  for (i <- children.indices)
    operanda(i) = new Operand(this, children(i), OperandRole.SAME_FOCUS_ACTION)

  def this(children: List[Expression]) =
    this(children.toArray(Array.ofDim[Expression](children.size)))

  def getChildExpression(n: Int): Expression = operanda(n).getChildExpression

  def size(): Int = operanda.length

  override def operands(): java.lang.Iterable[Operand] =
    Arrays.asList(operanda: _*)

  override def getExpressionName(): String = "condSeq"

  override def computeSpecialProperties(): Int = {

    if (size == 0)
      return StaticProperty.SPECIAL_PROPERTY_MASK & ~StaticProperty.HAS_SIDE_EFFECTS

    var p = super.computeSpecialProperties()
    if (allNodesUntyped)
      p |= StaticProperty.ALL_NODES_UNTYPED

    var allAxisExpressions: Boolean = true
    var allChildAxis: Boolean = true
    var allSubtreeAxis: Boolean = true
    breakable {
      for (o <- operands().asScala) {
        val child = o.getChildExpression
        if (! child.isInstanceOf[AxisExpression]) {
          allAxisExpressions = false
          allChildAxis = false
          allSubtreeAxis = false
          break()
        }

        val axis = child.asInstanceOf[AxisExpression].getAxis
        if (axis != AxisInfo.CHILD)
          allChildAxis = false

        if (! AxisInfo.isSubtreeAxis(axis))
          allSubtreeAxis = false
      }
    }
    if (allAxisExpressions) {
      p |= StaticProperty.CONTEXT_DOCUMENT_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET |
        StaticProperty.NO_NODES_NEWLY_CREATED
      if (allChildAxis) {
        p |= StaticProperty.PEER_NODESET
      }
      if (allSubtreeAxis) {
        p |= StaticProperty.SUBTREE_NODESET
      }
      if (size == 2 &&
        getChildExpression(0).asInstanceOf[AxisExpression].getAxis == AxisInfo.ATTRIBUTE &&
        getChildExpression(1).asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD) {
        p |= StaticProperty.ORDERED_NODESET
      }
    }
    p
  }

  def copy(rebindings: RebindingMap): Expression = {

    val c2 = Array.ofDim[Expression](size)
    for (c <- 0 until size)
      c2(c) = getChildExpression(c).copy(rebindings)

    val b2 = new ConditionalBlock(c2)
    for (c <- 0 until size)
      b2.adoptChildExpression(c2(c))

    b2.allNodesUntyped = allNodesUntyped
    ExpressionTool.copyLocationInfo(this, b2)
    b2
  }

  override def getItemType(): ItemType = {

    if (size == 0)
      return ErrorType.getInstance

    var t1 = getChildExpression(0).getItemType
    val th = getConfiguration.getTypeHierarchy
    for (i <- 1 until size) {
      t1 = Type.getCommonSuperType(t1, getChildExpression(i).getItemType, th)
      if (t1 eq AnyItemType)
        return t1
    }
    t1
  }

  override def getCardinality(): Int = {

    if (size == 0)
      return StaticProperty.EMPTY

    var c1 = getChildExpression(0).getCardinality
    breakable {
      for (i <- 1 until size) {
        c1 = Cardinality.sum(c1, getChildExpression(i).getCardinality)
        if (c1 == StaticProperty.ALLOWS_ZERO_OR_MORE)
          break()
      }
    }
    c1
  }

  override def mayCreateNewNodes(): Boolean = someOperandCreatesNewNodes()

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    if (Block.neverReturnsTypedNodes(
      this,
      visitor.getConfiguration.getTypeHierarchy)) {
      resetLocalStaticProperties()
      allNodesUntyped = true
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val e = super.optimize(visitor, contextInfo)
    if (e != this)
      return e

    var lastOrdinaryInstruction: Int = -1
    var alwaysNonEmpty: Boolean = false
    var alwaysEmpty: Boolean = true
    breakable {
      for (c <- 0 until size if !(getChildExpression(c)
        .isInstanceOf[OnEmptyExpr] || getChildExpression(c)
        .isInstanceOf[OnNonEmptyExpr])) {
        lastOrdinaryInstruction = c
        if (getChildExpression(c).getItemType.getUType
          .intersection(UType.DOCUMENT.union(UType.TEXT)) == UType.VOID) {
          val card: Int = getChildExpression(c).getCardinality
          if (!Cardinality.allowsZero(card)) {
            alwaysNonEmpty = true
          }
          if (card != StaticProperty.ALLOWS_ZERO) {
            alwaysEmpty = false
          }
        } else {
          alwaysEmpty = false
          alwaysNonEmpty = false
          break()
        }
      }
    }
    if (alwaysEmpty) {
      visitor.getStaticContext.issueWarning(
        "The result of the sequence constructor will always be empty, so xsl:on-empty " +
          "instructions will always be evaluated, and xsl:on-non-empty instructions will never be evaluated",
        getLocation
      )
      val retain = new ArrayList[Expression]()
      for (c <- 0 until size)
        getChildExpression(c) match {
          case _: OnNonEmptyExpr =>
          case expr: OnEmptyExpr => retain.add(expr.getBaseExpression)
          case _                 => retain.add(getChildExpression(c))
        }
      Block.makeBlock(retain)
    }
    if (alwaysNonEmpty) {
      visitor.getStaticContext.issueWarning(
        "The result of the sequence constructor will never be empty, so xsl:on-empty " +
          "instructions will never be evaluated, and xsl:on-non-empty instructions will always be evaluated",
        getLocation
      )
      val retain = new ArrayList[Expression]()
      for (c <- 0 until size)
        getChildExpression(c) match {
          case _: OnEmptyExpr       =>
          case expr: OnNonEmptyExpr => retain.add(expr.getBaseExpression)
          case _                    => retain.add(getChildExpression(c))
        }
      Block.makeBlock(retain)
    }
    if (lastOrdinaryInstruction == -1) {
      val retain = new ArrayList[Expression]()
      for (c <- 0 until size if getChildExpression(c).isInstanceOf[OnEmptyExpr])
        retain.add(getChildExpression(c).asInstanceOf[OnEmptyExpr].getBaseExpression)
      Block.makeBlock(retain)
    }
    this
  }

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit =
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      child.checkPermittedContents(parentType, whole = false)
    }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("condSeq", this)
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      child.export(out)
    }
    out.endElement()
  }

  override def toShortString(): String =
    "(" + getChildExpression(0).toShortString() + ", ...)"

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val onNonEmptyPending = new ArrayList[OnNonEmptyExpr]()
    val action: Action = () =>
      for (e <- onNonEmptyPending.asScala)
        e.process(output, context)

    val significantItemDetector =
      new SignificantItemDetector(output, action)

    for (o <- operands().asScala) {
      val child = o.getChildExpression
      try child match {
        case _: OnEmptyExpr =>
        case expr: OnNonEmptyExpr =>
          if (significantItemDetector.isEmpty) {
            onNonEmptyPending.add(expr)
          } else {
            child.process(output, context)
          }
        case _ =>
          child.process(significantItemDetector, context)
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(child.getLocation)
          e.maybeSetContext(context)
          throw e
        }
      }
    }
    if (significantItemDetector.isEmpty)
      for (o <- operands().asScala) {
        val child = o.getChildExpression
        if (child.isInstanceOf[OnEmptyExpr]) {
          child.process(output, context)
        }
      }
    null
  }

  override def getImplementationMethod(): Int = Expression.PROCESS_METHOD
  override def getStreamerName(): String = "ConditionalBlock"

}
