package net.sf.saxon.expr.instruct

import java.util._

import net.sf.saxon.event.Outputter
import net.sf.saxon.expr.Expression._
import net.sf.saxon.expr._
import net.sf.saxon.expr.instruct.Block._
import net.sf.saxon.expr.parser._
import net.sf.saxon.model._
import net.sf.saxon.om.{AxisInfo, Item, SequenceIterator}
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.{EmptyIterator, ListIterator}
import net.sf.saxon.value._

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

object Block {

  def makeBlock(e1: Expression, e2: Expression): Expression = {
    if (e1 == null || Literal.isEmptySequence(e1)) {
      return e2
    }
    if (e2 == null || Literal.isEmptySequence(e2)) {
      return e1
    }
    if (e1.isInstanceOf[Block] || e2.isInstanceOf[Block]) {
      val list: List[Expression] = new ArrayList[Expression](10)
      if (e1.isInstanceOf[Block]) {
        for (o <- e1.operands().asScala) {
          list.add(o.getChildExpression)
        }
      } else {
        list.add(e1)
      }
      if (e2.isInstanceOf[Block]) {
        for (o <- e2.operands().asScala) {
          list.add(o.getChildExpression)
        }
      } else {
        list.add(e2)
      }
      var exps: Array[Expression] = Array.ofDim[Expression](list.size)
      exps = list.toArray(exps)
      new Block(exps)
    } else {
      val exps: Array[Expression] = Array(e1, e2)
      new Block(exps)
    }
  }

  def makeBlock(list: List[Expression]): Expression =
    if (list.isEmpty) {
      Literal.makeEmptySequence()
    } else if (list.size == 1) {
      list.get(0)
    } else {
      var exps: Array[Expression] = Array.ofDim[Expression](list.size)
      exps = list.toArray(exps)
      new Block(exps)
    }

  def neverReturnsTypedNodes(insn: Instruction, th: TypeHierarchy): Boolean = {
    for (o <- insn.operands().asScala) {
      val exp: Expression = o.getChildExpression
      if (!exp.hasSpecialProperty(StaticProperty.ALL_NODES_UNTYPED)) {
        val it: ItemType = exp.getItemType
        if (th.relationship(it, NodeKindTest.ELEMENT) != Affinity.DISJOINT ||
          th.relationship(it, NodeKindTest.ATTRIBUTE) != Affinity.DISJOINT) {
          false
        }
      }
    }
    true
  }

}

class Block(children: Array[Expression]) extends Instruction {

  @BeanProperty
  val operanda: Array[Operand] = new Array[Operand](children.length)

  private var allNodesUntyped: Boolean = _

  for (i <- 0 until children.length) {
    operanda(i) = new Operand(this, children(i), OperandRole.SAME_FOCUS_ACTION)
  }

  for (e <- children) {
    adoptChildExpression(e)
  }

  override def isInstruction(): Boolean = false

  private def child(n: Int): Expression = operanda(n).getChildExpression

  private def setChild(n: Int, child: Expression): Unit = {
    operanda(n).setChildExpression(child)
  }

  private def size(): Int = operanda.length

  override def operands(): java.lang.Iterable[Operand] =
    Arrays.asList(operanda: _*)

  override def hasVariableBinding(binding: Binding): Boolean = {
    if (binding.isInstanceOf[LocalParam]) {
      for (o <- operanda if o.getChildExpression eq binding) {
        return true
      }
    }
    false
  }

  override def getExpressionName(): String = "sequence"

  override def computeSpecialProperties(): Int = {
    if (size == 0) {
      StaticProperty.SPECIAL_PROPERTY_MASK & ~StaticProperty.HAS_SIDE_EFFECTS
    }
    var p: Int = super.computeSpecialProperties()
    if (allNodesUntyped) {
      p |= StaticProperty.ALL_NODES_UNTYPED
    }
    var allAxisExpressions: Boolean = true
    var allChildAxis: Boolean = true
    var allSubtreeAxis: Boolean = true
    breakable {
      for (o <- operands().asScala) {
        val child = o.getChildExpression
        if (!(child.isInstanceOf[AxisExpression])) {
          allAxisExpressions = false
          allChildAxis = false
          allSubtreeAxis = false
          break()
        }
        val axis: Int = child.asInstanceOf[AxisExpression].getAxis
        if (axis != AxisInfo.CHILD) {
          allChildAxis = false
        }
        if (!AxisInfo.isSubtreeAxis(axis)) {
          allSubtreeAxis = false
        }
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
        child(0)
          .asInstanceOf[AxisExpression]
          .getAxis == AxisInfo.ATTRIBUTE &&
        child(1).asInstanceOf[AxisExpression].getAxis == AxisInfo.CHILD) {
        p |= StaticProperty.ORDERED_NODESET
      }
    }
    p
  }

  override def implementsStaticTypeCheck(): Boolean = true

  override def staticTypeCheck(req: SequenceType,
                               backwardsCompatible: Boolean,
                               role: RoleDiagnostic,
                               visitor: ExpressionVisitor): Expression = {
    val tc: TypeChecker =
      visitor.getConfiguration.getTypeChecker(backwardsCompatible)
    if (backwardsCompatible && !Cardinality.allowsMany(req.getCardinality)) {
      val first: Expression = FirstItemExpression.makeFirstItemExpression(this)
      tc.staticTypeCheck(first, req, role, visitor)
    }
    val checked: Array[Expression] = Array.ofDim[Expression](operanda.length)
    var subReq: SequenceType = req
    if (req.getCardinality != StaticProperty.ALLOWS_ZERO_OR_MORE) {
      subReq = SequenceType.makeSequenceType(
        req.getPrimaryType,
        StaticProperty.ALLOWS_ZERO_OR_MORE)
    }
    for (i <- 0 until operanda.length) {
      checked(i) = tc.staticTypeCheck(operanda(i).getChildExpression,
        subReq,
        role,
        visitor)
    }
    val b2: Block = new Block(checked)
    ExpressionTool.copyLocationInfo(this, b2)
    b2.allNodesUntyped = allNodesUntyped
    val reqCard: Int = req.getCardinality
    val suppliedCard: Int = b2.getCardinality
    if (!Cardinality.subsumes(req.getCardinality, suppliedCard)) {
      if ((reqCard & suppliedCard) == 0) {
        val err = new XPathException(
          "The required cardinality of the " + role.getMessage +
            " is " +
            Cardinality.toString(reqCard) +
            ", but the supplied cardinality is " +
            Cardinality.toString(suppliedCard),
          role.getErrorCode,
          getLocation
        )
        err.setIsTypeError(true)
        err.setFailingExpression(this)
        throw err
      } else {
        CardinalityChecker.makeCardinalityChecker(b2, reqCard, role)
      }
    }
    b2
  }

  def mergeAdjacentTextInstructions(): Expression = {
    val isLiteralText: Array[Boolean] = Array.ofDim[Boolean](size)
    var hasAdjacentTextNodes: Boolean = false
    for (i <- 0 until size) {
      isLiteralText(i) = child(i).isInstanceOf[ValueOf] &&
        child(i)
          .asInstanceOf[ValueOf]
          .getSelect
          .isInstanceOf[StringLiteral] &&
        !child(i).asInstanceOf[ValueOf].isDisableOutputEscaping
      if (i > 0 && isLiteralText(i) && isLiteralText(i - 1)) {
        hasAdjacentTextNodes = true
      }
    }
    if (hasAdjacentTextNodes) {
      val content: List[Expression] = new ArrayList[Expression](size)
      var pendingText: String = null
      for (i <- 0 until size) {
        if (isLiteralText(i)) {
          pendingText = (if (pendingText == null) "" else pendingText) +
            child(i)
              .asInstanceOf[ValueOf]
              .getSelect
              .asInstanceOf[StringLiteral]
              .getStringValue
        } else {
          if (pendingText != null) {
            val inst: ValueOf =
              new ValueOf(new StringLiteral(pendingText), false, false)
            content.add(inst)
            pendingText = null
          }
          content.add(child(i))
        }
      }
      if (pendingText != null) {
        val inst: ValueOf =
          new ValueOf(new StringLiteral(pendingText), false, false)
        content.add(inst)
      }
      makeBlock(content)
    } else {
      this
    }
  }

  def copy(rebindings: RebindingMap): Expression = {
    val c2: Array[Expression] = Array.ofDim[Expression](size)
    for (c <- 0 until size) {
      c2(c) = child(c).copy(rebindings)
    }
    val b2: Block = new Block(c2)
    for (c <- 0 until size) {
      b2.adoptChildExpression(c2(c))
    }
    b2.allNodesUntyped = allNodesUntyped
    ExpressionTool.copyLocationInfo(this, b2)
    b2
  }

  override def getItemType(): ItemType = {

    if (size == 0)
      return ErrorType

    var t1: ItemType = null
    val th: TypeHierarchy = getConfiguration.getTypeHierarchy
    for (i <- 0 until size) {
      val child1: Expression = child(i)
      /*if (!(child1.isInstanceOf[Message])) {*/
      // Message class does not exist
      val t = child1.getItemType
      t1 =
        if (t1 == null)
          t
        else Type.getCommonSuperType(t1, t, th)
      if (t1 eq AnyItemType) {
        return t1 // no point going any further
      }
      // }
    }
    if (t1 == null)
      ErrorType
    else
      t1
  }

  override def getStaticUType(contextItemType: UType): UType =
    if (isInstruction) {
      super.getStaticUType(contextItemType)
    } else {
      if (size == 0)
        return UType.VOID

      var t1 = child(0).getStaticUType(contextItemType)
      for (i <- 1 until size) {
        t1 = t1.union(child(i).getStaticUType(contextItemType))
        if (t1 == UType.ANY)
          return t1
      }
      t1
    }

  override def computeCardinality(): Int = {
    if (size == 0) {
      StaticProperty.EMPTY
    }
    var c1: Int = child(0).getCardinality
    breakable {
      for (i <- 1 until size) {
        c1 = Cardinality.sum(c1, child(i).getCardinality)
        if (c1 == StaticProperty.ALLOWS_MANY) {
          break()
        }
      }
    }
    c1
  }

  override def mayCreateNewNodes(): Boolean = someOperandCreatesNewNodes()

  override def checkForUpdatingSubexpressions(): Unit = {
    if (size < 2) {
      return
    }
    var updating: Boolean = false
    var nonUpdating: Boolean = false
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      if (ExpressionTool.isNotAllowedInUpdatingContext(child)) {
        if (updating) {
          val err = new XPathException(
            "If any subexpression is updating, then all must be updating",
            "XUST0001")
          err.setLocation(child.getLocation)
          throw err
        }
        nonUpdating = true
      }
      if (child.isUpdatingExpression) {
        if (nonUpdating) {
          val err = new XPathException(
            "If any subexpression is updating, then all must be updating",
            "XUST0001")
          err.setLocation(child.getLocation)
          throw err
        }
        updating = true
      }
    }
  }

  override def isVacuousExpression(): Boolean = {
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      if (! child.isVacuousExpression)
        return false
    }
    true
  }

  override def simplify(): Expression = {
    var allAtomic: Boolean = true
    var nested: Boolean = false
    for (c <- 0 until size) {
      setChild(c, child(c).simplify())
      if (!Literal.isAtomic(child(c))) {
        allAtomic = false
      }
      if (child(c).isInstanceOf[Block]) {
        nested = true
      } else if (Literal.isEmptySequence(child(c))) {
        nested = true
      }
    }
    if (size == 1) {
      val e: Expression = getOperanda()(0).getChildExpression
      e.setParentExpression(getParentExpression)
      e
    } else if (size == 0) {
      val result: Expression = Literal.makeEmptySequence()
      ExpressionTool.copyLocationInfo(this, result)
      result.setParentExpression(getParentExpression)
      result
    } else if (nested) {
      val list: List[Expression] = new ArrayList[Expression](size * 2)
      flatten(list)
      val children: Array[Expression] = Array.ofDim[Expression](list.size)
      for (i <- 0 until list.size) {
        children(i) = list.get(i)
      }
      val newBlock: Block = new Block(children)
      ExpressionTool.copyLocationInfo(this, newBlock)
      newBlock.simplify()
    } else if (allAtomic) {
      val values: Array[AtomicValue] = Array.ofDim[AtomicValue](size)
      for (c <- 0 until size) {
        values(c) = child(c)
          .asInstanceOf[Literal]
          .getValue
          .asInstanceOf[AtomicValue]
      }
      val result: Expression =
        Literal.makeLiteral(new SequenceExtent(values.asInstanceOf[Array[Item]]), this)
      result.setParentExpression(getParentExpression)
      result
    } else {
      this
    }
  }

  private def flatten(targetList: List[Expression]): Unit = {
    var currentLiteralList: List[Item] = null
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      if (Literal.isEmptySequence(child)) {} else if (child
        .isInstanceOf[Block]) {
        flushCurrentLiteralList(currentLiteralList, targetList)
        currentLiteralList = null
        child.asInstanceOf[Block].flatten(targetList)
      } else if (child.isInstanceOf[Literal] &&
        !(child
          .asInstanceOf[Literal]
          .getValue
          .isInstanceOf[IntegerRange])) {
        val iterator: SequenceIterator =
          child.asInstanceOf[Literal].getValue.iterate()
        if (currentLiteralList == null) {
          currentLiteralList = new ArrayList(10)
        }
        var item: Item = null
        while ({
          item = iterator.next()
          item
        } != null) currentLiteralList.add(item)
      } else {
        flushCurrentLiteralList(currentLiteralList, targetList)
        currentLiteralList = null
        targetList.add(child)
      }
    }
    flushCurrentLiteralList(currentLiteralList, targetList)
  }

  private def flushCurrentLiteralList(currentLiteralList: List[Item],
                                      list: List[Expression]): Unit =
    if (currentLiteralList != null) {
      val iter: ListIterator[Item] = new ListIterator[Item](currentLiteralList)
      val lit: Literal = Literal.makeLiteral(iter.materialize(), this)
      list.add(lit)
    }

  def isCandidateForSharedAppend(): Boolean = {
    for (o <- operands().asScala) {
      val exp: Expression = o.getChildExpression
      if (exp.isInstanceOf[VariableReference] || exp.isInstanceOf[Literal]) {
        return true
      }
    }
    false
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    if (neverReturnsTypedNodes(this,
      visitor.getConfiguration.getTypeHierarchy)) {
      resetLocalStaticProperties()
      allNodesUntyped = true
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextInfo)
    var canSimplify: Boolean = false
    var prevLiteral: Boolean = false
    breakable {
      for (o <- operands().asScala) {
        val child = o.getChildExpression
        if (child.isInstanceOf[Block]) {
          canSimplify = true
          break()
        }
        if (child.isInstanceOf[Literal] &&
          !(child.asInstanceOf[Literal].getValue.isInstanceOf[IntegerRange])) {
          if (prevLiteral || Literal.isEmptySequence(child)) {
            canSimplify = true
            break()
          }
          prevLiteral = true
        } else {
          prevLiteral = false
        }
      }
    }
    if (canSimplify) {
      val list: List[Expression] = new ArrayList[Expression](size * 2)
      flatten(list)
      val result: Expression = Block.makeBlock(list)
      result.setRetainedStaticContext(getRetainedStaticContext)
      return result
    }
    if (size == 0) {
      Literal.makeEmptySequence()
    } else if (size == 1) {
      child(0)
    } else {
      this
    }
  }

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit =
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      child.checkPermittedContents(parentType, whole = false)
    }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("sequence", this)
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      child.export(out)
    }
    out.endElement()
  }

  override def toShortString(): String =
    "(" + child(0).toShortString() + ", ...)"

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    var tc: TailCall = null
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      try child match {
        case returner: TailCallReturner =>
          tc = returner.processLeavingTail(output, context)
        case _ =>
          child.process(output, context)
          tc = null
      } catch {
        case e: XPathException => {
          e.maybeSetLocation(child.getLocation)
          e.maybeSetContext(context)
          throw e
        }
      }
    }
    tc
  }

  override def getImplementationMethod(): Int = ITERATE_METHOD | PROCESS_METHOD

  override def iterate(context: XPathContext): SequenceIterator =
    if (size == 0)
      EmptyIterator.emptyIterator()
    else if (size == 1)
      child(0).iterate(context)
    else
      new BlockIterator(operanda, context)

  override def evaluatePendingUpdates(context: XPathContext,
                                      pul: PendingUpdateList): Unit =
    for (o <- operands().asScala) {
      val child = o.getChildExpression
      child.evaluatePendingUpdates(context, pul)
    }

  override def getStreamerName(): String = "Block"
}
