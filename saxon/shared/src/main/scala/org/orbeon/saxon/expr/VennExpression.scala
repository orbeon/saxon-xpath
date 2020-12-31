package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.instruct.Block
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.expr.sort.{DocumentSorter, GlobalOrderComparer}
import org.orbeon.saxon.functions.{CurrentGroupCall, SystemFunction}
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{AxisInfo, SequenceIterator}
import org.orbeon.saxon.pattern._
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.{Cardinality, SequenceType}

import java.util
import java.util.Set


class VennExpression(p1: Expression, val op: Int, p2: Expression) extends BinaryExpression(p1, op, p2) {

  override def simplify(): Expression = {
    if (! getLhsExpression.isInstanceOf[DocumentSorter]) {
      this.setLhsExpression(new DocumentSorter(getLhsExpression))
    }
    if (! getRhsExpression.isInstanceOf[DocumentSorter]) {
      this.setRhsExpression(new DocumentSorter(getRhsExpression))
    }
    super.simplify()
    this
  }

  override def getExpressionName: String = op match {
    case Token.UNION     => "union"
    case Token.INTERSECT => "intersect"
    case Token.EXCEPT    => "except"
    case _               => "unknown"
  }

  def getItemType: ItemType = {
    val t1 = getLhsExpression.getItemType
    if (op == Token.UNION) {
      val t2: ItemType = getRhsExpression.getItemType
      val th = getConfiguration.getTypeHierarchy
      Type.getCommonSuperType(t1, t2, th)
    } else {
      t1
    }
  }

  override def getStaticUType(contextItemType: UType): UType = op match {
    case Token.UNION =>
      getLhsExpression
        .getStaticUType(contextItemType)
        .union(getRhsExpression.getStaticUType(contextItemType))
    case Token.INTERSECT =>
      getLhsExpression
        .getStaticUType(contextItemType)
        .intersection(getRhsExpression.getStaticUType(contextItemType))
    case Token.EXCEPT => null
    case _ => getLhsExpression.getStaticUType(contextItemType)

  }

  override def computeCardinality(): Int = {
    val c1 = getLhsExpression.getCardinality
    val c2 = getRhsExpression.getCardinality
    op match {
      case Token.UNION =>
        if (Literal.isEmptySequence(getLhsExpression)) {
          return c2
        }
        if (Literal.isEmptySequence(getRhsExpression)) {
          return c1
        }
        c1 | c2 | StaticProperty.ALLOWS_ONE | StaticProperty.ALLOWS_MANY
      case Token.INTERSECT =>
        if (Literal.isEmptySequence(getLhsExpression)) {
          return StaticProperty.EMPTY
        }
        if (Literal.isEmptySequence(getRhsExpression)) {
          return StaticProperty.EMPTY
        }
        (c1 & c2) | StaticProperty.ALLOWS_ZERO | StaticProperty.ALLOWS_ONE
      case Token.EXCEPT =>
        if (Literal.isEmptySequence(getLhsExpression)) {
          return StaticProperty.EMPTY
        }
        if (Literal.isEmptySequence(getRhsExpression)) {
          return c1
        }
        c1 | StaticProperty.ALLOWS_ZERO | StaticProperty.ALLOWS_ONE
      case _ =>
        StaticProperty.ALLOWS_ZERO_OR_MORE
    }
  }

  override def computeSpecialProperties(): Int = {
    val prop0 = getLhsExpression.getSpecialProperties
    val prop1 = getRhsExpression.getSpecialProperties
    var props = StaticProperty.ORDERED_NODESET
    if (testContextDocumentNodeSet(prop0, prop1))
      props |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    if (testSubTree(prop0, prop1))
      props |= StaticProperty.SUBTREE_NODESET
    if (createsNoNewNodes(prop0, prop1))
      props |= StaticProperty.NO_NODES_NEWLY_CREATED
    props
  }

  private def testContextDocumentNodeSet(prop0: Int, prop1: Int): Boolean =
    op match {
      case Token.UNION =>
        (prop0 & prop1 & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0
      case Token.INTERSECT =>
        ((prop0 | prop1) & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0
      case Token.EXCEPT =>
        (prop0 & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0
      case _ =>
        false
    }

  def gatherComponents(op: Int, set: Set[Expression]): Unit = {
    getLhsExpression match {
      case vennExpression: VennExpression if vennExpression.op == op =>
        vennExpression.gatherComponents(op, set)
      case _=>
        set.add(getLhsExpression)
    }
    getRhsExpression match {
      case vennExpression: VennExpression if vennExpression.op == op =>
        vennExpression.gatherComponents(op, set)
      case _ =>
        set.add(getRhsExpression)
    }
  }

  private def testSubTree(prop0: Int, prop1: Int): Boolean =
    op match {
      case Token.UNION => (prop0 & prop1 & StaticProperty.SUBTREE_NODESET) != 0
      case Token.INTERSECT =>
        ((prop0 | prop1) & StaticProperty.SUBTREE_NODESET) != 0
      case Token.EXCEPT => (prop0 & StaticProperty.SUBTREE_NODESET) != 0
      case _ => false
    }

  private def createsNoNewNodes(prop0: Int, prop1: Int): Boolean =
    (prop0 & StaticProperty.NO_NODES_NEWLY_CREATED) != 0 &&
      (prop1 & StaticProperty.NO_NODES_NEWLY_CREATED) != 0

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val config = visitor.getConfiguration
    val tc     = config.getTypeChecker(false)
    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)
    if (! getLhsExpression.isInstanceOf[Pattern]) {
      val role0: RoleDiagnostic = new RoleDiagnostic(
        RoleDiagnostic.BINARY_EXPR,
        Token.tokens(op),
        0)
      this.setLhsExpression(tc.staticTypeCheck(getLhsExpression,
        SequenceType.NODE_SEQUENCE,
        role0,
        visitor))
    }
    if (! getRhsExpression.isInstanceOf[Pattern]) {
      val role1: RoleDiagnostic = new RoleDiagnostic(
        RoleDiagnostic.BINARY_EXPR,
        Token.tokens(op),
        1)
      this.setRhsExpression(tc.staticTypeCheck(getRhsExpression,
        SequenceType.NODE_SEQUENCE,
        role1,
        visitor))
    }
    if (op != Token.UNION) {
      val th = config.getTypeHierarchy
      val t0: ItemType = getLhsExpression.getItemType
      val t1: ItemType = getRhsExpression.getItemType
      if (th.relationship(t0, t1) == Affinity.DISJOINT) {
        if (op == Token.INTERSECT) {
          return Literal.makeEmptySequence
        } else {
          if (getLhsExpression.hasSpecialProperty(
            StaticProperty.ORDERED_NODESET)) {
            return getLhsExpression
          } else {
            return new DocumentSorter(getLhsExpression)
          }
        }
      }
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val e = super.optimize(visitor, contextItemType)
    if (e != this) {
      return e
    }
    val config: Configuration = visitor.getConfiguration
    val th = config.getTypeHierarchy
    val lhs: Expression = getLhsExpression
    val rhs: Expression = getRhsExpression
    op match {
      case Token.UNION =>
        if (Literal.isEmptySequence(lhs) &&
          (rhs.getSpecialProperties & StaticProperty.ORDERED_NODESET) !=
            0) {
          return rhs
        }
        if (Literal.isEmptySequence(rhs) &&
          (lhs.getSpecialProperties & StaticProperty.ORDERED_NODESET) !=
            0) {
          return lhs
        }
        if (lhs.isInstanceOf[CurrentGroupCall] && rhs
          .isInstanceOf[ContextItemExpression]) {
          return lhs
        }
      case Token.INTERSECT =>
        if (Literal.isEmptySequence(lhs)) {
          return lhs
        }
        if (Literal.isEmptySequence(rhs)) {
          return rhs
        }
        if (lhs.isInstanceOf[CurrentGroupCall] && rhs
          .isInstanceOf[ContextItemExpression]) {
          return rhs
        }
      case Token.EXCEPT =>
        if (Literal.isEmptySequence(lhs)) {
          return lhs
        }
        if (Literal.isEmptySequence(rhs) &&
          (lhs.getSpecialProperties & StaticProperty.ORDERED_NODESET) !=
            0) {
          return lhs
        }
        if (lhs.isInstanceOf[CurrentGroupCall] && rhs
          .isInstanceOf[ContextItemExpression]) {
          return new TailExpression(lhs, 2)
        }
      case _ =>
    }
    if (lhs.isInstanceOf[AxisExpression] && rhs.isInstanceOf[AxisExpression]) {
      val a1: AxisExpression = lhs.asInstanceOf[AxisExpression]
      val a2: AxisExpression = rhs.asInstanceOf[AxisExpression]
      if (a1.getAxis == a2.getAxis) {
        if (a1.getNodeTest == a2.getNodeTest) {
          if (op == Token.EXCEPT)
            return Literal.makeEmptySequence else return a1
        } else {
          val ax: AxisExpression = new AxisExpression(
            a1.getAxis,
            new CombinedNodeTest(a1.getNodeTest, op, a2.getNodeTest))
          ExpressionTool.copyLocationInfo(this, ax)
          return ax
        }
      }
    }
    if (lhs.isInstanceOf[SlashExpression] && rhs.isInstanceOf[SlashExpression] && op == Token.UNION) {
      val path1 = lhs.asInstanceOf[SlashExpression]
      val path2 = rhs.asInstanceOf[SlashExpression]
      if (path1.getFirstStep.isEqual(path2.getFirstStep)) {
        val venn = new VennExpression(path1.getRemainingSteps, op, path2.getRemainingSteps)
        ExpressionTool.copyLocationInfo(this, venn)
        val path = ExpressionTool.makePathExpression(path1.getFirstStep, venn)
        ExpressionTool.copyLocationInfo(this, path)
        return path.optimize(visitor, contextItemType)
      }
    }
    if (lhs.isInstanceOf[FilterExpression] && rhs.isInstanceOf[FilterExpression]) {
      val exp0 = lhs.asInstanceOf[FilterExpression]
      val exp1 = rhs.asInstanceOf[FilterExpression]
      if (! exp0.isPositional(th) && !exp1.isPositional(th) &&
        exp0.getSelectExpression.isEqual(exp1.getSelectExpression)) {
        var filter: Expression = null
        op match {
          case Token.UNION =>
            filter = new OrExpression(exp0.getFilter, exp1.getFilter)
          case Token.INTERSECT =>
            filter = new AndExpression(exp0.getFilter, exp1.getFilter)
          case Token.EXCEPT =>
            val negate2: Expression = SystemFunction.makeCall(
              "not",
              getRetainedStaticContext,
              exp1.getFilter
            )
            filter = new AndExpression(exp0.getFilter, negate2)
          case _ => throw new AssertionError("Unknown op " + op)
        }
        ExpressionTool.copyLocationInfo(this, filter)
        val f = new FilterExpression(exp0.getSelectExpression, filter)
        ExpressionTool.copyLocationInfo(this, f)
        return f.simplify()
          .typeCheck(visitor, contextItemType)
          .optimize(visitor, contextItemType)
      }
    }
    if (! visitor.isOptimizeForStreaming && op == Token.UNION &&
      lhs.isInstanceOf[AxisExpression] &&
      rhs.isInstanceOf[AxisExpression]) {
      val a0 = lhs.asInstanceOf[AxisExpression]
      val a1 = rhs.asInstanceOf[AxisExpression]
      if (a0.getAxis == AxisInfo.ATTRIBUTE && a1.getAxis == AxisInfo.CHILD) {
        return new Block(Array(lhs, rhs))
      } else if (a1.getAxis == AxisInfo.ATTRIBUTE && a0.getAxis == AxisInfo.CHILD) {
        return new Block(Array(rhs, lhs))
      }
    }
    if (op == Token.INTERSECT && !Cardinality.allowsMany(
      lhs.getCardinality)) {
      return new SingletonIntersectExpression(lhs,
        op,
        rhs.unordered(retainAllNodes = false, forStreaming = false))
    }
    if (op == Token.INTERSECT && !Cardinality.allowsMany(
      rhs.getCardinality)) {
      return new SingletonIntersectExpression(rhs,
        op,
        lhs.unordered(retainAllNodes = false, forStreaming = false))
    }
    if (operandsAreDisjoint(th)) {
      if (op == Token.INTERSECT) {
        return Literal.makeEmptySequence
      } else if (op == Token.EXCEPT) {
        if ((lhs.getSpecialProperties & StaticProperty.ORDERED_NODESET) !=
          0) {
          return lhs
        } else {
          return new DocumentSorter(lhs)
        }
      }
    }
    this
  }

  private def operandsAreDisjoint(th: TypeHierarchy): Boolean =
    th.relationship(getLhsExpression.getItemType, getRhsExpression.getItemType) == Affinity.DISJOINT

  override def unordered(retainAllNodes: Boolean, forStreaming: Boolean): Expression =
    if (op == Token.UNION && !forStreaming &&
      operandsAreDisjoint(getConfiguration.getTypeHierarchy)) {
      val block: Block = new Block(Array(getLhsExpression, getRhsExpression))
      ExpressionTool.copyLocationInfo(this, block)
      block
    } else
      this

  def copy(rebindings: RebindingMap): Expression = {
    val exp = new VennExpression(
      getLhsExpression.copy(rebindings),
      op,
      getRhsExpression.copy(rebindings)
    )
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override  def getOperandRole(arg: Int): OperandRole =
    OperandRole.SAME_FOCUS_ACTION

  override def equals(other: Any): Boolean =
    other match {
      case b: VennExpression =>
        if (op != b.op) {
          false
        } else if (getLhsExpression.isEqual(b.getLhsExpression) && getRhsExpression.isEqual(b.getRhsExpression)) {
          true
        } else if (op == Token.UNION || op == Token.INTERSECT) {
          val s0 = new util.HashSet[Expression](10)
          gatherComponents(op, s0)
          val s1 = new util.HashSet[Expression](10)
          b.gatherComponents(op, s1)
          s0 == s1
        } else
          false
      case _ =>
        false
    }

  override def computeHashCode(): Int =
    getLhsExpression.hashCode ^ getRhsExpression.hashCode

  override def toPattern(config: Configuration): Pattern = {
    if (isPredicatePattern(getLhsExpression) || isPredicatePattern(getRhsExpression)) {
      throw new XPathException(
        "Cannot use a predicate pattern as an operand of a union, intersect, or except op",
        "XTSE0340")
    }
    if (op == Token.UNION) {
      new UnionPattern(getLhsExpression.toPattern(config), getRhsExpression.toPattern(config))
    } else {
      if (op == Token.EXCEPT)
        new ExceptPattern(getLhsExpression.toPattern(config), getRhsExpression.toPattern(config))
      else
        new IntersectPattern(getLhsExpression.toPattern(config), getRhsExpression.toPattern(config))
    }
  }

  private def isPredicatePattern(exp: Expression): Boolean = {
    var expr = exp
    expr match {
      case itemChecker: ItemChecker =>
        expr = itemChecker.getBaseExpression
      case _ =>
    }
    expr.isInstanceOf[FilterExpression] &&
      expr
        .asInstanceOf[FilterExpression]
        .getSelectExpression
        .isInstanceOf[ContextItemExpression]
  }

   override def tag(): String = {
    if (op == Token.UNION)
      return "union"
    Token.tokens(op)
  }

  override def iterate(c: XPathContext): SequenceIterator = {
    val i1 = getLhsExpression.iterate(c)
    val i2 = getRhsExpression.iterate(c)
    op match {
      case Token.UNION =>
        new UnionEnumeration(i1, i2, GlobalOrderComparer.getInstance)
      case Token.INTERSECT =>
        new IntersectionEnumeration(i1, i2, GlobalOrderComparer.getInstance)
      case Token.EXCEPT =>
        new DifferenceEnumeration(i1, i2, GlobalOrderComparer.getInstance)
      case _ =>
        throw new UnsupportedOperationException("Unknown op in Venn Expression")
    }
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    if (op == Token.UNION)
      getLhsExpression.effectiveBooleanValue(context) || getRhsExpression.effectiveBooleanValue(context)
    else
      super.effectiveBooleanValue(context)

  override def getStreamerName: String = "VennExpression"
}
