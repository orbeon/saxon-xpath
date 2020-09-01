package net.sf.saxon.expr

import net.sf.saxon.expr.instruct.Block
import net.sf.saxon.expr.parser._
import net.sf.saxon.expr.sort.DocumentSorter
import net.sf.saxon.expr.sort.GlobalOrderComparer
import net.sf.saxon.functions.CurrentGroupCall
import net.sf.saxon.functions.SystemFunction
import net.sf.saxon.model._
import net.sf.saxon.om.AxisInfo
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.pattern._
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.Cardinality
import net.sf.saxon.value.SequenceType
import java.util.HashSet
import java.util.Set
import net.sf.saxon.utils.Configuration

class VennExpression(p1: Expression, val op: Int, p2: Expression) extends BinaryExpression(p1, op, p2) {

  override def simplify(): Expression = {
    if (!(getLhsExpression.isInstanceOf[DocumentSorter])) {
      this.setLhsExpression(new DocumentSorter(getLhsExpression))
    }
    if (!(getRhsExpression.isInstanceOf[DocumentSorter])) {
      this.setRhsExpression(new DocumentSorter(getRhsExpression))
    }
    super.simplify()
    this
  }

  override def getExpressionName: String = op match {
    case Token.UNION => "union"
    case Token.INTERSECT => "intersect"
    case Token.EXCEPT => "except"
    case _ => "unknown"

  }

  def getItemType: ItemType = {
    val t1: ItemType = getLhsExpression.getItemType
    if (op == Token.UNION) {
      val t2: ItemType = getRhsExpression.getItemType
      val th: TypeHierarchy = getConfiguration.getTypeHierarchy
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
    val c1: Int = getLhsExpression.getCardinality
    val c2: Int = getRhsExpression.getCardinality
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
          StaticProperty.EMPTY
        }
        if (Literal.isEmptySequence(getRhsExpression)) {
          StaticProperty.EMPTY
        }
        (c1 & c2) | StaticProperty.ALLOWS_ZERO | StaticProperty.ALLOWS_ONE
      case Token.EXCEPT =>
        if (Literal.isEmptySequence(getLhsExpression)) {
          StaticProperty.EMPTY
        }
        if (Literal.isEmptySequence(getRhsExpression)) {
          return c1
        }
        c1 | StaticProperty.ALLOWS_ZERO | StaticProperty.ALLOWS_ONE

    }
    StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  override def computeSpecialProperties(): Int = {
    val prop0: Int = getLhsExpression.getSpecialProperties
    val prop1: Int = getRhsExpression.getSpecialProperties
    var props: Int = StaticProperty.ORDERED_NODESET
    if (testContextDocumentNodeSet(prop0, prop1)) {
      props |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    if (testSubTree(prop0, prop1)) {
      props |= StaticProperty.SUBTREE_NODESET
    }
    if (createsNoNewNodes(prop0, prop1)) {
      props |= StaticProperty.NO_NODES_NEWLY_CREATED
    }
    props
  }

  private def testContextDocumentNodeSet(prop0: Int, prop1: Int): Boolean = {
    op match {
      case Token.UNION =>
        (prop0 & prop1 & StaticProperty.CONTEXT_DOCUMENT_NODESET) !=
          0
      case Token.INTERSECT =>
        ((prop0 | prop1) & StaticProperty.CONTEXT_DOCUMENT_NODESET) !=
          0
      case Token.EXCEPT =>
        (prop0 & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0

    }
    false
  }

  def gatherComponents(op: Int, set: Set[Expression]): Unit = {
    if (getLhsExpression.isInstanceOf[VennExpression] &&
      getLhsExpression.asInstanceOf[VennExpression].op ==
        op) {
      getLhsExpression
        .asInstanceOf[VennExpression]
        .gatherComponents(op, set)
    } else {
      set.add(getLhsExpression)
    }
    if (getRhsExpression.isInstanceOf[VennExpression] &&
      getRhsExpression.asInstanceOf[VennExpression].op ==
        op) {
      getRhsExpression
        .asInstanceOf[VennExpression]
        .gatherComponents(op, set)
    } else {
      set.add(getRhsExpression)
    }
  }

  private def testSubTree(prop0: Int, prop1: Int): Boolean = {
    op match {
      case Token.UNION => (prop0 & prop1 & StaticProperty.SUBTREE_NODESET) != 0
      case Token.INTERSECT =>
        ((prop0 | prop1) & StaticProperty.SUBTREE_NODESET) != 0
      case Token.EXCEPT => (prop0 & StaticProperty.SUBTREE_NODESET) != 0

    }
    false
  }

  private def createsNoNewNodes(prop0: Int, prop1: Int): Boolean =
    (prop0 & StaticProperty.NO_NODES_NEWLY_CREATED) != 0 &&
      (prop1 & StaticProperty.NO_NODES_NEWLY_CREATED) != 0

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val tc: TypeChecker = config.getTypeChecker(false)
    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)
    if (!(getLhsExpression.isInstanceOf[Pattern])) {
      val role0: RoleDiagnostic = new RoleDiagnostic(
        RoleDiagnostic.BINARY_EXPR,
        Token.tokens(op),
        0)
      this.setLhsExpression(tc.staticTypeCheck(getLhsExpression,
        SequenceType.NODE_SEQUENCE,
        role0,
        visitor))
    }
    if (!(getRhsExpression.isInstanceOf[Pattern])) {
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
      val th: TypeHierarchy = config.getTypeHierarchy
      val t0: ItemType = getLhsExpression.getItemType
      val t1: ItemType = getRhsExpression.getItemType
      if (th.relationship(t0, t1) == Affinity.DISJOINT) {
        if (op == Token.INTERSECT) {
          Literal.makeEmptySequence()
        } else {
          if (getLhsExpression.hasSpecialProperty(
            StaticProperty.ORDERED_NODESET)) {
            getLhsExpression
          } else {
            new DocumentSorter(getLhsExpression)
          }
        }
      }
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    val e: Expression = super.optimize(visitor, contextItemType)
    if (e != this) {
      return e
    }
    val config: Configuration = visitor.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
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
          new TailExpression(lhs, 2)
        }

    }
    if (lhs.isInstanceOf[AxisExpression] && rhs.isInstanceOf[AxisExpression]) {
      val a1: AxisExpression = lhs.asInstanceOf[AxisExpression]
      val a2: AxisExpression = rhs.asInstanceOf[AxisExpression]
      if (a1.getAxis == a2.getAxis) {
        if (a1.getNodeTest == a2.getNodeTest) {
          if (op == Token.EXCEPT) Literal.makeEmptySequence() else return a1
        } else {
          val ax: AxisExpression = new AxisExpression(
            a1.getAxis,
            new CombinedNodeTest(a1.getNodeTest, op, a2.getNodeTest))
          ExpressionTool.copyLocationInfo(this, ax)
          return ax
        }
      }
    }
    if (lhs.isInstanceOf[SlashExpression] && rhs
      .isInstanceOf[SlashExpression] &&
      op == Token.UNION) {
      val path1: SlashExpression = lhs.asInstanceOf[SlashExpression]
      val path2: SlashExpression = rhs.asInstanceOf[SlashExpression]
      if (path1.getFirstStep.isEqual(path2.getFirstStep)) {
        val venn: VennExpression = new VennExpression(path1.getRemainingSteps,
          op,
          path2.getRemainingSteps)
        ExpressionTool.copyLocationInfo(this, venn)
        val path: Expression =
          ExpressionTool.makePathExpression(path1.getFirstStep, venn)
        ExpressionTool.copyLocationInfo(this, path)
        path.optimize(visitor, contextItemType)
      }
    }
    if (lhs.isInstanceOf[FilterExpression] && rhs
      .isInstanceOf[FilterExpression]) {
      val exp0: FilterExpression = lhs.asInstanceOf[FilterExpression]
      val exp1: FilterExpression = rhs.asInstanceOf[FilterExpression]
      if (!exp0.isPositional(th) && !exp1.isPositional(th) &&
        exp0.getSelectExpression.isEqual(exp1.getSelectExpression)) {
        var filter: Expression = null
        op match {
          case Token.UNION =>
            filter = new OrExpression(exp0.getFilter, exp1.getFilter)
          case Token.INTERSECT =>
            filter = new AndExpression(exp0.getFilter, exp1.getFilter)
          case Token.EXCEPT =>
            var negate2: Expression = SystemFunction.makeCall(
              "not",
              getRetainedStaticContext,
              exp1.getFilter)
            filter = new AndExpression(exp0.getFilter, negate2)
          case _ => throw new AssertionError("Unknown op " + op)

        }
        ExpressionTool.copyLocationInfo(this, filter)
        val f: FilterExpression =
          new FilterExpression(exp0.getSelectExpression, filter)
        ExpressionTool.copyLocationInfo(this, f)
        f.simplify()
          .typeCheck(visitor, contextItemType)
          .optimize(visitor, contextItemType)
      }
    }
    if (!visitor.isOptimizeForStreaming && op == Token.UNION &&
      lhs.isInstanceOf[AxisExpression] &&
      rhs.isInstanceOf[AxisExpression]) {
      val a0: AxisExpression = lhs.asInstanceOf[AxisExpression]
      val a1: AxisExpression = rhs.asInstanceOf[AxisExpression]
      if (a0.getAxis == AxisInfo.ATTRIBUTE && a1.getAxis == AxisInfo.CHILD) {
        new Block(Array(lhs, rhs))
      } else if (a1.getAxis == AxisInfo.ATTRIBUTE && a0.getAxis == AxisInfo.CHILD) {
        new Block(Array(rhs, lhs))
      }
    }
    if (op == Token.INTERSECT && !Cardinality.allowsMany(
      lhs.getCardinality)) {
      new SingletonIntersectExpression(lhs,
        op,
        rhs.unordered(retainAllNodes = false, forStreaming = false))
    }
    if (op == Token.INTERSECT && !Cardinality.allowsMany(
      rhs.getCardinality)) {
      new SingletonIntersectExpression(rhs,
        op,
        lhs.unordered(retainAllNodes = false, forStreaming = false))
    }
    if (operandsAreDisjoint(th)) {
      if (op == Token.INTERSECT) {
        Literal.makeEmptySequence()
      } else if (op == Token.EXCEPT) {
        if ((lhs.getSpecialProperties & StaticProperty.ORDERED_NODESET) !=
          0) {
          return lhs
        } else {
          new DocumentSorter(lhs)
        }
      }
    }
    this
  }

  private def operandsAreDisjoint(th: TypeHierarchy): Boolean =
    th.relationship(getLhsExpression.getItemType, getRhsExpression.getItemType) ==
      Affinity.DISJOINT

  override def unordered(retainAllNodes: Boolean,
                         forStreaming: Boolean): Expression = {
    if (op == Token.UNION && !forStreaming &&
      operandsAreDisjoint(getConfiguration.getTypeHierarchy)) {
      val block: Block = new Block(Array(getLhsExpression, getRhsExpression))
      ExpressionTool.copyLocationInfo(this, block)
      return block
    }
    this
  }

  def copy(rebindings: RebindingMap): Expression = {
    val exp: VennExpression = new VennExpression(
      getLhsExpression.copy(rebindings),
      op,
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override  def getOperandRole(arg: Int): OperandRole =
    OperandRole.SAME_FOCUS_ACTION

  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[VennExpression]) {
      val b: VennExpression = other.asInstanceOf[VennExpression]
      if (op != b.op) {
        return false
      }
      if (getLhsExpression.isEqual(b.getLhsExpression) && getRhsExpression
        .isEqual(b.getRhsExpression)) {
        return true
      }
      if (op == Token.UNION || op == Token.INTERSECT) {
        val s0: Set[Expression] = new HashSet[Expression](10)
        gatherComponents(op, s0)
        val s1: Set[Expression] = new HashSet[Expression](10)
        other.asInstanceOf[VennExpression].gatherComponents(op, s1)
        return s0 == s1
      }
    }
    false
  }

  override def computeHashCode(): Int =
    getLhsExpression.hashCode ^ getRhsExpression.hashCode

  override def toPattern(config: Configuration): Pattern = {
    if (isPredicatePattern(getLhsExpression) || isPredicatePattern(
      getRhsExpression)) {
      throw new XPathException(
        "Cannot use a predicate pattern as an operand of a union, intersect, or except op",
        "XTSE0340")
    }
    if (op == Token.UNION) {
      new UnionPattern(getLhsExpression.toPattern(config),
        getRhsExpression.toPattern(config))
    } else {
      if (op == Token.EXCEPT) {
        new ExceptPattern(getLhsExpression.toPattern(config),
          getRhsExpression.toPattern(config))
      } else {
        new IntersectPattern(getLhsExpression.toPattern(config),
          getRhsExpression.toPattern(config))
      }
    }
  }

  private def isPredicatePattern(exp: Expression): Boolean = {
    var expr = exp
    if (expr.isInstanceOf[ItemChecker]) {
      expr = expr.asInstanceOf[ItemChecker].getBaseExpression
    }
    expr.isInstanceOf[FilterExpression] &&
      (expr
        .asInstanceOf[FilterExpression]
        .getSelectExpression
        .isInstanceOf[ContextItemExpression])
  }

   override def tag(): String = {
    if (op == Token.UNION) {
      return "union"
    }
    Token.tokens(op)
  }

  override def iterate(c: XPathContext): SequenceIterator = {
    val i1: SequenceIterator = getLhsExpression.iterate(c)
    val i2: SequenceIterator = getRhsExpression.iterate(c)
    op match {
      case Token.UNION =>
        new UnionEnumeration(i1, i2, GlobalOrderComparer.getInstance)
      case Token.INTERSECT =>
        new IntersectionEnumeration(i1, i2, GlobalOrderComparer.getInstance)
      case Token.EXCEPT =>
        new DifferenceEnumeration(i1, i2, GlobalOrderComparer.getInstance)

    }
    throw new UnsupportedOperationException(
      "Unknown op in Venn Expression")
  }

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    if (op == Token.UNION) {
      getLhsExpression.effectiveBooleanValue(context) || getRhsExpression
        .effectiveBooleanValue(context)
    } else {
      super.effectiveBooleanValue(context)
    }

  override def getStreamerName: String = "VennExpression"

}
