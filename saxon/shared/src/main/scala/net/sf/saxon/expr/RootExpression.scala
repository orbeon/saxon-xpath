package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.parser._

import net.sf.saxon.model._

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.pattern.AnyNodeTest

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.pattern.NodeTestPattern

import net.sf.saxon.pattern.Pattern

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.SingletonIterator

class RootExpression extends Expression {

  private var contextMaybeUndefined: Boolean = true

  private var doneWarnings: Boolean = false

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
    if (contextInfo == null || contextInfo.getItemType == null ||
      contextInfo.getItemType == ErrorType) {
      val err = new XPathException(
        noContextMessage() + ": the context item is absent")
      err.setErrorCode("XPDY0002")
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    } else if (!doneWarnings && contextInfo.isParentless &&
      th.relationship(contextInfo.getItemType, NodeKindTest.DOCUMENT) ==
        Affinity.DISJOINT) {
      visitor.issueWarning(
        noContextMessage() +
          ": the context item is parentless and is not a document node",
        getLocation)
      doneWarnings = true
    }
    contextMaybeUndefined = contextInfo.isPossiblyAbsent
    if (th.isSubType(contextInfo.getItemType, NodeKindTest.DOCUMENT)) {
      val cie: ContextItemExpression = new ContextItemExpression()
      ExpressionTool.copyLocationInfo(this, cie)
      cie.setStaticInfo(contextInfo)
      return cie
    }
    val relation: Affinity.Affinity =
      th.relationship(contextInfo.getItemType, AnyNodeTest.getInstance)
    if (relation == Affinity.DISJOINT) {
      val err = new XPathException(
        noContextMessage() + ": the context item is not a node")
      err.setErrorCode("XPTY0020")
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression =
    typeCheck(visitor, contextItemType)

  override def computeSpecialProperties(): Int =
    StaticProperty.ORDERED_NODESET | StaticProperty.CONTEXT_DOCUMENT_NODESET |
      StaticProperty.SINGLE_DOCUMENT_NODESET |
      StaticProperty.NO_NODES_NEWLY_CREATED

  def isContextPossiblyUndefined: Boolean = contextMaybeUndefined

  def noContextMessage(): String = "Leading '/' selects nothing"

  override def equals(other: Any): Boolean = other.isInstanceOf[RootExpression]

  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  def getItemType: ItemType = NodeKindTest.DOCUMENT

  override def getStaticUType(contextItemType: UType): UType = UType.DOCUMENT

  override def getImplementationMethod(): Int = Expression.EVALUATE_METHOD

  override def computeHashCode(): Int = "RootExpression".hashCode

  def getNode(context: XPathContext): NodeInfo = {
    val current: Item = context.getContextItem
    if (current == null) {
      dynamicError("Finding root of tree: the context item is absent",
        "XPDY0002",
        context)
    }
    if (current.isInstanceOf[NodeInfo]) {
      val doc: NodeInfo = current.asInstanceOf[NodeInfo].getRoot
      if (doc.getNodeKind != Type.DOCUMENT) {
        dynamicError(
          "The root of the tree containing the context item is not a document node",
          "XPDY0050",
          context)
      }
      return doc
    }
    typeError("Finding root of tree: the context item is not a node",
      "XPTY0020",
      context)
    null
  }

  override def getIntrinsicDependencies(): Int =
    StaticProperty.DEPENDS_ON_CONTEXT_DOCUMENT

  def copy(rebindings: RebindingMap): Expression = {
    val exp: RootExpression = new RootExpression()
    ExpressionTool.copyLocationInfo(this, exp)
    exp
  }

  override def toPattern(config: Configuration): Pattern =
    new NodeTestPattern(NodeKindTest.DOCUMENT)

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    var pmnSet = pathMapNodeSet
    if (pmnSet == null) {
      val cie: ContextItemExpression = new ContextItemExpression()
      ExpressionTool.copyLocationInfo(this, cie)
      pmnSet = new PathMap.PathMapNodeSet(pathMap.makeNewRoot(cie))
    }
    pmnSet.createArc(AxisInfo.ANCESTOR_OR_SELF, NodeKindTest.DOCUMENT)
  }

  override def toString: String = "(/)"

  override def getExpressionName(): String = "root"

  def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("root", this)
    destination.endElement()
  }

  override def iterate(context: XPathContext): SequenceIterator =
    SingletonIterator.makeIterator(getNode(context))

  override def evaluateItem(context: XPathContext): NodeInfo = getNode(context)

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    getNode(context) != null

  override def getStreamerName(): String = "RootExpression"

}
