package net.sf.saxon.expr.sort

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Cardinality

import net.sf.saxon.value.SequenceType

import scala.jdk.CollectionConverters._

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.util.control.Breaks._


object SortExpression {

  private val SAME_FOCUS_SORT_KEY: OperandRole = new OperandRole(
    OperandRole.HIGHER_ORDER,
    OperandUsage.ABSORPTION,
    SequenceType.OPTIONAL_ATOMIC)

  private val NEW_FOCUS_SORT_KEY: OperandRole = new OperandRole(
    OperandRole.USES_NEW_FOCUS | OperandRole.HIGHER_ORDER,
    OperandUsage.ABSORPTION,
    SequenceType.OPTIONAL_ATOMIC)

}

class SortExpression(select: Expression, sortKeys: SortKeyDefinitionList)
  extends Expression
    with SortKeyEvaluator {

  private var selectOp: Operand =
    new Operand(this, select, OperandRole.FOCUS_CONTROLLING_SELECT)

  private var sortOp: Operand =
    new Operand(this, sortKeys, OperandRole.ATOMIC_SEQUENCE)

  @BeanProperty
  var comparators: Array[AtomicComparer] = null

  adoptChildExpression(select)

  adoptChildExpression(sortKeys)

  override def getExpressionName: String = "sort"

  def getBaseOperand: Operand = selectOp

  def getBaseExpression: Expression = getSelect

  override def operands: java.lang.Iterable[Operand] =
    operandList(selectOp, sortOp)

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val target: PathMap.PathMapNodeSet =
      getSelect.addToPathMap(pathMap, pathMapNodeSet)
    for (sortKeyDefinition <- getSortKeyDefinitionList.asScala) {
      if (sortKeyDefinition.isSetContextForSortKey) {
        sortKeyDefinition.getSortKey.addToPathMap(pathMap, target)
      } else {
        sortKeyDefinition.getSortKey.addToPathMap(pathMap, pathMapNodeSet)
      }
      var e: Expression = sortKeyDefinition.getOrder
      if (e != null) {
        e.addToPathMap(pathMap, pathMapNodeSet)
      }
      e = sortKeyDefinition.getCaseOrder
      if (e != null) {
        e.addToPathMap(pathMap, pathMapNodeSet)
      }
      e = sortKeyDefinition.getDataTypeExpression
      if (e != null) {
        e.addToPathMap(pathMap, pathMapNodeSet)
      }
      e = sortKeyDefinition.getLanguage
      if (e != null) {
        e.addToPathMap(pathMap, pathMapNodeSet)
      }
      e = sortKeyDefinition.getCollationNameExpression
      if (e != null) {
        e.addToPathMap(pathMap, pathMapNodeSet)
      }
    }
    target
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    selectOp.typeCheck(visitor, contextInfo)
    val select2: Expression = getSelect
    if (select2 != getSelect) {
      adoptChildExpression(select2)
      this.setSelect(select2)
    }
    if (!Cardinality.allowsMany(select2.getCardinality)) {
      return select2
    }
    val sortedItemType: ItemType = getSelect.getItemType
    var allKeysFixed: Boolean = true
    breakable {
      for (sortKeyDefinition <- getSortKeyDefinitionList.asScala
           if !sortKeyDefinition.isFixed) {
        allKeysFixed = false
        break()
      }
    }
    if (allKeysFixed) {
      comparators = Array.ofDim[AtomicComparer](getSortKeyDefinitionList.size)
    }
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(false)
    for (i <- 0 until getSortKeyDefinitionList.size) {
      val sortKeyDef: SortKeyDefinition = getSortKeyDefinition(i)
      var sortKey: Expression = sortKeyDef.getSortKey
      if (sortKeyDef.isSetContextForSortKey) {
        val cit: ContextItemStaticInfo = visitor.getConfiguration
          .makeContextItemStaticInfo(sortedItemType, maybeUndefined = false)
        sortKey = sortKey.typeCheck(visitor, cit)
      } else {
        sortKey = sortKey.typeCheck(visitor, contextInfo)
      }
      if (sortKeyDef.isBackwardsCompatible) {
        sortKey = FirstItemExpression.makeFirstItemExpression(sortKey)
      } else {
        val role: RoleDiagnostic =
          new RoleDiagnostic(RoleDiagnostic.INSTRUCTION, "xsl:sort/select", 0)
        role.setErrorCode("XTTE1020")
        sortKey = tc.staticTypeCheck(sortKey,
          SequenceType.OPTIONAL_ATOMIC,
          role,
          visitor)
      }
      sortKeyDef.setSortKey(sortKey, sortKeyDef.isSetContextForSortKey)
      sortKeyDef.typeCheck(visitor, contextInfo)
      if (sortKeyDef.isFixed) {
        val comp: AtomicComparer = sortKeyDef.makeComparator(
          visitor.getStaticContext.makeEarlyEvaluationContext())
        sortKeyDef.setFinalComparator(comp)
        if (allKeysFixed) {
          comparators(i) = comp
        }
      }
      if (sortKeyDef.isSetContextForSortKey && !ExpressionTool.dependsOnFocus(
        sortKey)) {
        visitor.getStaticContext.issueWarning(
          "Sort key will have no effect because its value does not depend on the context item",
          sortKey.getLocation)
      }
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    selectOp.optimize(visitor, contextItemType)
    var cit: ContextItemStaticInfo = null
    if (getSortKeyDefinition(0).isSetContextForSortKey) {
      val sortedItemType: ItemType = getSelect.getItemType
      cit = visitor.getConfiguration
        .makeContextItemStaticInfo(sortedItemType, maybeUndefined = false)
    } else {
      cit = contextItemType
    }
    for (sortKeyDefinition <- getSortKeyDefinitionList.asScala) {
      var sortKey: Expression = sortKeyDefinition.getSortKey
      sortKey = sortKey.optimize(visitor, cit)
      sortKeyDefinition.setSortKey(sortKey, setContext = true)
    }
    if (Cardinality.allowsMany(getSelect.getCardinality)) {
      this
    } else {
      getSelect
    }
  }

  def copy(rebindings: RebindingMap): Expression = {
    val len = getSortKeyDefinitionList.size
    val sk2: Array[SortKeyDefinition] = Array.ofDim[SortKeyDefinition](len)
    for (i <- 0 until len) {
      sk2(i) = getSortKeyDefinition(i).copy(rebindings)
    }
    val se2: SortExpression = new SortExpression(
      getSelect.copy(rebindings),
      new SortKeyDefinitionList(sk2))
    ExpressionTool.copyLocationInfo(this, se2)
    se2.comparators = comparators
    se2
  }

  def isSortKey(child: Expression): Boolean = {
    for (sortKeyDefinition <- getSortKeyDefinitionList.asScala) {
      val exp: Expression = sortKeyDefinition.getSortKey
      if (exp == child) {
        true
      }
    }
    false
  }

  def computeCardinality(): Int = getSelect.getCardinality

  def getItemType: ItemType = getSelect.getItemType

  override def computeSpecialProperties(): Int = {
    var props: Int = 0
    if (getSelect.hasSpecialProperty(StaticProperty.CONTEXT_DOCUMENT_NODESET)) {
      props |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    if (getSelect.hasSpecialProperty(StaticProperty.SINGLE_DOCUMENT_NODESET)) {
      props |= StaticProperty.SINGLE_DOCUMENT_NODESET
    }
    if (getSelect.hasSpecialProperty(StaticProperty.NO_NODES_NEWLY_CREATED)) {
      props |= StaticProperty.NO_NODES_NEWLY_CREATED
    }
    props
  }

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  override def iterate(context: XPathContext): SequenceIterator = {
    var iter: SequenceIterator = getSelect.iterate(context)
    if (iter.isInstanceOf[EmptyIterator]) {
      return iter
    }
    var comps: Array[AtomicComparer] = comparators
    if (comparators == null) {
      val len = getSortKeyDefinitionList.size
      comps = Array.ofDim[AtomicComparer](len)
      for (s <- 0 until len) {
        var comp: AtomicComparer = getSortKeyDefinition(s).getFinalComparator
        if (comp == null) {
          comp = getSortKeyDefinition(s).makeComparator(context)
        }
        comps(s) = comp
      }
    }
    iter = new SortedIterator(context,
      iter,
      this,
      comps,
      getSortKeyDefinition(0).isSetContextForSortKey)
    iter
      .asInstanceOf[SortedIterator]
      .setHostLanguage(getPackageData.getHostLanguage)
    iter
  }

  def evaluateSortKey(n: Int, c: XPathContext): AtomicValue =
    getSortKeyDefinition(n).getSortKey
      .evaluateItem(c)
      .asInstanceOf[AtomicValue]

  override def toShortString: String =
    "sort(" + getBaseExpression.toShortString + ")"

  override def getStreamerName: String = "SortExpression"

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("sort", this)
    out.setChildRole("select")
    getSelect.export(out)
    getSortKeyDefinitionList.export(out)
    out.endElement()
  }

  def getSelect: Expression = selectOp.getChildExpression

  def setSelect(select: Expression): Unit = {
    selectOp.setChildExpression(select)
  }

  def getSortKeyDefinitionList: SortKeyDefinitionList =
    sortOp.getChildExpression.asInstanceOf[SortKeyDefinitionList]

  def getSortKeyDefinition(i: Int): SortKeyDefinition =
    getSortKeyDefinitionList.getSortKeyDefinition(i)

  def setSortKeyDefinitionList(skd: SortKeyDefinitionList): Unit = {
    sortOp.setChildExpression(skd)
  }

}
