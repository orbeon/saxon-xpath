package net.sf.saxon.expr.instruct

import net.sf.saxon.utils.Controller

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.expr.sort._

import net.sf.saxon.functions.CurrentGroupCall

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.lib.TraceListener

import net.sf.saxon.model.ErrorType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.FocusIterator

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.StandardNames

import net.sf.saxon.pattern.Pattern

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

import java.net.URI

import java.net.URISyntaxException

import ForEachGroup._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._

object ForEachGroup {

  val GROUP_BY: Int = 0

  val GROUP_ADJACENT: Int = 1

  val GROUP_STARTING: Int = 2

  val GROUP_ENDING: Int = 3

  private def fixupGroupReferences(exp: Expression,
                                   feg: ForEachGroup,
                                   selectedItemType: ItemType,
                                   isInLoop: Boolean): Unit = {
    if (exp == null) {} else if (exp.isInstanceOf[CurrentGroupCall]) {
      exp
        .asInstanceOf[CurrentGroupCall]
        .setControllingInstruction(feg, selectedItemType, isInLoop)
    } else if (exp.isInstanceOf[ForEachGroup]) {
      val feg2: ForEachGroup = exp.asInstanceOf[ForEachGroup]
      if (feg2 == feg) {
        fixupGroupReferences(feg2.getActionExpression,
          feg,
          selectedItemType,
          isInLoop = false)
      } else {
        fixupGroupReferences(feg2.getSelectExpression,
          feg,
          selectedItemType,
          isInLoop)
        fixupGroupReferences(feg2.getGroupingKey,
          feg,
          selectedItemType,
          isInLoop)
        if (feg2.getSortKeyDefinitions != null) {
          for (skd <- feg2.getSortKeyDefinitions.asScala) {
            fixupGroupReferences(skd.getOrder, feg, selectedItemType, isInLoop)
            fixupGroupReferences(skd.getCaseOrder,
              feg,
              selectedItemType,
              isInLoop)
            fixupGroupReferences(skd.getDataTypeExpression,
              feg,
              selectedItemType,
              isInLoop)
            fixupGroupReferences(skd.getLanguage,
              feg,
              selectedItemType,
              isInLoop)
            fixupGroupReferences(skd.getCollationNameExpression,
              feg,
              selectedItemType,
              isInLoop)
            fixupGroupReferences(skd.getOrder, feg, selectedItemType, isInLoop)
          }
        }
      }
    } else {
      for (o <- exp.operands().asScala) {
        fixupGroupReferences(o.getChildExpression,
          feg,
          selectedItemType,
          isInLoop || o.isHigherOrder)
      }
    }
  }

  private def getAlgorithmName(algorithm: Byte): String = algorithm match {
    case GROUP_BY => "by"
    case GROUP_ADJACENT => "adjacent"
    case GROUP_STARTING => "starting"
    case GROUP_ENDING => "ending"
    case _ => "** unknown algorithm **"

  }

}

class ForEachGroup(select: Expression,
                   action: Expression,
                   @BeanProperty var algorithm: Byte,
                   key: Expression,
                   private var collator: StringCollator,
                   collationNameExpression: Expression,
                   sortKeys: SortKeyDefinitionList)
  extends Instruction
    with SortKeyEvaluator
    with ContextMappingFunction
    with ContextSwitchingExpression {

  var cnExpression = collationNameExpression

  @BeanProperty
  var keyItemType: Int = _

  @transient private var sortComparators: Array[AtomicComparer] = null

  @BooleanBeanProperty
  var composite: Boolean = false

  var isInFork: Boolean = false

  val keyRole: OperandRole =
    if ((algorithm == GROUP_ENDING || algorithm == GROUP_STARTING))
      OperandRole.PATTERN
    else OperandRole.NEW_FOCUS_ATOMIC

  private var selectOp: Operand =
    new Operand(this, select, OperandRole.FOCUS_CONTROLLING_SELECT)

  private var actionOp: Operand =
    new Operand(this, action, OperandRole.FOCUS_CONTROLLED_ACTION)

  private var keyOp: Operand = new Operand(this, key, keyRole)

  private var collationOp: Operand = _

  private var sortKeysOp: Operand = _

  if (cnExpression != null) {
    collationOp =
      new Operand(this, cnExpression, OperandRole.SINGLE_ATOMIC)
  }

  if (sortKeys != null) {
    sortKeysOp = new Operand(this, sortKeys, OperandRole.SINGLE_ATOMIC)
  }

  for (o <- operands().asScala) {
    adoptChildExpression(o.getChildExpression)
  }

  override def getInstructionNameCode(): Int = StandardNames.XSL_FOR_EACH_GROUP

  override def operands(): java.lang.Iterable[Operand] =
    operandSparseList(selectOp, actionOp, keyOp, collationOp, sortKeysOp)

  def getSelectExpression(): Expression = selectOp.getChildExpression

  def getActionExpression(): Expression = actionOp.getChildExpression

  def getGroupingKey(): Expression = keyOp.getChildExpression

  def getSortKeyDefinitions(): SortKeyDefinitionList =
    if (sortKeysOp == null) null
    else sortKeysOp.getChildExpression.asInstanceOf[SortKeyDefinitionList]

  def getSortKeyComparators(): Array[AtomicComparer] = sortComparators

  def getCollation(): StringCollator = collator

  def getBaseURI(): URI =
    try getRetainedStaticContext.getStaticBaseUri
    catch {
      case err: XPathException => null

    }

  def setIsInFork(inFork: Boolean): Unit = {
    isInFork = inFork
  }

  override def allowExtractingCommonSubexpressions(): Boolean = false

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    selectOp.typeCheck(visitor, contextInfo)
    if (collationOp != null) {
      collationOp.typeCheck(visitor, contextInfo)
    }
    val selectedItemType: ItemType = getSelectExpression.getItemType
    if (selectedItemType == ErrorType.getInstance) {
      Literal.makeEmptySequence()
    }
    for (o <- operands().asScala) {
      fixupGroupReferences(this, this, selectedItemType, isInLoop = false)
    }
    val cit: ContextItemStaticInfo = visitor.getConfiguration
      .makeContextItemStaticInfo(selectedItemType, maybeUndefined = false)
    cit.setContextSettingExpression(getSelectExpression)
    actionOp.typeCheck(visitor, cit)
    keyOp.typeCheck(visitor, cit)
    if (Literal.isEmptySequence(getSelectExpression)) {
      getSelectExpression
    }
    if (Literal.isEmptySequence(getActionExpression)) {
      getActionExpression
    }
    if (getSortKeyDefinitions != null) {
      var allFixed: Boolean = true
      for (sk <- getSortKeyDefinitions.asScala) {
        var sortKey: Expression = sk.getSortKey
        sortKey = sortKey.typeCheck(visitor, cit)
        if (sk.isBackwardsCompatible) {
          sortKey = FirstItemExpression.makeFirstItemExpression(sortKey)
        } else {
          val role: RoleDiagnostic = new RoleDiagnostic(
            RoleDiagnostic.INSTRUCTION,
            "xsl:sort/select",
            0)
          role.setErrorCode("XTTE1020")
          sortKey = CardinalityChecker.makeCardinalityChecker(
            sortKey,
            StaticProperty.ALLOWS_ZERO_OR_ONE,
            role)
        }
        sk.setSortKey(sortKey, setContext = true)
        sk.typeCheck(visitor, contextInfo)
        if (sk.isFixed) {
          val comp: AtomicComparer = sk.makeComparator(
            visitor.getStaticContext.makeEarlyEvaluationContext())
          sk.setFinalComparator(comp)
        } else {
          allFixed = false
        }
      }
      if (allFixed) {
        sortComparators =
          Array.ofDim[AtomicComparer](getSortKeyDefinitions.size)
        for (i <- 0 until getSortKeyDefinitions.size) {
          sortComparators(i) =
            getSortKeyDefinitions.getSortKeyDefinition(i).getFinalComparator
        }
      }
    }
    keyItemType = getGroupingKey.getItemType.getPrimitiveType
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    selectOp.optimize(visitor, contextItemType)
    val selectedItemType: ItemType = getSelectExpression.getItemType
    val sit: ContextItemStaticInfo = visitor.getConfiguration
      .makeContextItemStaticInfo(selectedItemType, maybeUndefined = false)
    sit.setContextSettingExpression(getSelectExpression)
    actionOp.optimize(visitor, sit)
    keyOp.optimize(visitor, sit)
    if (Literal.isEmptySequence(getSelectExpression)) {
      getSelectExpression
    }
    if (Literal.isEmptySequence(getActionExpression)) {
      getActionExpression
    }
    if (getSortKeyDefinitions != null) {
      for (skd <- getSortKeyDefinitions.asScala) {
        var sortKey: Expression = skd.getSortKey
        sortKey = sortKey.optimize(visitor, sit)
        skd.setSortKey(sortKey, setContext = true)
      }
    }
    if (collationOp != null) {
      collationOp.optimize(visitor, contextItemType)
    }
    if (collator == null &&
      (getCollationNameExpression.isInstanceOf[StringLiteral])) {
      val collation: String =
        getCollationNameExpression.asInstanceOf[StringLiteral].getStringValue
      var collationURI: URI = null
      try {
        collationURI = new URI(collation)
        if (!collationURI.isAbsolute) {
          collationURI = getStaticBaseURI.resolve(collationURI)
          val collationNameString: String = collationURI.toString
          cnExpression = new StringLiteral(collationNameString)
          collator = visitor.getConfiguration.getCollation(collationNameString)
          if (collator == null) {
            val err: XPathException = new XPathException(
              "Unknown collation " + Err.wrap(collationURI.toString, Err.URI))
            err.setErrorCode("XTDE1110")
            err.setLocation(getLocation)
            throw err
          }
        }
      } catch {
        case err: URISyntaxException => {
          val e: XPathException = new XPathException(
            "Collation name '" + getCollationNameExpression + "' is not a valid URI")
          e.setErrorCode("XTDE1110")
          e.setLocation(getLocation)
          throw e
        }

      }
    }
    this
  }

  def copy(rebindings: RebindingMap): Expression = {
    var newKeyDef: Array[SortKeyDefinition] = null
    if (getSortKeyDefinitions != null) {
      newKeyDef = Array.ofDim[SortKeyDefinition](getSortKeyDefinitions.size)
      for (i <- 0 until getSortKeyDefinitions.size) {
        newKeyDef(i) =
          getSortKeyDefinitions.getSortKeyDefinition(i).copy(rebindings)
      }
    }
    val feg: ForEachGroup = new ForEachGroup(
      getSelectExpression.copy(rebindings),
      getActionExpression.copy(rebindings),
      algorithm,
      getGroupingKey.copy(rebindings),
      collator,
      getCollationNameExpression.copy(rebindings),
      if (newKeyDef == null) null else new SortKeyDefinitionList(newKeyDef)
    )
    ExpressionTool.copyLocationInfo(this, feg)
    feg.setComposite(isComposite)
    fixupGroupReferences(feg, feg, getSelectExpression.getItemType, isInLoop = false)
    feg
  }

  override def getItemType(): ItemType = getActionExpression.getItemType

  override def computeDependencies(): Int = {
    var dependencies: Int = 0
    dependencies |= getSelectExpression.getDependencies
    dependencies |= getGroupingKey.getDependencies & ~StaticProperty.DEPENDS_ON_FOCUS
    dependencies |= getActionExpression.getDependencies &
      ~(StaticProperty.DEPENDS_ON_FOCUS | StaticProperty.DEPENDS_ON_CURRENT_GROUP)
    if (getSortKeyDefinitions != null) {
      for (skd <- getSortKeyDefinitions.asScala) {
        dependencies |= skd.getSortKey.getDependencies & ~StaticProperty.DEPENDS_ON_FOCUS
        var e: Expression = skd.getCaseOrder
        if (e != null && !(e.isInstanceOf[Literal])) {
          dependencies |= e.getDependencies
        }
        e = skd.getDataTypeExpression
        if (e != null && !(e.isInstanceOf[Literal])) {
          dependencies |= e.getDependencies
        }
        e = skd.getLanguage
        if (e != null && !(e.isInstanceOf[Literal])) {
          dependencies |= e.getDependencies
        }
      }
    }
    if (getCollationNameExpression != null) {
      dependencies |= getCollationNameExpression.getDependencies
    }
    dependencies
  }

  override def computeSpecialProperties(): Int = {
    var p: Int = super.computeSpecialProperties()
    p |= getActionExpression.getSpecialProperties & StaticProperty.ALL_NODES_UNTYPED
    p
  }

  override def mayCreateNewNodes(): Boolean = {
    val props: Int = getActionExpression.getSpecialProperties
    (props & StaticProperty.NO_NODES_NEWLY_CREATED) == 0
  }

  override def getStreamerName(): String = "ForEachGroup"

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val target: PathMap.PathMapNodeSet =
      getSelectExpression.addToPathMap(pathMap, pathMapNodeSet)
    if (getCollationNameExpression != null) {
      getCollationNameExpression.addToPathMap(pathMap, pathMapNodeSet)
    }
    if (getSortKeyDefinitions != null) {
      for (skd <- getSortKeyDefinitions.asScala) {
        skd.getSortKey.addToPathMap(pathMap, target)
        var e: Expression = skd.getOrder
        if (e != null) {
          e.addToPathMap(pathMap, pathMapNodeSet)
        }
        e = skd.getCaseOrder
        if (e != null) {
          e.addToPathMap(pathMap, pathMapNodeSet)
        }
        e = skd.getDataTypeExpression
        if (e != null) {
          e.addToPathMap(pathMap, pathMapNodeSet)
        }
        e = skd.getLanguage
        if (e != null) {
          e.addToPathMap(pathMap, pathMapNodeSet)
        }
        e = skd.getCollationNameExpression
        if (e != null) {
          e.addToPathMap(pathMap, pathMapNodeSet)
        }
      }
    }
    getActionExpression.addToPathMap(pathMap, target)
  }

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    getActionExpression.checkPermittedContents(parentType, whole = false)
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val controller: Controller = context.getController
    assert(controller != null)
    val pipe: PipelineConfiguration = output.getPipelineConfiguration
    val groupIterator: GroupIterator = getGroupIterator(context)
    val c2: XPathContextMajor = context.newContext()
    c2.setOrigin(this)
    val focusIterator: FocusIterator = c2.trackFocus(groupIterator)
    c2.setCurrentGroupIterator(groupIterator)
    c2.setCurrentTemplateRule(null)
    pipe.setXPathContext(c2)
    if (controller.isTracing) {
      val listener: TraceListener = controller.getTraceListener
      assert(listener != null)
      var item: Item = null
      while (({
        item = focusIterator.next()
        item
      }) != null) {
        listener.startCurrentItem(item)
        getActionExpression.process(output, c2)
        listener.endCurrentItem(item)
      }
    } else {
      while (focusIterator.next() != null) getActionExpression.process(output,
        c2)
    }
    pipe.setXPathContext(context)
    null
  }

  def getCollationNameExpression(): Expression =
    if (collationOp == null) null else collationOp.getChildExpression

  private def getCollator(context: XPathContext): StringCollator =
    if (getCollationNameExpression != null) {
      val collationValue: StringValue = getCollationNameExpression
        .evaluateItem(context)
        .asInstanceOf[StringValue]
      assert(collationValue != null)
      val cname: String = collationValue.getStringValue
      try context.getConfiguration.getCollation(cname,
        getStaticBaseURIString,
        "FOCH0002")
      catch {
        case e: XPathException => {
          e.setLocation(getLocation)
          throw e
        }

      }
    } else {
      CodepointCollator.getInstance
    }

  private def getGroupIterator(context: XPathContext): GroupIterator =
    getGroupIterator(getSelectExpression, context)

  def getGroupIterator(select: Expression,
                       context: XPathContext): GroupIterator = {
    var groupIterator: GroupIterator = null
    algorithm match {
      case GROUP_BY => {
        var coll: StringCollator = collator
        if (coll == null) {
          coll = getCollator(context)
        }
        val c2: XPathContext = context.newMinorContext()
        val population: FocusIterator = c2.trackFocus(select.iterate(context))
        groupIterator =
          new GroupByIterator(population, getGroupingKey, c2, coll, composite)
      }
      case GROUP_ADJACENT => {
        var coll: StringCollator = collator
        if (coll == null) {
          coll = getCollator(context)
        }
        groupIterator = new GroupAdjacentIterator(select,
          getGroupingKey,
          context,
          coll,
          composite)
      }
      case GROUP_STARTING =>
        groupIterator = new GroupStartingIterator(
          select,
          getGroupingKey.asInstanceOf[Pattern],
          context)
      case GROUP_ENDING =>
        groupIterator = new GroupEndingIterator(
          select,
          getGroupingKey.asInstanceOf[Pattern],
          context)
      case _ => throw new AssertionError("Unknown grouping algorithm")

    }
    if (getSortKeyDefinitions != null) {
      var comps: Array[AtomicComparer] = sortComparators
      val xpc: XPathContext = context.newMinorContext()
      if (comps == null) {
        comps = Array.ofDim[AtomicComparer](getSortKeyDefinitions.size)
        for (s <- 0 until getSortKeyDefinitions.size) {
          comps(s) =
            getSortKeyDefinitions.getSortKeyDefinition(s).makeComparator(xpc)
        }
      }
      groupIterator = new SortedGroupIterator(xpc, groupIterator, this, comps)
    }
    groupIterator
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val master: GroupIterator = getGroupIterator(context)
    val c2: XPathContextMajor = context.newContext()
    c2.setOrigin(this)
    c2.trackFocus(master)
    c2.setCurrentGroupIterator(master)
    c2.setCurrentTemplateRule(null)
    new ContextMappingIterator(this, c2)
  }

  def map(context: XPathContext): SequenceIterator =
    getActionExpression.iterate(context)

  def evaluateSortKey(n: Int, c: XPathContext): AtomicValue =
    getSortKeyDefinitions
      .getSortKeyDefinition(n)
      .getSortKey
      .evaluateItem(c)
      .asInstanceOf[AtomicValue]

  def getSortKeyDefinitionList(): SortKeyDefinitionList = {
    if (sortKeysOp == null) return null
    sortKeysOp.getChildExpression.asInstanceOf[SortKeyDefinitionList]
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("forEachGroup", this)
    out.emitAttribute("algorithm", getAlgorithmName(algorithm))
    var flags: String = ""
    if (composite) {
      flags = "c"
    }
    if (isInFork) {
      flags += "k"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    out.setChildRole("select")
    getSelectExpression.export(out)
    if (algorithm == GROUP_BY || algorithm == GROUP_ADJACENT) {
      out.setChildRole("key")
      getGroupingKey.export(out)
    } else {
      out.setChildRole("match")
      getGroupingKey.export(out)
    }
    if (getSortKeyDefinitions != null) {
      out.setChildRole("sort")
      getSortKeyDefinitionList.export(out)
    }
    if (getCollationNameExpression != null) {
      out.setChildRole("collation")
      getCollationNameExpression.export(out)
    }
    out.setChildRole("content")
    getActionExpression.export(out)
    out.endElement()
  }

  def setSelect(select: Expression): Unit = {
    selectOp.setChildExpression(select)
  }

  def setAction(action: Expression): Unit = {
    actionOp.setChildExpression(action)
  }

  def setKey(key: Expression): Unit = {
    keyOp.setChildExpression(key)
  }

  def setCollationNameExpression(collationNameExpression: Expression): Unit = {
    if (collationOp == null) {
      collationOp =
        new Operand(this, collationNameExpression, OperandRole.SINGLE_ATOMIC)
    } else {
      collationOp.setChildExpression(collationNameExpression)
    }
  }

  def setSortKeyDefinitions(sortKeyDefinitions: SortKeyDefinitionList): Unit = {
    if (sortKeysOp == null) {
      sortKeysOp =
        new Operand(this, sortKeyDefinitions, OperandRole.SINGLE_ATOMIC)
    } else {
      sortKeysOp.setChildExpression(sortKeyDefinitions)
    }
  }

}
