package net.sf.saxon.expr.sort

import net.sf.saxon.utils.Configuration
import net.sf.saxon.event.Outputter
import net.sf.saxon.event.ReceiverOption
import net.sf.saxon.expr._
import net.sf.saxon.expr.accum.Accumulator
import net.sf.saxon.expr.accum.AccumulatorManager
import net.sf.saxon.expr.instruct.Instruction
import net.sf.saxon.expr.instruct.TailCall
import net.sf.saxon.expr.parser._
import net.sf.saxon.functions._
import net.sf.saxon.lib.Feature
import net.sf.saxon.lib.ParseOptions
import net.sf.saxon.lib.Validation
import net.sf.saxon.model.ItemType
import net.sf.saxon.model.SchemaType
import net.sf.saxon.model.Type
import net.sf.saxon.model.TypeHierarchy
import net.sf.saxon.om._
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.s9api.Location
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.NoDynamicContextException
import net.sf.saxon.trans.XPathException
import net.sf.saxon.trans.XsltController
import net.sf.saxon.tree.iter.EmptyIterator
import net.sf.saxon.tree.iter.ManualIterator
import net.sf.saxon.tree.iter.SingletonIterator
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.ObjectValue
import net.sf.saxon.value.SequenceType
import java.util.ArrayList
import java.util.List
import java.util.Set

import MergeInstr.MergeSource._
import net.sf.saxon.expr.sort.MergeInstr.MergeSource

import scala.jdk.CollectionConverters._
//use scala-arm from http://jsuereth.com/scala-arm/

object MergeInstr {

  object MergeSource {

    private def copy(exp: Expression, rebindings: RebindingMap): Expression =
      if (exp == null) null else exp.copy(rebindings)

  }

  class MergeSource(mi: MergeInstr) {


    private var instruction: MergeInstr = mi

    var location: Location = _

    var forEachItemOp: Operand = null

    var forEachStreamOp: Operand = null

    var rowSelectOp: Operand = null

    var sourceName: String = null

    var mergeKeyDefinitions: SortKeyDefinitionList = null

    var baseURI: String = null

    var validation: Int = _

    var schemaType: SchemaType = _

    var streamable: Boolean = _

    var accumulators: Set[Accumulator] = _

    var invertedAction: AnyRef = _

    def this(instruction: MergeInstr,
             forEachItem: Expression,
             forEachStream: Expression,
             rSelect: Expression,
             name: String,
             sKeys: SortKeyDefinitionList,
             baseURI: String) = {
      this(instruction)
      this.instruction = instruction
      if (forEachItem != null) {
        initForEachItem(instruction, forEachItem)
      }
      if (forEachStream != null) {
        initForEachStream(instruction, forEachStream)
      }
      if (rSelect != null) {
        initRowSelect(instruction, rSelect)
      }
      this.sourceName = name
      this.mergeKeyDefinitions = sKeys
      this.baseURI = baseURI
    }

    def initForEachItem(instruction: MergeInstr,
                        forEachItem: Expression): Unit = {
      forEachItemOp =
        new Operand(instruction, forEachItem, OperandRole.INSPECT)
    }

    def initForEachStream(instruction: MergeInstr,
                          forEachStream: Expression): Unit = {
      forEachStreamOp =
        new Operand(instruction, forEachStream, OperandRole.INSPECT)
    }

    def initRowSelect(instruction: MergeInstr, rowSelect: Expression): Unit = {
      rowSelectOp = new Operand(instruction, rowSelect, ROW_SELECT)
    }

    def setStreamable(streamable: Boolean): Unit = {
      this.streamable = streamable
      if (streamable &&
        instruction.getConfiguration.getBooleanProperty(
          Feature.STREAMING_FALLBACK)) {
        this.streamable = false
        val select: Expression = rowSelectOp.getChildExpression
        rowSelectOp.setChildExpression(
          SystemFunction
            .makeCall("snapshot", select.getRetainedStaticContext, select))
      }
    }

    def copyMergeSource(newInstr: MergeInstr,
                        rebindings: RebindingMap): MergeSource = {
      val newKeyDef: Array[SortKeyDefinition] =
        Array.ofDim[SortKeyDefinition](mergeKeyDefinitions.size)
      for (i <- 0 until mergeKeyDefinitions.size) {
        newKeyDef(i) =
          mergeKeyDefinitions.getSortKeyDefinition(i).copy(rebindings)
      }
      val ms: MergeSource = new MergeSource(
        newInstr,
        copy(getForEachItem, rebindings),
        copy(getForEachSource, rebindings),
        copy(getRowSelect, rebindings),
        sourceName,
        new SortKeyDefinitionList(newKeyDef),
        baseURI
      )
      ms.validation = validation
      ms.schemaType = schemaType
      ms.streamable = streamable
      ms.location = location
      ms
    }

    def getForEachItem(): Expression =
      if (forEachItemOp == null) null else forEachItemOp.getChildExpression

    def setForEachItem(forEachItem: Expression): Unit = {
      if (forEachItem != null) {
        forEachItemOp.setChildExpression(forEachItem)
      }
    }

    def getForEachSource(): Expression =
      if (forEachStreamOp == null) null else forEachStreamOp.getChildExpression

    def setForEachStream(forEachStream: Expression): Unit = {
      if (forEachStream != null) {
        forEachStreamOp.setChildExpression(forEachStream)
      }
    }

    def getRowSelect(): Expression = rowSelectOp.getChildExpression

    def setRowSelect(rowSelect: Expression): Unit = {
      rowSelectOp.setChildExpression(rowSelect)
    }

    def getMergeKeyDefinitionSet(): SortKeyDefinitionList = mergeKeyDefinitions

    def setMergeKeyDefinitionSet(keys: SortKeyDefinitionList): Unit = {
      mergeKeyDefinitions = keys
    }

    def prepareForStreaming(): Unit = {}

  }

  def fixupGroupReferences(exp: Expression,
                           instr: MergeInstr,
                           isInLoop: Boolean): Unit = {
    if (exp == null) {} else if (exp.isCallOn(classOf[CurrentMergeGroup])) {
      val fn: CurrentMergeGroup = exp
        .asInstanceOf[SystemFunctionCall]
        .getTargetFunction
        .asInstanceOf[CurrentMergeGroup]
      fn.setControllingInstruction(instr, isInLoop)
    } else if (exp.isCallOn(classOf[CurrentMergeKey])) {
      val fn: CurrentMergeKey = exp
        .asInstanceOf[SystemFunctionCall]
        .getTargetFunction
        .asInstanceOf[CurrentMergeKey]
      fn.setControllingInstruction(instr)
    } else if (exp.isInstanceOf[MergeInstr]) {
      val instr2: MergeInstr = exp.asInstanceOf[MergeInstr]
      if (instr2 == instr) {
        fixupGroupReferences(instr2.getAction, instr, false)
      } else {
        for (m <- instr2.getMergeSources) {
          for (skd <- m.mergeKeyDefinitions.asScala) {
            fixupGroupReferences(skd.getOrder, instr, isInLoop)
            fixupGroupReferences(skd.getCaseOrder, instr, isInLoop)
            fixupGroupReferences(skd.getDataTypeExpression, instr, isInLoop)
            fixupGroupReferences(skd.getLanguage, instr, isInLoop)
            fixupGroupReferences(skd.getCollationNameExpression,
              instr,
              isInLoop)
            fixupGroupReferences(skd.getOrder, instr, isInLoop)
          }
          if (m.forEachItemOp != null) {
            fixupGroupReferences(m.getForEachItem, instr, isInLoop)
          }
          if (m.forEachStreamOp != null) {
            fixupGroupReferences(m.getForEachSource, instr, isInLoop)
          }
          if (m.rowSelectOp != null) {
            fixupGroupReferences(m.getRowSelect, instr, isInLoop)
          }
        }
      }
    } else {
      for (o <- exp.operands().asScala) {
        fixupGroupReferences(o.getChildExpression,
          instr,
          isInLoop || o.isEvaluatedRepeatedly)
      }
    }
  }

  private val ROW_SELECT: OperandRole = new OperandRole(
    OperandRole.USES_NEW_FOCUS | OperandRole.HIGHER_ORDER,
    OperandUsage.INSPECTION,
    SequenceType.ANY_SEQUENCE)

  class MergeKeyMappingFunction(private var baseContext: XPathContext,
                                private var ms: MergeSource)
    extends ContextMappingFunction {

    private var keyContext: XPathContext = baseContext.newMinorContext()

    private var manualIterator: ManualIterator = new ManualIterator()

    keyContext.setTemporaryOutputState(StandardNames.XSL_MERGE_KEY)

    manualIterator.setPosition(1)

    keyContext.setCurrentIterator(manualIterator)

    def map(context: XPathContext): SequenceIterator = {
      val currentItem: Item = context.getContextItem
      manualIterator.setContextItem(currentItem)
      val newItem: ItemWithMergeKeys = new ItemWithMergeKeys(
        currentItem,
        ms.mergeKeyDefinitions,
        ms.sourceName,
        keyContext)
      SingletonIterator.makeIterator(new ObjectValue(newItem))
    }

  }

}

class MergeInstr extends Instruction {

   var mergeSources: Array[MergeSource] = _

  private var actionOp: Operand = _

   var comparators: Array[AtomicComparer] = _

  def init(mSources: Array[MergeSource], action: Expression): MergeInstr = {
    actionOp = new Operand(this, action, OperandRole.FOCUS_CONTROLLED_ACTION)
    this.mergeSources = mSources
    for (mSource <- mSources) {
      adoptChildExpression(mSource.getForEachItem)
      adoptChildExpression(mSource.getForEachSource)
      adoptChildExpression(mSource.getRowSelect)
    }
    adoptChildExpression(action)
    this
  }

  def getMergeSources(): Array[MergeSource] = mergeSources

  def setAction(action: Expression): Unit = {
    actionOp.setChildExpression(action)
  }

  def getAction(): Expression = actionOp.getChildExpression

  override def getInstructionNameCode(): Int = StandardNames.XSL_MERGE

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    getAction.checkPermittedContents(parentType, false)
  }

  override def allowExtractingCommonSubexpressions(): Boolean = false

  override def getItemType(): ItemType = getAction.getItemType

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
    val tc: TypeChecker = config.getTypeChecker(false)
    var inputType: ItemType = null
    for (mergeSource <- mergeSources) {
      var rowContextItemType: ContextItemStaticInfo = contextInfo
      if (mergeSource.getForEachItem != null) {
        mergeSource.forEachItemOp.typeCheck(visitor, contextInfo)
        rowContextItemType = config.makeContextItemStaticInfo(
          mergeSource.getForEachItem.getItemType,
          false)
      } else if (mergeSource.getForEachSource != null) {
        mergeSource.forEachStreamOp.typeCheck(visitor, contextInfo)
        val role: RoleDiagnostic = new RoleDiagnostic(
          RoleDiagnostic.INSTRUCTION,
          "xsl:merge/for-each-source",
          0)
        mergeSource.setForEachStream(
          tc.staticTypeCheck(mergeSource.getForEachSource,
            SequenceType.STRING_SEQUENCE,
            role,
            visitor))
        rowContextItemType =
          config.makeContextItemStaticInfo(NodeKindTest.DOCUMENT, false)
      }
      mergeSource.rowSelectOp.typeCheck(visitor, rowContextItemType)
      val rowItemType: ItemType = mergeSource.getRowSelect.getItemType
      inputType =
        if (inputType == null) rowItemType
        else Type.getCommonSuperType(inputType, rowItemType, th)
      val cit: ContextItemStaticInfo =
        config.makeContextItemStaticInfo(inputType, false)
      if (mergeSource.mergeKeyDefinitions != null) {
        for (skd <- mergeSource.mergeKeyDefinitions.asScala) {
          var sortKey: Expression = skd.getSortKey
          sortKey = sortKey.typeCheck(visitor, cit)
          if (sortKey != null) {
            val role: RoleDiagnostic = new RoleDiagnostic(
              RoleDiagnostic.INSTRUCTION,
              "xsl:merge-key/select",
              0)
            role.setErrorCode("XTTE1020")
            sortKey = CardinalityChecker.makeCardinalityChecker(
              sortKey,
              StaticProperty.ALLOWS_ZERO_OR_ONE,
              role)
            skd.setSortKey(sortKey, true)
          }
          var exp: Expression = skd.getLanguage.typeCheck(
            visitor,
            config.makeContextItemStaticInfo(inputType, false))
          skd.setLanguage(exp)
          exp = skd.getOrder.typeCheck(visitor, cit)
          skd.setOrder(exp)
          exp = skd.getCollationNameExpression
          if (exp != null) {
            exp = exp.typeCheck(visitor, cit)
            skd.setCollationNameExpression(exp)
          }
          exp = skd.getCaseOrder.typeCheck(visitor, cit)
          skd.setCaseOrder(exp)
          exp = skd.getDataTypeExpression
          if (exp != null) {
            exp = exp.typeCheck(visitor, cit)
            skd.setDataTypeExpression(exp)
          }
        }
      }
    }
    actionOp.typeCheck(visitor,
      config.makeContextItemStaticInfo(inputType, false))
    if (Literal.isEmptySequence(getAction)) {
      getAction
    }
    if (mergeSources.length == 1 &&
      Literal.isEmptySequence(mergeSources(0).getRowSelect)) {
      mergeSources(0).getRowSelect
    }
    fixupGroupReferences()
    this
  }

  def fixupGroupReferences(): Unit = {
    MergeInstr.fixupGroupReferences(this, this, false)
  }

  override def mayCreateNewNodes(): Boolean = {
    val props: Int = getAction.getSpecialProperties
    (props & StaticProperty.NO_NODES_NEWLY_CREATED) == 0
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
    var inputType: ItemType = null
    for (mergeSource <- mergeSources) {
      var rowContextItemType: ContextItemStaticInfo = contextInfo
      if (mergeSource.getForEachItem != null) {
        mergeSource.forEachItemOp.optimize(visitor, contextInfo)
        rowContextItemType = config.makeContextItemStaticInfo(
          mergeSource.getForEachItem.getItemType,
          false)
      } else if (mergeSource.getForEachSource != null) {
        mergeSource.forEachStreamOp.optimize(visitor, contextInfo)
        rowContextItemType =
          config.makeContextItemStaticInfo(NodeKindTest.DOCUMENT, false)
      }
      mergeSource.rowSelectOp.optimize(visitor, rowContextItemType)
      val rowItemType: ItemType = mergeSource.getRowSelect.getItemType
      inputType =
        if (inputType == null) rowItemType
        else Type.getCommonSuperType(inputType, rowItemType, th)
    }
    val cit: ContextItemStaticInfo =
      config.makeContextItemStaticInfo(inputType, false)
    this.setAction(getAction.optimize(visitor, cit))
    if (Literal.isEmptySequence(getAction)) {
      getAction
    }
    if (mergeSources.length == 1 &&
      Literal.isEmptySequence(mergeSources(0).getRowSelect)) {
      mergeSources(0).getRowSelect
    }
    this
  }

  override def prepareForStreaming(): Unit = {
    for (mergeSource <- mergeSources) {
      mergeSource.prepareForStreaming()
    }
  }

  private def checkMergeAtt(sortKeyDefs: Array[SortKeyDefinition]): Unit = {
    for (i <- 1 until sortKeyDefs.length
         if !sortKeyDefs(0).isEqual(sortKeyDefs(i))) {
      throw new XPathException(
        "Corresponding xsl:merge-key attributes in different xsl:merge-source elements " +
          "do not have the same effective values",
        "XTDE2210")
    }
  }

  private def getLastPositionFinder(
                                     context: XPathContext): LastPositionFinder = new LastPositionFinder() {
    private var last: Int = -1

    override def getLength(): Int =
      if (last >= 0) {
        last
      } else {
        val comps: Array[AtomicComparer] = getComparators(context)
        val mgi: GroupIterator = context.getCurrentMergeGroupIterator
        val c1: XPathContextMajor = context.newContext()
        c1.setCurrentMergeGroupIterator(mgi)
        var inputIterator: SequenceIterator =
          getMergedInputIterator(context, comps, c1)
        inputIterator = new MergeGroupingIterator(
          inputIterator,
          getComparer(mergeSources(0).mergeKeyDefinitions, comps),
          null)
        last = Count.steppingCount(inputIterator)
        last
      }
  }

  override def iterate(context: XPathContext): SequenceIterator =
    try {
      val comps: Array[AtomicComparer] = getComparators(context)
      val mgi: GroupIterator = context.getCurrentMergeGroupIterator
      val c1: XPathContextMajor = context.newContext()
      c1.setCurrentMergeGroupIterator(mgi)
      var inputIterator: SequenceIterator =
        getMergedInputIterator(context, comps, c1)
      inputIterator = new MergeGroupingIterator(
        inputIterator,
        getComparer(mergeSources(0).mergeKeyDefinitions, comps),
        getLastPositionFinder(context))
      c1.setCurrentMergeGroupIterator(
        inputIterator.asInstanceOf[GroupIterator])
      val c3: XPathContext = c1.newMinorContext()
      c3.trackFocus(inputIterator)
      new ContextMappingIterator((cxt) => getAction.iterate(cxt), c3)
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        throw e
      }

    }

  private def getMergedInputIterator(
                                      context: XPathContext,
                                      comps: Array[AtomicComparer],
                                      c1: XPathContextMajor): SequenceIterator = {
    var inputIterator: SequenceIterator = EmptyIterator.getInstance
    for (ms <- mergeSources) {
      var anchorsIter: SequenceIterator = null
      if (ms.streamable && ms.getForEachSource != null) {} else if (ms.getForEachSource != null) {
        val options: ParseOptions = new ParseOptions(
          context.getConfiguration.getParseOptions)
        options.setSchemaValidationMode(ms.validation)
        options.setTopLevelType(ms.schemaType)
        options.setApplicableAccumulators(ms.accumulators)
        val uriIter: SequenceIterator = ms.getForEachSource.iterate(c1)
        val controller: XsltController =
          context.getController.asInstanceOf[XsltController]
        val accumulatorManager: AccumulatorManager =
          controller.getAccumulatorManager
        anchorsIter = new ItemMappingIterator(
          uriIter,
          (baseItem) => {
            var uri: String = baseItem.getStringValue
            var node: NodeInfo = DocumentFn.makeDoc(
              uri,
              getRetainedStaticContext.getStaticBaseUriString,
              getPackageData,
              options,
              c1,
              getLocation,
              true)
            accumulatorManager.setApplicableAccumulators(node.getTreeInfo,
              ms.accumulators)
            node
          }
        )
        val c2: XPathContext = c1.newMinorContext()
        val anchorsIterFocus: FocusIterator = c2.trackFocus(anchorsIter)
        while (anchorsIterFocus.next() != null) {
          val c4: XPathContext = c2.newMinorContext()
          val rowIntr: FocusIterator =
            c4.trackFocus(ms.getRowSelect.iterate(c2))
        /*  val addMergeKeys: MergeKeyMappingFunction = // No Class MergeKeyMappingFunction found
            new MergeKeyMappingFunction(c4, ms)*/
          val contextMapKeysItr: ContextMappingIterator =
            new ContextMappingIterator(null, c4)
          inputIterator =
            makeMergeIterator(inputIterator, comps, ms, contextMapKeysItr)
        }
      } else if (ms.getForEachItem != null) {
        anchorsIter = ms.getForEachItem.iterate(c1)
        val c2: XPathContext = c1.newMinorContext()
        val anchorsIterFocus: FocusIterator = c2.trackFocus(anchorsIter)
        while (anchorsIterFocus.next() != null) inputIterator =
          getInputIterator(comps, inputIterator, ms, c2)
      } else {
        inputIterator = getInputIterator(comps, inputIterator, ms, c1)
      }
    }
    inputIterator
  }

  private def getInputIterator(comps: Array[AtomicComparer],
                               inputIterator: SequenceIterator,
                               ms: MergeSource,
                               c2: XPathContext): SequenceIterator = {
    var inputItr = inputIterator
    val c4: XPathContext = c2.newMinorContext()
    c4.setTemporaryOutputState(StandardNames.XSL_MERGE_KEY)
    val rowIntr: FocusIterator = c4.trackFocus(ms.getRowSelect.iterate(c2))
    /*val addMergeKeys: MergeKeyMappingFunction =  // no class found
      new MergeKeyMappingFunction(c4, ms)*/
    val contextMapKeysItr: ContextMappingIterator =
      new ContextMappingIterator(null, c4)
    inputItr =
      makeMergeIterator(inputItr, comps, ms, contextMapKeysItr)
    inputItr
  }

  private def getComparators(context: XPathContext): Array[AtomicComparer] = {
    var comps: Array[AtomicComparer] = comparators
    if (comparators == null) {
      val tempSKeys: Array[SortKeyDefinition] =
        Array.ofDim[SortKeyDefinition](mergeSources.length)
      for (i <- 0 until mergeSources(0).mergeKeyDefinitions.size) {
        for (j <- 0 until mergeSources.length) {
          tempSKeys(j) = mergeSources(j).mergeKeyDefinitions
            .getSortKeyDefinition(i)
            .fix(context)
        }
        checkMergeAtt(tempSKeys)
      }
      comps =
        Array.ofDim[AtomicComparer](mergeSources(0).mergeKeyDefinitions.size)
      for (s <- 0 until mergeSources(0).mergeKeyDefinitions.size) {
        var comp: AtomicComparer = mergeSources(0).mergeKeyDefinitions
          .getSortKeyDefinition(s)
          .getFinalComparator
        if (comp == null) {
          comp = mergeSources(0).mergeKeyDefinitions
            .getSortKeyDefinition(s)
            .makeComparator(context)
        }
        comps(s) = comp
      }
    }
    comps
  }

  private def makeMergeIterator(
                                 result: SequenceIterator,
                                 comps: Array[AtomicComparer],
                                 ms: MergeSource,
                                 contextMapKeysItr: ContextMappingIterator): SequenceIterator = {
    var seqItrRes = result
    seqItrRes =
      if (seqItrRes == null || seqItrRes.isInstanceOf[EmptyIterator])
        contextMapKeysItr
      else
        new MergeIterator(seqItrRes,
          contextMapKeysItr,
          getComparer(ms.mergeKeyDefinitions, comps))
    seqItrRes
  }

  override def operands(): java.lang.Iterable[Operand] = {
    val list: List[Operand] = new ArrayList[Operand](6)
    list.add(actionOp)
    if (mergeSources != null) {
      for (ms <- mergeSources) {
        if (ms.forEachItemOp != null) {
          list.add(ms.forEachItemOp)
        }
        if (ms.forEachStreamOp != null) {
          list.add(ms.forEachStreamOp)
        }
        if (ms.rowSelectOp != null) {
          list.add(ms.rowSelectOp)
        }
        list.add(
          new Operand(this, ms.mergeKeyDefinitions, OperandRole.SINGLE_ATOMIC))
      }
    }
    list
  }

  def getGroupingKey(): Expression =
    mergeSources(0).mergeKeyDefinitions.getSortKeyDefinition(0).getSortKey

  def getComparer(sKeys: SortKeyDefinitionList,
                  comps: Array[AtomicComparer]): ItemOrderComparer =
    (a, b) => {
      val aObj: ObjectValue[ItemWithMergeKeys] = a.asInstanceOf[ObjectValue[ItemWithMergeKeys]]
      val bObj: ObjectValue[ItemWithMergeKeys] = b.asInstanceOf[ObjectValue[ItemWithMergeKeys]]
      val aItem: ItemWithMergeKeys =
        aObj.getObject
      val bItem: ItemWithMergeKeys =
        bObj.getObject
      for (i <- 0 until sKeys.size) {
        var `val`: Int = 0
        `val` = comps(i).compareAtomicValues(aItem.sortKeyValues.get(i),
          bItem.sortKeyValues.get(i))
        if (`val` != 0) {
          `val`
        }
      }
      0
    }

  def copy(rebindings: RebindingMap): Expression = {
    val newMerge: MergeInstr = new MergeInstr()
    val c2: Array[MergeSource] = Array.ofDim[MergeSource](mergeSources.length)
    val a2: Expression = getAction.copy(rebindings)
    for (c <- 0 until mergeSources.length) {
      c2(c) = mergeSources(c).copyMergeSource(newMerge, rebindings)
    }
    newMerge.init(c2, a2)
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("merge", this)
    for (mergeSource <- mergeSources) {
      out.startSubsidiaryElement("mergeSrc")
      if (mergeSource.sourceName != null &&
        !mergeSource.sourceName.startsWith("saxon-merge-source-")) {
        out.emitAttribute("name", mergeSource.sourceName)
      }
      if (mergeSource.validation != Validation.SKIP && mergeSource.validation != Validation.BY_TYPE) {
        out.emitAttribute("validation",
          Validation.toString(mergeSource.validation))
      }
      if (mergeSource.validation == Validation.BY_TYPE) {
        val `type`: SchemaType = mergeSource.schemaType
        if (`type` != null) {
          out.emitAttribute("type", `type`.getStructuredQName)
        }
      }
      if (mergeSource.accumulators != null && !mergeSource.accumulators.isEmpty) {
        val fsb: FastStringBuffer = new FastStringBuffer(256)
        for (acc <- mergeSource.accumulators.asScala) {
          if (!fsb.isEmpty) {
            fsb.append(" ")
          }
          fsb.append(acc.getAccumulatorName.getEQName)
        }
        out.emitAttribute("accum", fsb.toString)
      }
      if (mergeSource.streamable) {
        out.emitAttribute("flags", "s")
      }
      if (mergeSource.getForEachItem != null) {
        out.setChildRole("forEachItem")
        mergeSource.getForEachItem.export(out)
      }
      if (mergeSource.getForEachSource != null) {
        out.setChildRole("forEachStream")
        mergeSource.getForEachSource.export(out)
      }
      out.setChildRole("selectRows")
      mergeSource.getRowSelect.export(out)
      mergeSource.getMergeKeyDefinitionSet.export(out)
      out.endSubsidiaryElement()
    }
    out.setChildRole("action")
    getAction.export(out)
    out.endElement()
  }

  override def processLeavingTail(output: Outputter,
                                  context: XPathContext): TailCall = {
    try {
      var iter = iterate(context)
      iter.forEachOrFail((it) =>
        output.append(it, getLocation, ReceiverOption.ALL_NAMESPACES))
    } catch {
      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        e.maybeSetContext(context)
        throw e
      }

    }
    null
  }

  override def getStreamerName(): String = "MergeInstr"

}
