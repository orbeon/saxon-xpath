package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser._

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.StandardNames

import net.sf.saxon.s9api.Location

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans._

import net.sf.saxon.tree.iter.EmptyIterator

import java.util.ArrayList

import java.util.List

import ApplyTemplates._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object ApplyTemplates {

   class ApplyTemplatesPackage(
                                         private var selectedItems: Sequence,
                                         private var targetMode: Component.M,
                                         private var params: ParameterSet,
                                         private var tunnelParams: ParameterSet,
                                         private var output: Outputter,
                                         private var evaluationContext: XPathContextMajor,
                                         private var locationId: Location)
    extends TailCall {

    def processLeavingTail(): TailCall = {
      evaluationContext.trackFocus(selectedItems.iterate())
      evaluationContext.setCurrentMode(targetMode)
      evaluationContext.setCurrentComponent(targetMode)
      targetMode.getActor.applyTemplates(params,
        tunnelParams,
        output,
        evaluationContext,
        locationId)
    }

  }

}

class ApplyTemplates  ()
  extends Instruction
    with ITemplateCall
    with ComponentInvocation {

  private var selectOp: Operand = _

  @BeanProperty
  var actualParams: Array[WithParam] = _

  @BeanProperty
  var tunnelParams: Array[WithParam] = _

   var useCurrentMode: Boolean = false

   var useTailRecur: Boolean = false

   var mode: Mode = _

   var implicitSelect: Boolean = _

   var inStreamableConstruct: Boolean = false

 //  var ruleManager: RuleManager = _  //class does not exist

  var bindingSlot: Int = -1

  def setBindingSlot(slot: Int): Unit = this.bindingSlot = slot

  def getBindingSlot(): Int = bindingSlot

  def this(select: Expression,
           useCurrentMode: Boolean,
           useTailRecursion: Boolean,
           implicitSelect: Boolean,
           inStreamableConstruct: Boolean,
           mode: Mode/*,
           ruleManager: RuleManager*/) = { // RuleManager not exist
    this()
    selectOp = new Operand(this, select, OperandRole.SINGLE_ATOMIC)
    init(select, useCurrentMode, useTailRecursion, mode)
    this.implicitSelect = implicitSelect
    this.inStreamableConstruct = inStreamableConstruct
    //this.ruleManager = ruleManager
  }

   def init(select: Expression,
                     useCurrentMode: Boolean,
                     useTailRecursion: Boolean,
                     mode: Mode): Unit = {
    this.setSelect(select)
    this.useCurrentMode = useCurrentMode
    this.useTailRecur = useTailRecursion
    this.mode = mode
    adoptChildExpression(select)
  }

  def setMode(target: SimpleMode): Unit = {
    this.mode = target
  }

  override def operands(): java.lang.Iterable[Operand] = {
    val operanda: List[Operand] = new ArrayList[Operand]()
    operanda.add(selectOp)
    WithParam.gatherOperands(this, getActualParams, operanda)
    WithParam.gatherOperands(this, getTunnelParams, operanda)
    operanda
  }

  override def getInstructionNameCode(): Int = StandardNames.XSL_APPLY_TEMPLATES

  override def getImplementationMethod(): Int =
    super.getImplementationMethod | Expression.WATCH_METHOD

  override def simplify(): Expression = {
    WithParam.simplify(getActualParams)
    WithParam.simplify(getTunnelParams)
    this.setSelect(getSelect.simplify())
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                contextInfo: ContextItemStaticInfo): Expression = {
    WithParam.typeCheck(actualParams, visitor, contextInfo)
    WithParam.typeCheck(tunnelParams, visitor, contextInfo)
    try selectOp.typeCheck(visitor, contextInfo)
    catch {
      case e: XPathException => {
        if (implicitSelect) {
          val code: String = e.getErrorCodeLocalPart
          if ("XPTY0020" == code || "XPTY0019" == code) {
            val err: XPathException = new XPathException(
              "Cannot apply-templates to child nodes when the context item is an atomic value")
            err.setErrorCode("XTTE0510")
            err.setIsTypeError(true)
            throw err
          } else if ("XPDY0002" == code) {
            val err: XPathException = new XPathException(
              "Cannot apply-templates to child nodes when the context item is absent")
            err.setErrorCode("XTTE0510")
            err.setIsTypeError(true)
            throw err
          }
        }
        throw e
      }

    }
    adoptChildExpression(getSelect)
    if (Literal.isEmptySequence(getSelect)) {
      getSelect
    }
    this
  }

override  def optimize(visitor: ExpressionVisitor,
               contextInfo: ContextItemStaticInfo): Expression = {
    WithParam.optimize(visitor, actualParams, contextInfo)
    WithParam.optimize(visitor, tunnelParams, contextInfo)
    selectOp.typeCheck(visitor, contextInfo)
    selectOp.optimize(visitor, contextInfo)
    if (Literal.isEmptySequence(getSelect)) {
      getSelect
    }
    this
  }

  override def getIntrinsicDependencies(): Int =
    super.getIntrinsicDependencies |
      (if (useCurrentMode) StaticProperty.DEPENDS_ON_CURRENT_ITEM else 0)

  //def getRuleManager(): RuleManager = ruleManager //  RuleManager not exist

  def copy(rebindings: RebindingMap): Expression = {
    val a2: ApplyTemplates = new ApplyTemplates(getSelect.copy(rebindings),
      useCurrentMode,
      useTailRecur,
      implicitSelect,
      inStreamableConstruct,
      mode)
    // ruleManager
    a2.setActualParams(WithParam.copy(a2, getActualParams, rebindings))
    a2.setTunnelParams(WithParam.copy(a2, getTunnelParams, rebindings))
    ExpressionTool.copyLocationInfo(this, a2)
    //a2.ruleManager = ruleManager
    a2
  }

  override def mayCreateNewNodes(): Boolean = true

  override def process(output: Outputter, context: XPathContext): Unit = {
    apply(output, context, false)
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall =
    apply(output, context, useTailRecur)

   def apply(output: Outputter,
                      context: XPathContext,
                      returnTailCall: Boolean): TailCall = {
    val targetMode: Component.M = getTargetMode(context)
    val thisMode: Mode = targetMode.getActor
    val params: ParameterSet = Instruction.assembleParams(context, getActualParams)
    val tunnels: ParameterSet =Instruction assembleTunnelParams(context, getTunnelParams)
    if (returnTailCall) {
      val c2: XPathContextMajor = context.newContext()
      c2.setOrigin(this)
      new ApplyTemplatesPackage(
        ExpressionTool.lazyEvaluate(getSelect, context, false),
        targetMode,
        params,
        tunnels,
        output,
        c2,
        getLocation)
    }
    val iter: SequenceIterator = getSelect.iterate(context)
    if (iter.isInstanceOf[EmptyIterator]) {
      return null
    }
    val c2: XPathContextMajor = context.newContext()
    c2.trackFocus(iter)
    c2.setCurrentMode(targetMode)
    c2.setOrigin(this)
    c2.setCurrentComponent(targetMode)
    if (inStreamableConstruct) {
      c2.setCurrentGroupIterator(null)
    }
    val pipe: PipelineConfiguration = output.getPipelineConfiguration
    pipe.setXPathContext(c2)
    try {
      var tc: TailCall =
        thisMode.applyTemplates(params, tunnels, output, c2, getLocation)
      while (tc != null) tc = tc.processLeavingTail()
    } catch {
      case e: StackOverflowError => {
        val err: XPathException = new XPathException.StackOverflow(
          "Too many nested apply-templates calls. The stylesheet may be looping.",
          SaxonErrorCode.SXLM0001,
          getLocation)
        err.setXPathContext(context)
        throw err
      }

    }
    pipe.setXPathContext(context)
    null
  }

  def getTargetMode(context: XPathContext): Component.M = {
    var targetMode: Component.M = null
    if (useCurrentMode) {
      targetMode = context.getCurrentMode
    } else {
      if (bindingSlot >= 0) {
        targetMode =
          context.getTargetComponent(bindingSlot).asInstanceOf[Component.M]
        if (targetMode.getVisibility == Visibility.ABSTRACT) {
          throw new AssertionError("Modes cannot be abstract")
        }
      } else {
        targetMode = mode.getDeclaringComponent.asInstanceOf[Component.M]
      }
    }
    targetMode
  }

  def getSelectExpression(): Expression = getSelect

  def isImplicitSelect(): Boolean = implicitSelect

  def useTailRecursion(): Boolean = useTailRecur

  def usesCurrentMode(): Boolean = useCurrentMode

  def getMode(): Mode = mode

  def getFixedTarget(): Component = mode.getDeclaringComponent

  def getSymbolicName(): SymbolicName =
    if (mode == null) null else mode.getSymbolicName

  override def addToPathMap(
                    pathMap: PathMap,
                    pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val result: PathMap.PathMapNodeSet =
      super.addToPathMap(pathMap, pathMapNodeSet)
    result.setReturnable(false)
    new PathMap.PathMapNodeSet(pathMap.makeNewRoot(this))
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("applyT", this)
    if (mode != null && !mode.isUnnamedMode) {
      out.emitAttribute("mode", mode.getModeName)
    }
    var flags: String = ""
    if (useCurrentMode) {
      flags = "c"
    }
    if (useTailRecur) {
      flags += "t"
    }
    if (implicitSelect) {
      flags += "i"
    }
    if (!flags.isEmpty) {
      out.emitAttribute("flags", flags)
    }
    out.emitAttribute("bSlot", "" + getBindingSlot)
    out.setChildRole("select")
    getSelect.export(out)
    if (getActualParams.length != 0) {
      WithParam.exportParameters(getActualParams, out, false)
    }
    if (getTunnelParams.length != 0) {
      WithParam.exportParameters(getTunnelParams, out, true)
    }
    out.endElement()
  }

  def getSelect(): Expression = selectOp.getChildExpression

  def setSelect(select: Expression): Unit = {
    selectOp.setChildExpression(select)
  }

  override def getStreamerName(): String = "ApplyTemplates"

}
