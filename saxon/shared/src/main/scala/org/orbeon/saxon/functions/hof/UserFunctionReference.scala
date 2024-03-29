package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.UserFunction
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionTool, ExpressionVisitor, RebindingMap}
import org.orbeon.saxon.functions.AbstractFunction
import org.orbeon.saxon.functions.hof.UserFunctionReference._
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.query.AnnotationList
import org.orbeon.saxon.style.StylesheetPackage
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.{SymbolicName, XPathException}
import org.orbeon.saxon.utils.Controller

import scala.beans.BeanProperty

object UserFunctionReference {

  class BoundUserFunction(private var agent: ExportAgent,
                          private var function: Function,
                          private var component: Component,
                          @BeanProperty var controller: Controller)
    extends AbstractFunction
      with ContextOriginator {

    def getTargetFunction: Function = function

    override def makeNewContext(oldContext: XPathContext,
                                originator: ContextOriginator): XPathContext = {
      if (controller.getConfiguration != oldContext.getConfiguration) {
        throw new IllegalStateException(
          "A function created under one Configuration cannot be called under a different Configuration")
      }
      var c2: XPathContextMajor = null
      c2 = controller.newXPathContext
      c2.setTemporaryOutputState(StandardNames.XSL_FUNCTION)
      c2.setCurrentOutputUri(null)
      c2.setCurrentComponent(component)
      c2.setURIResolver(oldContext.getURIResolver)
      c2.setOrigin(originator)
      function.makeNewContext(c2, originator)
    }

    def call(context: XPathContext, args: Array[Sequence]): Sequence = {
      val c2: XPathContext = function.makeNewContext(context, this)
      c2 match {
        case major: XPathContextMajor if component != null =>
          major.setCurrentComponent(component)
        case _ =>
      }
      function.call(c2, args)
    }

    def getFunctionItemType: FunctionItemType = function.getFunctionItemType
    override def getAnnotations: AnnotationList = function.getAnnotations
    def getFunctionName: StructuredQName = function.getFunctionName
    def getArity: Int = function.getArity
    def getDescription: String = function.getDescription

    override def export(out: ExpressionPresenter): Unit =
      agent.export(out)
  }

}

class UserFunctionReference
  extends Expression
    with ComponentInvocation
    with UserFunctionResolvable
    with Callable {

  private var functionName: SymbolicName = _
  private var nominalTarget: UserFunction = _

  var bindingSlot: Int = -1

  private var optimizeCounter: Int = 0
  private var typeCheckCounter: Int = 0

  def this(target: UserFunction) {
    this()
    this.nominalTarget = target
    this.functionName = target.getSymbolicName
  }

  def this(name: SymbolicName) = {
    this()
    this.functionName = name
  }

  def setFunction(function: UserFunction): Unit = {
    if (function.getSymbolicName != functionName)
      throw new IllegalArgumentException("Function name does not match")
    this.nominalTarget = function
  }

  override def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    if (nominalTarget.getFunctionName.hasURI(NamespaceConstant.ANONYMOUS) && {
      typeCheckCounter += 1
      typeCheckCounter - 1
    } < 10) {
      nominalTarget.typeCheck(visitor)
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    if (nominalTarget.getFunctionName.hasURI(NamespaceConstant.ANONYMOUS) && {
      optimizeCounter += 1
      optimizeCounter - 1
    } < 10) {
      var o: Expression = null
      o = nominalTarget.getBody.optimize(visitor, ContextItemStaticInfo.ABSENT)
      nominalTarget.setBody(o)
      val slotManager = visitor.getConfiguration.makeSlotManager
      for (i <- 0 until getArity)
        slotManager.allocateSlotNumber(nominalTarget.getParameterDefinitions()(i).getVariableQName)
      ExpressionTool.allocateSlots(o, getArity, slotManager)
      nominalTarget.setStackFrameMap(slotManager)
    }
    this
  }

  def getFixedTarget: Component = nominalTarget.getDeclaringComponent
  def getSymbolicName: SymbolicName = functionName
  def getFunctionItemType(th: TypeHierarchy): FunctionItemType = nominalTarget.getFunctionItemType
  def getFunctionName: StructuredQName = nominalTarget.getFunctionName
  def getArity: Int = nominalTarget.getArity
  def computeCardinality(): Int = StaticProperty.EXACTLY_ONE
  def getImplementationMethod: Int = Expression.EVALUATE_METHOD
  def getItemType: ItemType = nominalTarget.getFunctionItemType
  override def getStaticUType(contextItemType: UType): UType = UType.FUNCTION

  def copy(rebindings: RebindingMap): Expression = {
    val ref: UserFunctionReference = new UserFunctionReference(nominalTarget)
    ref.optimizeCounter = optimizeCounter
    ref.typeCheckCounter = typeCheckCounter
    ref
  }

  override def evaluateItem(context: XPathContext): Function =
    if (bindingSlot == -1) {
      new BoundUserFunction(this,
        nominalTarget,
        nominalTarget.getDeclaringComponent,
        context.getController)
    } else {
      val targetComponent: Component = context.getTargetComponent(bindingSlot)
      new BoundUserFunction(
        this,
        targetComponent.getActor.asInstanceOf[UserFunction],
        targetComponent,
        context.getController)
    }

  def call(context: XPathContext, arguments: Array[Sequence]): Function =
    evaluateItem(context)

  override def getExpressionName: String = "UserFunctionReference"
  override def toString: String = getFunctionName.getEQName + "#" + getArity
  override def toShortString: String = getFunctionName.getDisplayName + "#" + getArity

  def setBindingSlot(slot: Int): Unit = bindingSlot = slot
  def getBindingSlot: Int = bindingSlot

  def export(out: ExpressionPresenter): Unit = {
    val options: ExpressionPresenter.ExportOptions = out.getOptions.asInstanceOf[ExpressionPresenter.ExportOptions]
    if ("JS" == options.target && options.targetVersion == 1) {
      throw new XPathException(
        "Higher-order functions are not available in Saxon-JS v1.*",
        "XTSE3540",
        getLocation)
    }
    if (nominalTarget.getDeclaringComponent == null) {
      out.startElement("inlineFn")
      nominalTarget.export(out)
      out.endElement()
    } else {
      val rootPackage: StylesheetPackage = options.rootPackage
      val containingPackage: StylesheetPackage =
        nominalTarget.getDeclaringComponent.getContainingPackage
      if (rootPackage != null &&
        !(rootPackage == containingPackage || rootPackage.contains(
          containingPackage))) {
        throw new XPathException(
          "Cannot export a package containing a reference to a user-defined function (" +
            toShortString +
            ") that is not present in the package being exported")
      }
      out.startElement("ufRef")
      out.emitAttribute("name", nominalTarget.getFunctionName)
      out.emitAttribute("arity", nominalTarget.getArity.toString)
      out.emitAttribute("bSlot", "" + getBindingSlot)
      out.endElement()
    }
  }
}
