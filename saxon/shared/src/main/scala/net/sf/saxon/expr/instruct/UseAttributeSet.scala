package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.lib.NamespaceConstant

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.StandardNames

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.SymbolicName

import net.sf.saxon.trans.XPathException

import java.util.ArrayList

import java.util.Collections

import java.util.List

object UseAttributeSet {

  def makeUseAttributeSets(targets: Array[StructuredQName]
                           /*instruction: StyleElement*/): Expression = { // StyleElement does not exist
    val list: List[UseAttributeSet] =
      makeUseAttributeSetInstructions(targets)
    makeCompositeExpression(list)
  }

  def makeUseAttributeSetInstructions(
                                       targets: Array[StructuredQName] /*,
                                       instruction: StyleElement*/): List[UseAttributeSet] = { // StyleElement does not exist
    val list: List[UseAttributeSet] =
      new ArrayList[UseAttributeSet](targets.length)
    for (name <- targets) {
      val use: UseAttributeSet = null //makeUseAttributeSet(name)
      if (use != null) {
        list.add(use)
      }
    }
    list
  }

  def makeCompositeExpression(targets: List[UseAttributeSet]): Expression =
    if (targets.size == 0) {
      Literal.makeEmptySequence()
    } else if (targets.size == 1) {
      targets.get(0)
    } else {
      new Block(targets.toArray(Array.ofDim[Expression](0)))
    }

 /* private def makeUseAttributeSet(name: StructuredQName
                                   /*instruction: StyleElement*/): UseAttributeSet = { // StyleElement does not exist
    var target: AttributeSet = null
    if (name.hasURI(NamespaceConstant.XSLT) && name.getLocalPart.==(
      "original")) {
      target = instruction.getXslOriginal(StandardNames.XSL_ATTRIBUTE_SET)
        .asInstanceOf[AttributeSet]
    } else {
      val invokee: Component = instruction.getContainingPackage.getComponent(
        new SymbolicName(StandardNames.XSL_ATTRIBUTE_SET, name))
      instruction.getPrincipalStylesheetModule.getAttributeSetDeclarations(
        name)
      if (invokee == null) {
        instruction.compileError("Unknown attribute set " + name.getEQName,
          "XTSE0710")
        null
      }
      target = invokee.getActor.asInstanceOf[AttributeSet]
    }
    val invocation: UseAttributeSet =
      new UseAttributeSet(name, target.isDeclaredStreamable)
    invocation.setTarget(target)
    invocation.setBindingSlot(-1)
   /* invocation.setRetainedStaticContext(
      instruction.makeRetainedStaticContext())*/
    invocation
  }*/

}

class UseAttributeSet(name: StructuredQName, streamable: Boolean)
  extends Instruction
    with ComponentInvocation
    with ContextOriginator {

  private var targetName: StructuredQName = name

  private var target: AttributeSet = _

  var isDeclaredStreamable: Boolean = streamable

  var bindingSlot: Int = -1

  override def isInstruction(): Boolean = false

  def setTarget(target: AttributeSet): Unit = {
    this.target = target
  }

  def getSymbolicName(): SymbolicName =
    new SymbolicName(StandardNames.XSL_ATTRIBUTE_SET, targetName)

  def getTargetAttributeSet(): AttributeSet = target

  def getFixedTarget(): Component = {
    if (target != null && bindingSlot < 0) {
      target.getDeclaringComponent
    }
    null
  }

  override def operands(): java.lang.Iterable[Operand] =
    Collections.emptyList()

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = this

  def copy(rebindings: RebindingMap): Expression = {
    val ua: UseAttributeSet =
      new UseAttributeSet(targetName, isDeclaredStreamable)
    ua.setTarget(target)
    ua.setBindingSlot(bindingSlot)
    ua
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = this

  override def getItemType(): ItemType = NodeKindTest.ATTRIBUTE

  override def getIntrinsicDependencies(): Int =
    StaticProperty.DEPENDS_ON_XSLT_CONTEXT | StaticProperty.DEPENDS_ON_FOCUS

  def getTargetAttributeSetName(): StructuredQName = targetName

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    var target: Component = null
    if (bindingSlot < 0) {
      target = getFixedTarget
    } else {
      target = context.getTargetComponent(bindingSlot)
      if (target.isHiddenAbstractComponent) {
        val err: XPathException = new XPathException(
          "Cannot expand an abstract attribute set (" + targetName.getDisplayName +
            ") with no implementation",
          "XTDE3052")
        err.setLocation(getLocation)
        throw err
      }
    }
    if (target == null) {
      throw new AssertionError(
        "Failed to locate attribute set " + getTargetAttributeSetName.getEQName)
    }
    val as: AttributeSet = target.getActor.asInstanceOf[AttributeSet]
    val c2: XPathContextMajor = context.newContext()
    c2.setCurrentComponent(target)
    c2.setOrigin(this)
    var sm: SlotManager = as.getStackFrameMap
    if (sm == null) {
      sm = SlotManager.EMPTY
    }
    c2.openStackFrame(sm)
    as.expand(output, c2)
    null
  }

  override def getExpressionName(): String = "useAS"

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("useAS", this)
    out.emitAttribute("name", targetName)
    out.emitAttribute("bSlot", "" + getBindingSlot)
    if (isDeclaredStreamable) {
      out.emitAttribute("flags", "s")
    }
    out.endElement()
  }

  override def equals(obj: Any): Boolean = {
    if (!(obj.isInstanceOf[UseAttributeSet])) {
      false
    }
    targetName == obj.asInstanceOf[UseAttributeSet].targetName
  }

  override def computeHashCode(): Int = 0x86423719 ^ targetName.hashCode

  override def getStreamerName(): String = "UseAttributeSet"

  override def setBindingSlot(slot: Int): Unit = bindingSlot = slot

  override def getBindingSlot(): Int = bindingSlot
}
