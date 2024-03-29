package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.functions.String_1

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.TypeHierarchy

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.SingletonIterator

import org.orbeon.saxon.tree.util.Orphan

import org.orbeon.saxon.value.Cardinality

abstract class SimpleNodeConstructor extends Instruction {

  var select: Expression = Literal.makeEmptySequence
  var selectOp: Operand =
    new Operand(this, select, OperandRole.SINGLE_ATOMIC)

  def setSelect(select: Expression): Unit = {
    selectOp.setChildExpression(select)
  }

  def getSelect: Expression = selectOp.getChildExpression

  override def operands: java.lang.Iterable[Operand] = selectOp

  override def mayCreateNewNodes(): Boolean = true

  override def alwaysCreatesNewNodes(): Boolean = true

  override def computeCardinality(): Int = getSelect.getCardinality

  override def computeSpecialProperties(): Int =
    super.computeSpecialProperties() | StaticProperty.SINGLE_DOCUMENT_NODESET

  override def allowExtractingCommonSubexpressions(): Boolean = false

  def localTypeCheck(visitor: ExpressionVisitor,
                     contextItemType: ContextItemStaticInfo): Unit

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    localTypeCheck(visitor, contextInfo)
    val th = visitor.getConfiguration.getTypeHierarchy
    if (getSelect.isInstanceOf[ValueOf]) {
      val valSelect: Expression = getSelect.asInstanceOf[ValueOf].getSelect
      if (th.isSubType(valSelect.getItemType, BuiltInAtomicType.STRING) &&
        !Cardinality.allowsMany(valSelect.getCardinality)) {
        this.setSelect(valSelect)
      }
    }
    if (getSelect.isCallOn(classOf[String_1])) {
      val fn: SystemFunctionCall = getSelect.asInstanceOf[SystemFunctionCall]
      val arg: Expression = fn.getArg(0)
      if (arg.getItemType == BuiltInAtomicType.UNTYPED_ATOMIC &&
        !Cardinality.allowsMany(arg.getCardinality)) {
        this.setSelect(arg)
      }
    } else if (getSelect.isInstanceOf[CastExpression] &&
      getSelect.asInstanceOf[CastExpression].getTargetType ==
        BuiltInAtomicType.STRING) {
      val arg: Expression =
        getSelect.asInstanceOf[CastExpression].getBaseExpression
      if (arg.getItemType == BuiltInAtomicType.UNTYPED_ATOMIC &&
        !Cardinality.allowsMany(arg.getCardinality)) {
        this.setSelect(arg)
      }
    }
    adoptChildExpression(getSelect)
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextItemType)
    if (getSelect.isCallOn(classOf[String_1])) {
      val sf: SystemFunctionCall = getSelect.asInstanceOf[SystemFunctionCall]
      val th = visitor.getConfiguration.getTypeHierarchy
      if (th.isSubType(sf.getArg(0).getItemType, BuiltInAtomicType.STRING) &&
        !Cardinality.allowsMany(sf.getArg(0).getCardinality)) {
        this.setSelect(sf.getArg(0))
      }
    }
    this
  }

  override def getStreamerName: String = "SimpleNodeConstructor"

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val value: CharSequence = getSelect.evaluateAsString(context)
    try processValue(value, output, context)
    catch {
      case e: XPathException => {
        e.maybeSetLocation(getLocation)
        throw e
      }

    }
    null
  }

  def processValue(value: CharSequence,
                   output: Outputter,
                   context: XPathContext): Unit

  override def evaluateItem(context: XPathContext): Item = {
    val contentItem: Item = getSelect.evaluateItem(context)
    var content: String = null
    if (contentItem == null) {
      content = ""
    } else {
      content = contentItem.getStringValue
      content = checkContent(content, context)
    }
    val o: Orphan = new Orphan(context.getConfiguration)
    o.setNodeKind(getItemType.getPrimitiveType.toShort)
    o.setStringValue(content)
    o.setNodeName(evaluateNodeName(context))
    o
  }

  def checkContent(data: String, context: XPathContext): String =
    data

  def evaluateNodeName(context: XPathContext): NodeName = null

  override def iterate(context: XPathContext): SequenceIterator =
    SingletonIterator.makeIterator(evaluateItem(context))

  def isLocal: Boolean = ExpressionTool.isLocalConstructor(this)

}
