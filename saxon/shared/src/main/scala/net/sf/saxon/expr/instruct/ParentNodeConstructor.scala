package net.sf.saxon.expr.instruct

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.PathMap

import net.sf.saxon.lib.ParseOptions

import net.sf.saxon.lib.Validation

import net.sf.saxon.model._

import net.sf.saxon.pattern.MultipleNodeKindTest

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.SequenceType

import ParentNodeConstructor._

import scala.beans.{BeanProperty, BooleanBeanProperty}

object ParentNodeConstructor {

  private val SAME_FOCUS_CONTENT: OperandRole =
    new OperandRole(0, OperandUsage.ABSORPTION, SequenceType.ANY_SEQUENCE)

}

abstract class ParentNodeConstructor
  extends Instruction
    with ValidatingInstruction
    with InstructionWithComplexContent {

   var contentOp: Operand = _

  @BeanProperty
  var validationOptions: ParseOptions = null

   var preservingTypes: Boolean = true

  def getSchemaType(): SchemaType =
    if (validationOptions == null) null else validationOptions.getTopLevelType

  def setValidationAction(mode: Int, schemaType: SchemaType): Unit = {
    preservingTypes = mode == Validation.PRESERVE && schemaType == null
    if (!preservingTypes) {
      if (validationOptions == null) {
        validationOptions = new ParseOptions()
      }
      if (schemaType == Untyped.getInstance) {
        validationOptions.setSchemaValidationMode(Validation.SKIP)
      } else {
        validationOptions.setSchemaValidationMode(mode)
        validationOptions.setTopLevelType(schemaType)
      }
    }
  }

  def getValidationAction(): Int =
    if (validationOptions == null) Validation.PRESERVE
    else validationOptions.getSchemaValidationMode

  def setNoNeedToStrip(): Unit = {
    preservingTypes = true
  }

  def setContentExpression(content: Expression): Unit = {
    if (contentOp == null) {
      contentOp = new Operand(this, content, SAME_FOCUS_CONTENT)
    } else {
      contentOp.setChildExpression(content)
    }
  }

  def getContentExpression(): Expression =
    if (contentOp == null) null else contentOp.getChildExpression

  def getContentOperand: Operand = contentOp

  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    checkContentSequence(visitor.getStaticContext)
    this
  }

  override def allowExtractingCommonSubexpressions(): Boolean = false

   def checkContentSequence(env: StaticContext): Unit

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextItemType)
    if (!Literal.isEmptySequence(getContentExpression)) {
      if (getContentExpression.isInstanceOf[Block]) {
        this.setContentExpression(getContentExpression)
          .asInstanceOf[Block]
          .mergeAdjacentTextInstructions()
      }
      if (visitor.isOptimizeForStreaming) {
        visitor.obtainOptimizer().makeCopyOperationsExplicit(this, contentOp)
      }
    }
    if (visitor.getStaticContext.getPackageData.isSchemaAware) {
      val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
      if (getValidationAction == Validation.STRIP) {
        if (getContentExpression.hasSpecialProperty(
          StaticProperty.ALL_NODES_UNTYPED) ||
          th.relationship(getContentExpression.getItemType,
            MultipleNodeKindTest.DOC_ELEM_ATTR) ==
            Affinity.DISJOINT) {
          setNoNeedToStrip()
        }
      }
    } else {
      setValidationAction(Validation.STRIP, null)
      setNoNeedToStrip()
    }
    this
  }

  override def mayCreateNewNodes(): Boolean = true

  override def alwaysCreatesNewNodes(): Boolean = true

  override def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val result: PathMap.PathMapNodeSet =
      super.addToPathMap(pathMap, pathMapNodeSet)
    result.setReturnable(false)
    val th: TypeHierarchy = getConfiguration.getTypeHierarchy
    val `type`: ItemType = getItemType
    if (th.relationship(`type`, NodeKindTest.ELEMENT) != Affinity.DISJOINT ||
      th.relationship(`type`, NodeKindTest.DOCUMENT) != Affinity.DISJOINT) {
      result.addDescendants()
    }
    new PathMap.PathMapNodeSet(pathMap.makeNewRoot(this))
  }

  def isPreservingTypes: Boolean = preservingTypes

  def isLocal: Boolean = ExpressionTool.isLocalConstructor(this)

}
