package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.Error

import net.sf.saxon.ma.map.MapType

import net.sf.saxon.model._

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.pattern.NodeKindTest

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Cardinality

import scala.beans.{BeanProperty, BooleanBeanProperty}

class SingletonAtomizer(sequence: Expression,
                        role: RoleDiagnostic,
                        @BooleanBeanProperty var allowEmpty: Boolean)
  extends UnaryExpression(sequence) {

  private var roleDiagnostic: RoleDiagnostic = role

  def getOperandRole(): OperandRole = OperandRole.SINGLE_ATOMIC

  override def simplify(): Expression = {
    val operand: Expression = getBaseExpression.simplify()
    if (operand.isInstanceOf[Literal] &&
      operand.asInstanceOf[Literal].getValue.isInstanceOf[AtomicValue]) {
      return operand
    }
    this.setBaseExpression(operand)
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    val operand: Expression = getBaseExpression
    ExpressionTool.resetStaticProperties(this)
    if (Literal.isEmptySequence(operand)) {
      if (!allowEmpty) {
        typeError(
          "An empty sequence is not allowed as the " + roleDiagnostic.getMessage,
          roleDiagnostic.getErrorCode,
          null)
      }
      return operand
    }
    val operandType: ItemType = operand.getItemType
    if (operandType.isPlainType) {
      return operand
    }
    if (!operandType.isAtomizable(visitor.getConfiguration.getTypeHierarchy)) {
      var err: XPathException = null
      err =
        if (operandType.isInstanceOf[MapType])
          new XPathException("Cannot atomize a map (" + toShortString() + ")",
            "FOTY0013")
        else if (operandType.isInstanceOf[FunctionItemType])
          new XPathException("Cannot atomize a function item", "FOTY0013")
        else
          new XPathException(
            "Cannot atomize an element that is defined in the schema to have element-only content",
            "FOTY0012")
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      err.setFailingExpression(getParentExpression)
      throw err
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val exp: Expression = super.optimize(visitor, contextInfo)
    if (exp == this) {
      this.setBaseExpression(getBaseExpression.unordered(true, false))
      if (getBaseExpression.getItemType.isPlainType &&
        !Cardinality.allowsMany(getBaseExpression.getCardinality)) {
        getBaseExpression
      }
      this
    } else {
      exp
    }
  }

  override def computeSpecialProperties(): Int = {
    val p: Int = super.computeSpecialProperties()
    p | StaticProperty.NO_NODES_NEWLY_CREATED
  }

  def copy(rebindings: RebindingMap): Expression = {
    val e2: Expression = new SingletonAtomizer(
      getBaseExpression.copy(rebindings),
      roleDiagnostic,
      allowEmpty)
    ExpressionTool.copyLocationInfo(this, e2)
    e2
  }

  override def getImplementationMethod(): Int = Expression.EVALUATE_METHOD

  override def getStreamerName(): String = "SingletonAtomizer"

  def getRole(): RoleDiagnostic = roleDiagnostic

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val result: PathMap.PathMapNodeSet =
      getBaseExpression.addToPathMap(pathMap, pathMapNodeSet)
    if (result != null) {
      val th: TypeHierarchy = getConfiguration.getTypeHierarchy
      val operandItemType: ItemType = getBaseExpression.getItemType
      if (th.relationship(NodeKindTest.ELEMENT, operandItemType) !=
        Affinity.DISJOINT ||
        th.relationship(NodeKindTest.DOCUMENT, operandItemType) !=
          Affinity.DISJOINT) {
        result.setAtomized()
      }
    }
    null
  }

  override def evaluateItem(context: XPathContext): AtomicValue = {
    var found: Int = 0
    var result: AtomicValue = null
    val iter: SequenceIterator = getBaseExpression.iterate(context)
    var item: Item = null
    while (({
      item = iter.next()
      item
    }) != null) {
      var seq: AtomicSequence = null
      try seq = item.atomize()
      catch {
        case e: XPathException =>
          if (roleDiagnostic == null || e
            .isInstanceOf[Error.UserDefinedXPathException]) {
            throw e
          } else {
            val message: String = e.getMessage + ". Failed while atomizing the " + roleDiagnostic.getMessage
            val e2: XPathException = new XPathException(
              message,
              e.getErrorCodeLocalPart,
              e.getLocator)
            e2.setXPathContext(context)
            e2.maybeSetLocation(getLocation)
            throw e2
          }

      }
      found += seq.getLength
      if (found > 1) {
        typeError(
          "A sequence of more than one item is not allowed as the " +
            roleDiagnostic.getMessage +
            CardinalityChecker
              .depictSequenceStart(getBaseExpression.iterate(context), 3),
          roleDiagnostic.getErrorCode,
          context
        )
      }
      if (found == 1) {
        result = seq.head()
      }
    }
    if (found == 0 && !allowEmpty) {
      typeError(
        "An empty sequence is not allowed as the " + roleDiagnostic.getMessage,
        roleDiagnostic.getErrorCode,
        null)
    }
    result
  }

  override def getItemType(): ItemType = {
    var isSchemaAware: Boolean = true
    try isSchemaAware = getPackageData.isSchemaAware
    catch {
      case err: NullPointerException =>
        if (!getConfiguration.isLicensedFeature(
          Configuration.LicenseFeature.SCHEMA_VALIDATION)) {
          isSchemaAware = false
        }

    }
    val in: ItemType = getBaseExpression.getItemType
    if (in.isPlainType) {
      return in
    } else if (in.isInstanceOf[NodeTest]) {
      val kinds: UType = in.getUType
      if (!isSchemaAware) {
        if (Atomizer.STRING_KINDS.subsumes(kinds)) {
          BuiltInAtomicType.STRING
        }
        if (Atomizer.UNTYPED_IF_UNTYPED_KINDS.subsumes(kinds)) {
          BuiltInAtomicType.UNTYPED_ATOMIC
        }
      } else {
        if (Atomizer.UNTYPED_KINDS.subsumes(kinds)) {
          BuiltInAtomicType.UNTYPED_ATOMIC
        }
      }
      in.getAtomizedItemType
    } else if (in.isInstanceOf[JavaExternalObjectType]) {
      in.getAtomizedItemType
    }
    BuiltInAtomicType.ANY_ATOMIC
  }

  override def computeCardinality(): Int =
    if (allowEmpty) {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    } else {
      StaticProperty.EXACTLY_ONE
    }

  override def getExpressionName(): String = "atomizeSingleton"

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("atomSing", this)
    if (allowEmpty) {
      out.emitAttribute("card", "?")
    }
    out.emitAttribute("diag", getRole.save())
    getBaseExpression.export(out)
    out.endElement()
  }

  override def toShortString(): String = getBaseExpression.toShortString()

}
