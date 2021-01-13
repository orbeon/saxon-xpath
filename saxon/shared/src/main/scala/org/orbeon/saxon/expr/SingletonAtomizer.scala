package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.Error
import org.orbeon.saxon.ma.map.MapType
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{AtomicSequence, Item}
import org.orbeon.saxon.pattern.{NodeKindTest, NodeTest}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.{AtomicValue, Cardinality}

import scala.beans.BooleanBeanProperty


class SingletonAtomizer(sequence: Expression,
                        role: RoleDiagnostic,
                        @BooleanBeanProperty var allowEmpty: Boolean)
  extends UnaryExpression(sequence) {

  private val roleDiagnostic: RoleDiagnostic = role

  def getOperandRole: OperandRole = OperandRole.SINGLE_ATOMIC

  override def simplify(): Expression = {
    val operand = getBaseExpression.simplify()
    operand match {
      case literal: Literal if literal.getValue.isInstanceOf[AtomicValue] =>
        return operand
      case _ =>
    }
    this.setBaseExpression(operand)
    this
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    val operand = getBaseExpression
    ExpressionTool.resetStaticProperties(this)
    if (Literal.isEmptySequence(operand)) {
      if (! allowEmpty) {
        typeError(
          "An empty sequence is not allowed as the " + roleDiagnostic.getMessage,
          roleDiagnostic.getErrorCode,
          null)
      }
      return operand
    }
    val operandType = operand.getItemType
    if (operandType.isPlainType)
      return operand
    if (! operandType.isAtomizable(visitor.getConfiguration.getTypeHierarchy)) {
      val err =
        operandType match {
          case _: MapType => new XPathException("Cannot atomize a map (" + toShortString + ")", "FOTY0013")
          case _: FunctionItemType => new XPathException("Cannot atomize a function item", "FOTY0013")
          case _ => new XPathException(
            "Cannot atomize an element that is defined in the schema to have element-only content",
            "FOTY0012")
        }
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      err.setFailingExpression(getParentExpression)
      throw err
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val exp = super.optimize(visitor, contextInfo)
    if (exp == this) {
      this.setBaseExpression(getBaseExpression.unordered(retainAllNodes = true, forStreaming = false))
      if (getBaseExpression.getItemType.isPlainType && ! Cardinality.allowsMany(getBaseExpression.getCardinality))
        return getBaseExpression
      this
    } else {
      exp
    }
  }

  override def computeSpecialProperties(): Int = {
    val p = super.computeSpecialProperties()
    p | StaticProperty.NO_NODES_NEWLY_CREATED
  }

  def copy(rebindings: RebindingMap): Expression = {
    val e2 = new SingletonAtomizer(
      getBaseExpression.copy(rebindings),
      roleDiagnostic,
      allowEmpty)
    ExpressionTool.copyLocationInfo(this, e2)
    e2
  }

  def getImplementationMethod: Int = Expression.EVALUATE_METHOD

  override def getStreamerName: String = "SingletonAtomizer"

  def getRole: RoleDiagnostic = roleDiagnostic

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val result = getBaseExpression.addToPathMap(pathMap, pathMapNodeSet)
    if (result != null) {
      val th = getConfiguration.getTypeHierarchy
      val operandItemType = getBaseExpression.getItemType
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
    var found = 0
    var result: AtomicValue = null
    val iter = getBaseExpression.iterate(context)
    var item: Item = null
    while ({
      item = iter.next()
      item
    } != null) {
      var seq: AtomicSequence = null
      try seq = item.atomize()
      catch {
        case e: XPathException =>
          if (roleDiagnostic == null || e.isInstanceOf[Error.UserDefinedXPathException]) {
            throw e
          } else {
            val message = e.getMessage + ". Failed while atomizing the " + roleDiagnostic.getMessage
            val e2 = new XPathException(
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
      if (found == 1)
        result = seq.head
    }
    if (found == 0 && !allowEmpty)
      typeError(
        "An empty sequence is not allowed as the " + roleDiagnostic.getMessage,
        roleDiagnostic.getErrorCode,
        null)
    result
  }

  override def getItemType: ItemType = {
    // ORBEON: Set to `true` so we can handle type annotations on our DOM.
    val isSchemaAware = true
//    try
//      isSchemaAware = getPackageData.isSchemaAware
//    catch {
//      case _: NullPointerException =>
//        if (!getConfiguration.isLicensedFeature(
//          Configuration.LicenseFeature.SCHEMA_VALIDATION)) {
//          isSchemaAware = false
//        }
//    }
    val in = getBaseExpression.getItemType
    if (in.isPlainType) {
      in
    } else
      in match {
        case _: NodeTest =>
          val kinds = in.getUType
          if (! isSchemaAware) {
            if (Atomizer.STRING_KINDS.subsumes(kinds))
              return BuiltInAtomicType.STRING
            if (Atomizer.UNTYPED_IF_UNTYPED_KINDS.subsumes(kinds))
              return BuiltInAtomicType.UNTYPED_ATOMIC
          } else {
            if (Atomizer.UNTYPED_KINDS.subsumes(kinds))
              return BuiltInAtomicType.UNTYPED_ATOMIC
          }
          in.getAtomizedItemType
        case _: JavaExternalObjectType =>
          in.getAtomizedItemType
        case _ =>
          BuiltInAtomicType.ANY_ATOMIC
      }
  }

  override def computeCardinality(): Int =
    if (allowEmpty)
      StaticProperty.ALLOWS_ZERO_OR_ONE
    else
      StaticProperty.EXACTLY_ONE

  override def getExpressionName: String = "atomizeSingleton"

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("atomSing", this)
    if (allowEmpty)
      out.emitAttribute("card", "?")
    out.emitAttribute("diag", getRole.save())
    getBaseExpression.export(out)
    out.endElement()
  }

  override def toShortString: String = getBaseExpression.toShortString
}
