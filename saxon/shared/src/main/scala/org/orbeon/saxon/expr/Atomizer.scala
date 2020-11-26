package org.orbeon.saxon.expr

import org.orbeon.saxon.event.ReceiverOption
import org.orbeon.saxon.expr.Atomizer._
import org.orbeon.saxon.expr.instruct.{Block, Choose, ValueOf}
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.Error
import org.orbeon.saxon.ma.arrays.ArrayItemType
import org.orbeon.saxon.ma.map.MapType
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.{NameTest, NodeKindTest, NodeTest}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.{AtomizingIterator, EmptyIterator, UntypedAtomizingIterator}
import org.orbeon.saxon.value.{AtomicValue, Cardinality, EmptySequence}

import scala.beans.{BeanProperty, BooleanBeanProperty}


object Atomizer {

  def makeAtomizer(sequence: Expression, role: RoleDiagnostic): Expression =
    sequence match {
      case literal: Literal if literal.getValue.isInstanceOf[AtomicSequence] =>
        sequence
      case _ =>
        new Atomizer(sequence, role)
    }

  def getAtomizedItemType(operand: Expression,
                          alwaysUntyped: Boolean,
                          th: TypeHierarchy): ItemType = {
    val in = operand.getItemType
    if (in.isPlainType) {
      in
    } else
      in match {
        case _: NodeTest =>
          val kinds = in.getUType
          if (alwaysUntyped) {
            if (STRING_KINDS.subsumes(kinds))
              return BuiltInAtomicType.STRING
            if (UNTYPED_IF_UNTYPED_KINDS.subsumes(kinds))
              return BuiltInAtomicType.UNTYPED_ATOMIC
          } else {
            if (UNTYPED_KINDS.subsumes(kinds))
              return BuiltInAtomicType.UNTYPED_ATOMIC
          }
          in.getAtomizedItemType
        case _: JavaExternalObjectType =>
          in.getAtomizedItemType
        case arrayItemType: ArrayItemType =>
          val ait: PlainType = arrayItemType
            .getMemberType
            .getPrimaryType
            .getAtomizedItemType
          if (ait == null) ErrorType else ait
        case _: FunctionItemType =>
          ErrorType
        case _ =>
          BuiltInAtomicType.ANY_ATOMIC
      }
  }

  val STRING_KINDS: UType =
    UType.NAMESPACE.union(UType.COMMENT).union(UType.PI)

  val UNTYPED_KINDS: UType = UType.TEXT.union(UType.DOCUMENT)

  val UNTYPED_IF_UNTYPED_KINDS: UType = UType.TEXT
    .union(UType.ELEMENT)
    .union(UType.DOCUMENT)
    .union(UType.ATTRIBUTE)

  def getAtomizingIterator(base: SequenceIterator,
                           oneToOne: Boolean): SequenceIterator = {
    val properties: Set[SequenceIterator.Property.Property] = base.getProperties
    if (properties.contains(SequenceIterator.Property.LAST_POSITION_FINDER)) {
      val count = base.asInstanceOf[LastPositionFinder].getLength
      if (count == 0) {
        return EmptyIterator.emptyIterator
      } else if (count == 1) {
        val first = base.next()
        return first.atomize().iterate()
      }
    } else if (properties.contains(SequenceIterator.Property.ATOMIZING)) {
      return new AxisAtomizingIterator(base.asInstanceOf[AtomizedValueIterator])
    }
    if (oneToOne)
      new UntypedAtomizingIterator(base)
    else
      new AtomizingIterator(base)
  }

  def atomize(sequence: Sequence): AtomicSequence =
    sequence match {
      case atomicSequence: AtomicSequence =>
        atomicSequence
      case _: EmptySequence[_] =>
        EmptyAtomicSequence.getInstance
      case _ =>
        val iter = getAtomizingIterator(sequence.iterate(), oneToOne = false)
        new AtomicArray(iter)
    }
}

class Atomizer(sequence: Expression, role: RoleDiagnostic)
  extends UnaryExpression(sequence) {

  @BooleanBeanProperty
  var untyped: Boolean = false

  private var singleValued: Boolean = false

  @BeanProperty
  var operandItemType: ItemType = getBaseExpression.getItemType

  private var roleDiagnostic: RoleDiagnostic = role

  sequence.setFlattened(true)

  def getOperandRole: OperandRole = OperandRole.ATOMIC_SEQUENCE

  def getImplementationMethod: Int = Expression.ITERATE_METHOD | Expression.WATCH_METHOD

  def setRoleDiagnostic(role: RoleDiagnostic): Unit =
    this.roleDiagnostic = role

  override def simplify(): Expression = {
    untyped = ! getPackageData.isSchemaAware
    computeSingleValued(getConfiguration.getTypeHierarchy)
    val operand = getBaseExpression.simplify()
    operand match {
      case literal: Literal =>
        val `val` = literal.getValue
        if (`val`.isInstanceOf[AtomicValue])
          return operand
        val iter = `val`.iterate()
        var i: Item = null
        while ({
          i = iter.next()
          i
        } != null) {
          i match {
            case _: NodeInfo =>
              return this
            case function: Function =>
              if (function.isArray) {
                return this
              } else if (function.isMap) {
                val err = new XPathException(expandMessage("Cannot atomize a map (" + i.toShortString + ")"), "FOTY0013")
                err.setIsTypeError(true)
                err.setLocation(getLocation)
                throw err
              } else {
                val err = new XPathException(expandMessage("Cannot atomize a function item"), "FOTY0013")
                err.setIsTypeError(true)
                err.setLocation(getLocation)
                throw err
              }
            case _ =>
          }
        }
        operand
      case of: ValueOf if ! ReceiverOption.contains(of.getOptions, ReceiverOption.DISABLE_ESCAPING) =>
        of.convertToCastAsString
      case _ =>
        this.setBaseExpression(operand)
        this
    }
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getOperand.typeCheck(visitor, contextInfo)
    untyped = untyped | ! visitor.getStaticContext.getPackageData.isSchemaAware
    val th = visitor.getConfiguration.getTypeHierarchy
    computeSingleValued(th)
    resetLocalStaticProperties()
    val operandType = getOperandItemType
    if (th.isSubType(operandType, BuiltInAtomicType.ANY_ATOMIC))
      return getBaseExpression
    if (! operandType.isAtomizable(th)) {
      val err =
        if (operandType.isInstanceOf[FunctionItemType]) {
          val thing = if (operandType.isInstanceOf[MapType]) "map" else "function item"
          new XPathException(expandMessage("Cannot atomize a " + thing), "FOTY0013")
        } else {
          new XPathException(
            expandMessage(
              "Cannot atomize an element that is defined in the schema to have element-only content"),
            "FOTY0012")
        }
      err.setIsTypeError(true)
      err.setLocation(getLocation)
      throw err
    }
    getBaseExpression.setFlattened(true)
    this
  }

  private def computeSingleValued(th: TypeHierarchy): Unit = {
    val operandType = getOperandItemType
    if (th.relationship(operandType, ArrayItemType.ANY_ARRAY_TYPE) !=
      Affinity.DISJOINT) {
      singleValued = false
    } else {
      singleValued = untyped
      if (! singleValued) {
        val nodeType: ItemType = getBaseExpression.getItemType
        nodeType match {
          case nodeTest: NodeTest =>
            val st = nodeTest.getContentType
            if (st == Untyped.getInstance || st.isAtomicType || (st.isComplexType && st != AnyType.getInstance))
              singleValued = true
            if (! nodeType.getUType.overlaps(
              UType.ELEMENT.union(UType.ATTRIBUTE))) {
              singleValued = true
            }
          case _ =>
        }
      }
    }
  }

  private def expandMessage(message: String): String =
    if (roleDiagnostic == null)
      message
    else
      message + ". Found while atomizing the " + roleDiagnostic.getMessage

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val exp = super.optimize(visitor, contextInfo)
    if (exp eq this) {
      val th = visitor.getConfiguration.getTypeHierarchy
      val operand = getBaseExpression
      if (th.isSubType(operand.getItemType, BuiltInAtomicType.ANY_ATOMIC))
        return operand
      operand match {
        case valueOf: ValueOf if ! ReceiverOption.contains(valueOf.getOptions, ReceiverOption.DISABLE_ESCAPING) =>
          val cast = valueOf.convertToCastAsString
          return cast.optimize(visitor, contextInfo)
        case assignation @ (_: LetExpression | _: ForExpression) =>

          // ORBEON: Would be nice if we could just call `assignation.getAction`, but the LUB is not computed, at
          // least with Scala 2.13.

          val action = operand.asInstanceOf[Assignation].getAction
          operand
            .asInstanceOf[Assignation]
            .setAction(new Atomizer(action, roleDiagnostic))
          return operand.optimize(visitor, contextInfo)
        case choose: Choose =>
          choose.atomizeActions()
          return operand.optimize(visitor, contextInfo)
        case block: Block =>
          val children = block.getOperanda
          val atomizedChildren: Array[Expression] =
            Array.ofDim[Expression](children.length)
          for (i <- children.indices)
            atomizedChildren(i) = new Atomizer(children(i).getChildExpression, roleDiagnostic)
          val newBlock: Block = new Block(atomizedChildren)
          return newBlock.typeCheck(visitor, contextInfo).optimize(visitor, contextInfo)
        case _ =>
      }
      if (untyped && operand.isInstanceOf[AxisExpression] &&
        operand.asInstanceOf[AxisExpression].getAxis == AxisInfo.ATTRIBUTE &&
        operand
          .asInstanceOf[AxisExpression]
          .getNodeTest
          .isInstanceOf[NameTest] &&
        ! operand.asInstanceOf[AxisExpression].isContextPossiblyUndefined) {
        val name =
          operand.asInstanceOf[AxisExpression].getNodeTest.getMatchingNodeName
        val qName: FingerprintedQName =
          new FingerprintedQName(name, visitor.getConfiguration.getNamePool)
        val ag: AttributeGetter = new AttributeGetter(qName)
        var checks: Int = 0
        if (!(operand
          .asInstanceOf[AxisExpression]
          .getContextItemType
          .isInstanceOf[NodeTest])) {
          checks = AttributeGetter.CHECK_CONTEXT_ITEM_IS_NODE
        }
        ag.setRequiredChecks(checks)
        ExpressionTool.copyLocationInfo(this, ag)
        return ag
      }
    }
    exp
  }

  override def computeSpecialProperties(): Int = {
    var p: Int = super.computeSpecialProperties()
    p &= ~StaticProperty.NODESET_PROPERTIES
    p | StaticProperty.NO_NODES_NEWLY_CREATED
  }

  override def resetLocalStaticProperties(): Unit = {
    super.resetLocalStaticProperties()
    operandItemType = null
  }

  def copy(rebindings: RebindingMap): Expression = {
    val copy: Atomizer =
      new Atomizer(getBaseExpression.copy(rebindings), roleDiagnostic)
    copy.untyped = untyped
    copy.singleValued = singleValued
    ExpressionTool.copyLocationInfo(this, copy)
    copy
  }

  override def getStreamerName: String = "Atomizer"

  override def iterate(context: XPathContext): SequenceIterator =
    try {
      val base: SequenceIterator = getBaseExpression.iterate(context)
      getAtomizingIterator(base,
        untyped && operandItemType.isInstanceOf[NodeTest])
    } catch {
      case e: XPathException =>
        if (roleDiagnostic == null || e
          .isInstanceOf[Error.UserDefinedXPathException]) {
          throw e
        } else {
          val message: String = expandMessage(e.getMessage)
          val e2: XPathException =
            new XPathException(message, e.getErrorCodeLocalPart, e.getLocator)
          e2.setXPathContext(context)
          e2.maybeSetLocation(getLocation)
          throw e2
        }

    }

  override def evaluateItem(context: XPathContext): AtomicValue = {
    val i: Item = getBaseExpression.evaluateItem(context)
    if (i == null) {
      null
    } else {
      i.atomize().head
    }
  }

  override def getItemType: ItemType = {
    operandItemType = getBaseExpression.getItemType
    val th = getConfiguration.getTypeHierarchy
    getAtomizedItemType(getBaseExpression, untyped, th)
  }

  override def computeCardinality(): Int = {
    val in = getOperandItemType
    val operand = getBaseExpression
    if (singleValued) {
      return operand.getCardinality
    } else if (untyped && in.isInstanceOf[NodeTest]) {
      return operand.getCardinality
    } else if (Cardinality.allowsMany(operand.getCardinality)) {
      return StaticProperty.ALLOWS_ZERO_OR_MORE
    } else if (in.isPlainType) {
      return operand.getCardinality
    } else if (in.isInstanceOf[NodeTest]) {
      val schemaType = in.asInstanceOf[NodeTest].getContentType
      if (schemaType.isAtomicType)
        return operand.getCardinality
    }
    StaticProperty.ALLOWS_ZERO_OR_MORE
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val result: PathMap.PathMapNodeSet =
      getBaseExpression.addToPathMap(pathMap, pathMapNodeSet)
    if (result != null) {
      val th = getConfiguration.getTypeHierarchy
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

  override def getExpressionName: String = "data"

  override def toString: String = "data(" + getBaseExpression.toString + ")"

  override def toShortString: String = getBaseExpression.toShortString

  override def emitExtraAttributes(out: ExpressionPresenter): Unit = {
    if (roleDiagnostic != null) {
      out.emitAttribute("diag", roleDiagnostic.save())
    }
  }

}
