package org.orbeon.saxon.expr

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.expr.instruct.Block
import org.orbeon.saxon.expr.parser.ContextItemStaticInfo
import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.expr.parser.ExpressionVisitor
import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.ma.arrays.ArrayItem
import org.orbeon.saxon.ma.arrays.ArrayItemType
import org.orbeon.saxon.ma.arrays.SquareArrayConstructor
import org.orbeon.saxon.ma.map.KeyValuePair
import org.orbeon.saxon.ma.map.MapItem
import org.orbeon.saxon.ma.map.MapType
import org.orbeon.saxon.ma.map.TupleType
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.GroundedValue
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import java.util.ArrayList
import java.util.Iterator
import java.util.List

import scala.collection
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class LookupAllExpression(base: Expression) extends UnaryExpression(base) {

  override def getOperandRole(): OperandRole = OperandRole.INSPECT

  override def getItemType: ItemType = {
    val base: ItemType = getBaseExpression.getItemType
    if (base.isInstanceOf[MapType]) {
      base.asInstanceOf[MapType].getValueType.getPrimaryType
    } else if (base.isInstanceOf[ArrayItemType]) {
      base.asInstanceOf[ArrayItemType].getMemberType.getPrimaryType
    } else {
      AnyItemType
    }
  }

  override def getStaticUType(contextItemType: UType): UType =
    getItemType.getUType

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    val th = config.getTypeHierarchy
    getOperand.typeCheck(visitor, contextInfo)
    val containerType: ItemType = getBaseExpression.getItemType
    val isArrayLookup: Boolean = containerType.isInstanceOf[ArrayItemType]
    val isMapLookup: Boolean = containerType
      .isInstanceOf[MapType] || containerType.isInstanceOf[TupleType]
    if (!isArrayLookup && !isMapLookup) {
      if (th.relationship(containerType, MapType.ANY_MAP_TYPE) ==
        Affinity.DISJOINT &&
        th.relationship(containerType, AnyFunctionType) ==
          Affinity.DISJOINT) {
        val err = new XPathException(
          "The left-hand operand of '?' must be a map or an array; the supplied expression is of type " +
            containerType,
          "XPTY0004")
        err.setLocation(getLocation)
        err.setIsTypeError(true)
        err.setFailingExpression(this)
        throw err
      }
    }
    if (getBaseExpression.isInstanceOf[Literal]) {
      new Literal(iterate(visitor.makeDynamicContext()).materialize)
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextItemType)
    if (getBaseExpression.isInstanceOf[Literal]) {
      new Literal(iterate(visitor.makeDynamicContext()).materialize)
    }
    if (getBaseExpression.isInstanceOf[SquareArrayConstructor]) {
      val children: List[Expression] = new ArrayList[Expression]()
      for (o <- getBaseExpression.operands.asScala) {
        children.add(o.getChildExpression.copy(new RebindingMap()))
      }
      val childExpressions: Array[Expression] =
        children.toArray(Array.ofDim[Expression](0))
      val block: Block = new Block(childExpressions)
      ExpressionTool.copyLocationInfo(this, block)
      return block
    }
    this
  }

  override def getCost: Double = getBaseExpression.getCost + 1

  override def getImplementationMethod: Int = Expression.ITERATE_METHOD

  def copy(rebindings: RebindingMap): LookupAllExpression =
    new LookupAllExpression(getBaseExpression.copy(rebindings))

  override def computeCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_MORE

  override def equals(other: Any): Boolean = {
    if (!(other.isInstanceOf[LookupAllExpression])) {
      return false
    }
    val p: LookupAllExpression = other.asInstanceOf[LookupAllExpression]
    getBaseExpression.isEqual(p.getBaseExpression)
  }

  override def computeHashCode(): Int =
    "LookupAll".hashCode ^ getBaseExpression.hashCode

  override def iterate(context: XPathContext): SequenceIterator =
    new SequenceIterator {
      val level0: SequenceIterator = getBaseExpression.iterate(context)

      var level1: collection.Iterator[_] = null

      var level2: SequenceIterator = null

      override def next(): Item =
        if (level2 == null) {
          if (level1 == null) {
            val base: Item = level0.next()
            if (base == null) {
              null
            } else if (base.isInstanceOf[ArrayItem]) {
              level1 = base.asInstanceOf[ArrayItem].members.iterator
              next()
            } else if (base.isInstanceOf[MapItem]) {
              level1 = base.asInstanceOf[MapItem].keyValuePairs.iterator.asScala
              next()
            } else {
              LookupExpression.mustBeArrayOrMap(LookupAllExpression.this, base)
              null
            }
          } else {
            if (level1.hasNext) {
              val nextEntry: Any = level1.next()
              if (nextEntry.isInstanceOf[KeyValuePair]) {
                val value: GroundedValue =
                  nextEntry.asInstanceOf[KeyValuePair].value
                level2 = value.iterate()
              } else if (nextEntry.isInstanceOf[GroundedValue]) {
                level2 = nextEntry.asInstanceOf[GroundedValue].iterate()
              } else {
                throw new IllegalStateException
              }
            } else {
              level1 = null
            }
            next()
          }
        } else {
          val next: Item = level2.next()
          if (next == null) {
            level2 = null
            next
          } else {
            next
          }
        }

      override def close(): Unit = {
        if (level0 != null) {
          level0.close()
        }
        if (level2 != null) {
          level2.close()
        }
      }
    }

  override def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("lookupAll", this)
    getBaseExpression.export(destination)
    destination.endElement()
  }

  override def toString: String =
    ExpressionTool.parenthesize(getBaseExpression) + "?*"

  override def toShortString: String = getBaseExpression.toShortString + "?*"

}
