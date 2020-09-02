package net.sf.saxon.expr

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.instruct.Block
import net.sf.saxon.expr.parser.ContextItemStaticInfo
import net.sf.saxon.expr.parser.ExpressionTool
import net.sf.saxon.expr.parser.ExpressionVisitor
import net.sf.saxon.expr.parser.RebindingMap
import net.sf.saxon.ma.arrays.ArrayItem
import net.sf.saxon.ma.arrays.ArrayItemType
import net.sf.saxon.ma.arrays.SquareArrayConstructor
import net.sf.saxon.ma.map.KeyValuePair
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.ma.map.MapType
import net.sf.saxon.ma.map.TupleType
import net.sf.saxon.model._
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.Item
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import java.util.ArrayList
import java.util.Iterator
import java.util.List

import scala.collection
import scala.jdk.CollectionConverters._
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
    val th: TypeHierarchy = config.getTypeHierarchy
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
      new Literal(iterate(visitor.makeDynamicContext()).materialize())
    }
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    getOperand.optimize(visitor, contextItemType)
    if (getBaseExpression.isInstanceOf[Literal]) {
      new Literal(iterate(visitor.makeDynamicContext()).materialize())
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
              level1 = base.asInstanceOf[ArrayItem].members().iterator
              next()
            } else if (base.isInstanceOf[MapItem]) {
              level1 = base.asInstanceOf[MapItem].keyValuePairs().iterator.asScala
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
                throw new IllegalStateException()
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
