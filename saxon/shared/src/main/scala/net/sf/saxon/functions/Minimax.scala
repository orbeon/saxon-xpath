package net.sf.saxon.functions

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.sort.AtomicComparer

import net.sf.saxon.expr.sort.DescendingComparer

import net.sf.saxon.expr.sort.GenericAtomicComparer

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.model._

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value._

import java.util.Properties

import Minimax._

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.util.control.Breaks._

object Minimax {

  def minimax(iter: SequenceIterator,
              isMaxFunction: Boolean,
              atomicComparer: AtomicComparer,
              ignoreNaN: Boolean,
              context: XPathContext): AtomicValue = {
    var atomicComp = atomicComparer
    val rules: ConversionRules = context.getConfiguration.getConversionRules
    val converter: StringToDouble =
      context.getConfiguration.getConversionRules.getStringToDoubleConverter
    var foundDouble: Boolean = false
    var foundFloat: Boolean = false
    var foundNaN: Boolean = false
    var foundString: Boolean = false
    if (isMaxFunction) {
      atomicComp = new DescendingComparer(atomicComp)
    }
    atomicComp = atomicComp.provideContext(context)
    var min: AtomicValue = null
    var prim: AtomicValue = null
    breakable {
      while (true) {
        min = iter.next().asInstanceOf[AtomicValue]
        if (min == null) {
          null
        }
        prim = min
        if (min.isInstanceOf[UntypedAtomicValue]) {
          try {
            min = new DoubleValue(converter.stringToNumber(min.getStringValueCS))
            prim = min
            foundDouble = true
          } catch {
            case e: NumberFormatException => {
              val de: XPathException = new XPathException(
                "Failure converting " + Err.wrap(min.getStringValueCS) +
                  " to a number")
              de.setErrorCode("FORG0001")
              de.setXPathContext(context)
              throw de
            }

          }
        } else {
          if (prim.isInstanceOf[DoubleValue]) {
            foundDouble = true
          } else if (prim.isInstanceOf[FloatValue]) {
            foundFloat = true
          } else if (prim.isInstanceOf[StringValue] && !(prim
            .isInstanceOf[AnyURIValue])) {
            foundString = true
          }
        }
        if (prim.isNaN) {
          if (ignoreNaN) {} else if (prim.isInstanceOf[DoubleValue]) {
            min
          } else {
            foundNaN = true
            min = FloatValue.NaN
            break
          }
        } else {
          if (!prim.getPrimitiveType.isOrdered(false)) {
            val de: XPathException = new XPathException(
              "Type " + prim.getPrimitiveType + " is not an ordered type")
            de.setErrorCode("FORG0006")
            de.setIsTypeError(true)
            de.setXPathContext(context)
            throw de
          }
          break
        }
      }
    }
    breakable {
      while (true) {
        val test: AtomicValue = iter.next().asInstanceOf[AtomicValue]
        if (test == null) {
          break
        }
        var test2: AtomicValue = test
        prim = test2
        if (test.isInstanceOf[UntypedAtomicValue]) {
          try {
            test2 = new DoubleValue(
              converter.stringToNumber(test.getStringValueCS))
            if (foundNaN) {
              DoubleValue.NaN
            }
            prim = test2
            foundDouble = true
          } catch {
            case e: NumberFormatException => {
              val de: XPathException = new XPathException(
                "Failure converting " + Err.wrap(test.getStringValueCS) +
                  " to a number")
              de.setErrorCode("FORG0001")
              de.setXPathContext(context)
              throw de
            }

          }
        } else {
          if (prim.isInstanceOf[DoubleValue]) {
            if (foundNaN) {
              DoubleValue.NaN
            }
            foundDouble = true
          } else if (prim.isInstanceOf[FloatValue]) {
            foundFloat = true
          } else if (prim.isInstanceOf[StringValue] && !(prim
            .isInstanceOf[AnyURIValue])) {
            foundString = true
          }
        }
        if (prim.isNaN) {
          if (ignoreNaN) {} else if (foundDouble) {
            DoubleValue.NaN
          } else {
            foundNaN = true
          }
        } else {
          try if (atomicComp.compareAtomicValues(prim, min) < 0) {
            min = test2
          } catch {
            case err: ClassCastException =>
              if (min.getItemType == test2.getItemType) {
                throw err
              } else {
                val de: XPathException = new XPathException(
                  "Cannot compare " + min.getItemType + " with " + test2.getItemType)
                de.setErrorCode("FORG0006")
                de.setIsTypeError(true)
                de.setXPathContext(context)
                throw de
              }

          }
        }
      }
    }
    if (foundNaN) {
      FloatValue.NaN
    }
    if (foundDouble) {
      if (!(min.isInstanceOf[DoubleValue])) {
        min = Converter.convert(min, BuiltInAtomicType.DOUBLE, rules)
      }
    } else if (foundFloat) {
      if (!(min.isInstanceOf[FloatValue])) {
        min = Converter.convert(min, BuiltInAtomicType.FLOAT, rules)
      }
    } else if (min.isInstanceOf[AnyURIValue] && foundString) {
      min = Converter.convert(min, BuiltInAtomicType.STRING, rules)
    }
    min
  }

  class Min extends Minimax {

    def isMaxFunction(): Boolean = false

  }

  class Max extends Minimax {

    def isMaxFunction(): Boolean = true

  }

}

abstract class Minimax extends CollatingFunctionFixed {

  @BeanProperty
  var argumentType: PlainType = BuiltInAtomicType.ANY_ATOMIC

  @BooleanBeanProperty
  var ignoreNaN: Boolean = false

  def isMaxFunction(): Boolean

  def getComparer(): AtomicComparer = getPreAllocatedAtomicComparer

  override def supplyTypeInformation(visitor: ExpressionVisitor,
                                     contextItemType: ContextItemStaticInfo,
                                     arguments: Array[Expression]): Unit = {
    val `type`: ItemType = arguments(0).getItemType
    argumentType = `type`.getAtomizedItemType
    if (argumentType.isInstanceOf[AtomicType]) {
      if (argumentType == BuiltInAtomicType.UNTYPED_ATOMIC) {
        argumentType = BuiltInAtomicType.DOUBLE
      }
      preAllocateComparer(argumentType.asInstanceOf[AtomicType],
        argumentType.asInstanceOf[AtomicType],
        visitor.getStaticContext)
    }
  }

  override def getResultItemType(args: Array[Expression]): ItemType = {
    val th: TypeHierarchy =
      getRetainedStaticContext.getConfiguration.getTypeHierarchy
    var base: ItemType = Atomizer.getAtomizedItemType(args(0), false, th)
    if (base == BuiltInAtomicType.UNTYPED_ATOMIC) {
      base = BuiltInAtomicType.DOUBLE
    }
    base.getPrimitiveItemType
  }

  override def getCardinality(arguments: Array[Expression]): Int =
    if (!Cardinality.allowsZero(arguments(0).getCardinality)) {
      StaticProperty.EXACTLY_ONE
    } else {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    }

  override def makeOptimizedFunctionCall(visitor: ExpressionVisitor,
                                         contextInfo: ContextItemStaticInfo,
                                         arguments: Expression*): Expression = {
    val card: Int = arguments(0).getCardinality
    if (!Cardinality.allowsMany(card)) {
      val it: ItemType = arguments(0).getItemType.getPrimitiveItemType
      if (it.isInstanceOf[BuiltInAtomicType] &&
        it.asInstanceOf[BuiltInAtomicType].isOrdered(false)) {
        val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
        if (th.relationship(it, BuiltInAtomicType.UNTYPED_ATOMIC) !=
          Affinity.DISJOINT) {
          UntypedSequenceConverter
            .makeUntypedSequenceConverter(visitor.getConfiguration,
              arguments(0),
              BuiltInAtomicType.DOUBLE)
            .typeCheck(visitor, contextInfo)
        } else {
          arguments(0)
        }
      }
    }
    null
  }

  override def getAtomicComparer(context: XPathContext): AtomicComparer = {
    val comparer: AtomicComparer = getPreAllocatedAtomicComparer
    if (comparer != null) {
      comparer
    }
    var `type`: PlainType = argumentType.getPrimitiveItemType
    if (`type` == BuiltInAtomicType.UNTYPED_ATOMIC) {
      `type` = BuiltInAtomicType.DOUBLE
    }
    val prim: BuiltInAtomicType = `type`.asInstanceOf[BuiltInAtomicType]
    GenericAtomicComparer.makeAtomicComparer(prim,
      prim,
      getStringCollator,
      context)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[AtomicValue] =
    new ZeroOrOne(
      minimax(arguments(0).iterate(),
        isMaxFunction,
        getAtomicComparer(context),
        ignoreNaN,
        context))

  override def exportAttributes(out: ExpressionPresenter): Unit = {
    super.exportAttributes(out)
    if (ignoreNaN) {
      out.emitAttribute("flags", "i")
    }
  }

  override def importAttributes(attributes: Properties): Unit = {
    super.importAttributes(attributes)
    val flags: String = attributes.getProperty("flags")
    if (flags != null && flags.contains("i")) {
      this.ignoreNaN = true
    }
  }

  override def getStreamerName(): String = "Minimax"

}
