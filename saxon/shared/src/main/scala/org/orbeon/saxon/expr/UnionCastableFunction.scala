package org.orbeon.saxon.expr

import org.orbeon.saxon.model.FunctionItemType

import org.orbeon.saxon.model.SpecificFunctionType

import org.orbeon.saxon.model.UnionType

import org.orbeon.saxon.om._

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.SequenceType

class UnionCastableFunction(targetType: UnionType,
                            resolver: NamespaceResolver,
                            allowEmpty: Boolean)
  extends UnionConstructorFunction(targetType, resolver, allowEmpty) {

  override def getFunctionItemType: FunctionItemType =
    new SpecificFunctionType(Array(SequenceType.ANY_SEQUENCE),
      SequenceType.SINGLE_BOOLEAN)

  override def getFunctionName: StructuredQName = null

  private def effectiveBooleanValue(iter: SequenceIterator,
                                    context: XPathContext): Boolean = {
    var count: Int = 0
    var item: Item = null
    while (({
      item = iter.next()
      item
    }) != null) if (item.isInstanceOf[NodeInfo]) {
      val atomizedValue: AtomicSequence = item.atomize()
      val length: Int = SequenceTool.getLength(atomizedValue)
      count += length
      if (count > 1) {
        return false
      }
      if (length != 0) {
        val av: AtomicValue = atomizedValue.head
        if (!castable(av, context)) {
          return false
        }
      }
    } else if (item.isInstanceOf[AtomicValue]) {
      val av: AtomicValue = item.asInstanceOf[AtomicValue]
        count += 1
      if (count > 1) {
        return false
      }
      if (!castable(av, context)) {
        false
      }
    } else {
      throw new XPathException(
        "Input to 'castable' operator cannot be atomized",
        "XPTY0004")
    }
    count != 0 || allowEmpty
  }

  private def castable(value: AtomicValue, context: XPathContext): Boolean =
    try {
      cast(value, context)
      true
    } catch {
      case err: XPathException => false

    }

  override def call(context: XPathContext, args: Array[Sequence]): BooleanValue = {
    val value: Boolean = effectiveBooleanValue(args(0).iterate(), context)
    BooleanValue.get(value)
  }

}
