package net.sf.saxon.expr

import net.sf.saxon.model.FunctionItemType

import net.sf.saxon.model.SpecificFunctionType

import net.sf.saxon.model.UnionType

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.SequenceType

class UnionCastableFunction(targetType: UnionType,
                            resolver: NamespaceResolver,
                            allowEmpty: Boolean)
  extends UnionConstructorFunction(targetType, resolver, allowEmpty) {

  override def getFunctionItemType(): FunctionItemType =
    new SpecificFunctionType(Array(SequenceType.ANY_SEQUENCE),
      SequenceType.SINGLE_BOOLEAN)

  override def getFunctionName(): StructuredQName = null

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
        val av: AtomicValue = atomizedValue.head()
        if (!castable(av, context)) {
          false
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
