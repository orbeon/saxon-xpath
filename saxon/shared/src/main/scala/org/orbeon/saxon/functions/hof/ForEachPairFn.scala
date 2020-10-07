package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.{Expression, MappingFunction, MappingIterator, XPathContext}
import org.orbeon.saxon.functions.SystemFunction
import org.orbeon.saxon.model.AnyItemType
import org.orbeon.saxon.model.ItemType
import org.orbeon.saxon.model.SpecificFunctionType
import org.orbeon.saxon.om._
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.ExternalObject
import org.orbeon.saxon.value.ObjectValue
import ForEachPairFn._


object ForEachPairFn {

  private class PairedSequenceIterator(private var seq0: SequenceIterator,
                                       private var seq1: SequenceIterator)
    extends SequenceIterator {

    private var args: Array[Sequence] = new Array[Sequence](2)

    def next(): ObjectValue[Array[Sequence]] = {
      val i0: Item = seq0.next()
      if (i0 == null) {
        close()
        return null
      }
      val i1: Item = seq1.next()
      if (i1 == null) {
        close()
        return null
      }
      args(0) = i0
      args(1) = i1
      new ObjectValue(args)
    }

    override def close(): Unit = {
      seq0.close()
      seq1.close()
    }

  }

}

class ForEachPairFn extends SystemFunction {

  /**
   * Get the return type, given knowledge of the actual arguments
   *
   * @param args the actual arguments supplied
   * @return the best available item type that the function will return
   */
  override def getResultItemType(args: Array[Expression]): ItemType = {
    val fnType: ItemType = args(2).getItemType
    if (fnType.isInstanceOf[SpecificFunctionType]) {
      fnType.asInstanceOf[SpecificFunctionType].getResultType.getPrimaryType
    } else {
      AnyItemType
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    SequenceTool.toLazySequence(
      evalMapPairs(arguments(2).head.asInstanceOf[Function],
        arguments(0).iterate(),
        arguments(1).iterate(),
        context))

  private def evalMapPairs(function: Function,
                           seq0: SequenceIterator,
                           seq1: SequenceIterator,
                           context: XPathContext): SequenceIterator = {
    val pairs: PairedSequenceIterator = new PairedSequenceIterator(seq0, seq1)
    val map: MappingFunction = new MappingFunction {
      override def map(item: Item): SequenceIterator = {
        val pair = item.asInstanceOf[ExternalObject[_]].getObject.asInstanceOf[Array[Sequence]]
         SystemFunction.dynamicCall(function, context, pair).iterate
      }
    }
    new MappingIterator(pairs, map)
  }

}
