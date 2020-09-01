package net.sf.saxon.functions.hof

import net.sf.saxon.expr.{Expression, MappingFunction, MappingIterator, XPathContext}
import net.sf.saxon.functions.SystemFunction
import net.sf.saxon.model.AnyItemType
import net.sf.saxon.model.ItemType
import net.sf.saxon.model.SpecificFunctionType
import net.sf.saxon.om._
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.ExternalObject
import net.sf.saxon.value.ObjectValue
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
