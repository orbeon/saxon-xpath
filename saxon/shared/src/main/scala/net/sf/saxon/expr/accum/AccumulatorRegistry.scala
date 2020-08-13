package net.sf.saxon.expr.accum

import java.util._

import net.sf.saxon.om.StructuredQName
import net.sf.saxon.value.Whitespace

import scala.jdk.CollectionConverters._

class AccumulatorRegistry {

   var accumulatorsByName: Map[StructuredQName, Accumulator] =
    new HashMap[StructuredQName, Accumulator]()

  def getUsedAccumulators(useAccumulatorsAtt: String
                          /*styleElement: StyleElement*/): Set[Accumulator] = {  // need to create StyleElement
    val accumulators: Set[Accumulator] = new HashSet[Accumulator]()
    val attNames: String = Whitespace.trim(useAccumulatorsAtt)
    val tokens: Array[String] = attNames.split("[ \t\r\n]+")
    if (tokens.length == 1 && tokens(0).==("#all")) {
      for (acc <- getAllAccumulators.asScala) {
        accumulators.add(acc)
      }
    } else if (tokens.length == 1 && tokens(0).isEmpty) {} else {
      /*val names: List[StructuredQName] =
        new ArrayList[StructuredQName](tokens.length)*/
     /* for (token <- tokens) {
        if (token.==("#all")) {
          /*styleElement.compileErrorInAttribute(
            "If use-accumulators contains the token '#all', it must be the only token",
            "XTSE3300",
            "use-accumulators")*/
          //break
        }
       /* val name: StructuredQName =
          styleElement.makeQName(token, "XTSE3300", "use-accumulators")*/
        if (names.contains(name)) {
          styleElement.compileErrorInAttribute(
            "Duplicate QName in use-accumulators attribute: " + token,
            "XTSE3300",
            "use-accumuators")
          //break
        }
        val acc: Accumulator = getAccumulator(name)
        if (acc == null) {
          styleElement.compileErrorInAttribute(
            "Unknown accumulator name: " + token,
            "XTSE3300",
            "use-accumulators")
          //break
        }
        names.add(name)
        accumulators.add(acc)
      }*/
    }
    accumulators
  }

  def addAccumulator(acc: Accumulator): Unit = {
    if (acc.getAccumulatorName != null) {
      accumulatorsByName.put(acc.getAccumulatorName, acc)
    }
  }

  def getAccumulator(name: StructuredQName): Accumulator =
    accumulatorsByName.get(name)

  def getAllAccumulators(): java.lang.Iterable[Accumulator] =
    accumulatorsByName.values

//  def getStreamingAccumulatorValue(node: NodeInfo,
//                                   accumulator: Accumulator,
//                                   phase: AccumulatorFn.Phase.Phase): Sequence = null

}
