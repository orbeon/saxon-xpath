package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.utils.Controller

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om._

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.QNameValue

import java.util.HashMap

import MemoFunction._

object MemoFunction {

  private def getCombinedKey(params: Array[Sequence]): String = {
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
    for (seq <- params) {
      val iter: SequenceIterator = seq.iterate()
      var item: Item = null
      while (({
        item = iter.next()
        item
      }) != null) {
        if (item.isInstanceOf[NodeInfo]) {
          val node: NodeInfo = item.asInstanceOf[NodeInfo]
          node.generateId(sb)
        } else if (item.isInstanceOf[QNameValue]) {
          sb.cat(Type.displayTypeName(item))
            .cat('/')
            .cat(item.asInstanceOf[QNameValue].getClarkName)
        } else if (item.isInstanceOf[AtomicValue]) {
          sb.cat(Type.displayTypeName(item))
            .cat('/')
            .cat(item.getStringValueCS)
        } else if (item.isInstanceOf[Function]) {
          sb.cat(item.getClass.getName)
            .cat("@")
            .cat("" + System.identityHashCode(item))
        }
        sb.cat('')
      }
      sb.cat('')
    }
    sb.toString
  }

}

class MemoFunction extends UserFunction {

  override def computeEvaluationMode(): Unit = {
    evaluator = ExpressionTool.eagerEvaluator(getBody)
  }

  override def isMemoFunction(): Boolean = true

  override def call(context: XPathContext,
                    actualArgs: Array[Sequence]): Sequence = {
    val key: String = getCombinedKey(actualArgs)
    val controller: Controller = context.getController
    var map: HashMap[String, Sequence] = controller
      .getUserData(this, "memo-function-cache")
      .asInstanceOf[HashMap[String, Sequence]]
    var value: Sequence = if (map == null) null else map.get(key)
    if (value != null) {
      return value
    }
    value = super.call(context, actualArgs)
    if (map == null) {
      map = new HashMap(32)
      controller.setUserData(this, "memo-function-cache", map)
    }
    map.put(key, value)
    value
  }

}
