package net.sf.saxon.functions

import net.sf.saxon.expr._

import net.sf.saxon.expr.sort.DocumentOrderIterator

import net.sf.saxon.expr.sort.LocalOrderComparer

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.SingletonIterator

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Whitespace

import SuperId._

object SuperId {

  val ID: Int = 0

  val ELEMENT_WITH_ID: Int = 1

  def getIdSingle(doc: TreeInfo,
                  idrefs: String,
                  operation: Int): SequenceIterator =
    if (Whitespace.containsWhitespace(idrefs)) {
      val tokens: Whitespace.Tokenizer = new Whitespace.Tokenizer(idrefs)
      val map: IdMappingFunction = new IdMappingFunction()
      map.document = doc
      map.operation = operation
      val result: SequenceIterator = new MappingIterator(tokens, map)
      new DocumentOrderIterator(result, LocalOrderComparer.getInstance)
    } else {
      SingletonIterator.makeIterator(
        doc.selectID(idrefs, operation == ELEMENT_WITH_ID))
    }

  def getIdMultiple(doc: TreeInfo,
                    idrefs: SequenceIterator,
                    operation: Int): SequenceIterator = {
    val map: IdMappingFunction = new IdMappingFunction()
    map.document = doc
    map.operation = operation
    val result: SequenceIterator = new MappingIterator(idrefs, map)
    new DocumentOrderIterator(result, LocalOrderComparer.getInstance)
  }

  private class IdMappingFunction extends MappingFunction {

    var document: TreeInfo = _

    var operation: Int = _

    def map(item: Item): SequenceIterator = {
      val idrefs: String = Whitespace.trim(item.getStringValueCS)
      if (Whitespace.containsWhitespace(idrefs)) {
        val tokens: Whitespace.Tokenizer = new Whitespace.Tokenizer(idrefs)
        val submap: IdMappingFunction = new IdMappingFunction()
        submap.document = document
        submap.operation = operation
        new MappingIterator(tokens, submap)
      } else {
        SingletonIterator.makeIterator(
          document.selectID(idrefs, operation == ELEMENT_WITH_ID))
      }
    }

  }

  class Id extends SuperId {

    override def getOp(): Int = ID

  }

  class ElementWithId extends SuperId {

    override def getOp(): Int = ELEMENT_WITH_ID

  }

}

abstract class SuperId extends SystemFunction {

  def getOp(): Int

  override def getSpecialProperties(arguments: Array[Expression]): Int = {
    var prop: Int = StaticProperty.ORDERED_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET |
      StaticProperty.NO_NODES_NEWLY_CREATED
    if ((getArity == 1) ||
      (arguments(1).getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) !=
        0) {
      prop |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    prop
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val start: NodeInfo =
      if (arguments.length == 1) getContextNode(context)
      else arguments(1).head().asInstanceOf[NodeInfo]
    val arg1: NodeInfo = start.getRoot
    if (arg1.getNodeKind != Type.DOCUMENT) {
      throw new XPathException(
        "In the " + getFunctionName.getLocalPart + "() function," +
          " the tree being searched must be one whose root is a document node",
        "FODC0001",
        context)
    }
    val doc: TreeInfo = arg1.getTreeInfo
    var result: SequenceIterator = null
    if (arguments(0).isInstanceOf[AtomicValue]) {
      result = getIdSingle(
        doc,
        arguments(0).asInstanceOf[AtomicValue].getStringValue,
        getOp)
    } else {
      val idrefs: SequenceIterator = arguments(0).iterate()
      result = getIdMultiple(doc, idrefs, getOp)
    }
    SequenceTool.toLazySequence(result)
  }

}
