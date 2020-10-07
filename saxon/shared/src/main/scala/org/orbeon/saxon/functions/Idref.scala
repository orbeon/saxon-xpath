package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.sort.DocumentOrderIterator

import org.orbeon.saxon.expr.sort.LocalOrderComparer

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om._

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.StringValue

import Idref._

object Idref {

  def getIdrefMultiple(doc: TreeInfo,
                       keys: SequenceIterator,
                       context: XPathContext): SequenceIterator = {
    val map: IdrefMappingFunction = new IdrefMappingFunction()
    map.document = doc
    map.keyContext = context
    /*  map.keyManager =
        context.getController.getExecutable.getTopLevelPackage.getKeyManager
      map.keySet = map.keyManager.getKeyDefinitionSet(  //getKeyDefinitionSet,getKeyManager not exist
        StandardNames.getStructuredQName(StandardNames.XS_IDREFS))*/
    val allValues: SequenceIterator = new MappingIterator(keys, map)
    new DocumentOrderIterator(allValues, LocalOrderComparer.getInstance)
  }

  private class IdrefMappingFunction extends MappingFunction {

    var document: TreeInfo = _

    var keyContext: XPathContext = _

    /*  var keyManager: KeyManager = _  // KeyManager not exist

      var keySet: KeyDefinitionSet = _ // KeyDefinitionSet not exist*/

    def map(item: Item): SequenceIterator = null

  }

}

class Idref extends SystemFunction {

  override def getSpecialProperties(arguments: Array[Expression]): Int = {
    var prop: Int = StaticProperty.ORDERED_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET |
      StaticProperty.NO_NODES_NEWLY_CREATED
    if ((getArity == 1) ||
      arguments(1).hasSpecialProperty(
        StaticProperty.CONTEXT_DOCUMENT_NODESET)) {
      prop |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    prop
  }

  def call(context: XPathContext,
           arguments: Array[Sequence]): ZeroOrMore[NodeInfo] = {
    val start: NodeInfo =
      if (arguments.length == 1) getContextNode(context)
      else arguments(1).head.asInstanceOf[NodeInfo]
    val arg2: NodeInfo = start.getRoot
    if (arg2.getNodeKind != Type.DOCUMENT) {
      throw new XPathException(
        "In the idref() function," +
          " the tree being searched must be one whose root is a document node",
        "FODC0001",
        context)
    }
    new ZeroOrMore(
      getIdrefMultiple(arg2.getTreeInfo, arguments(0).iterate(), context))
  }

}
