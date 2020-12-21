package org.orbeon.saxon.functions

import org.orbeon.saxon.expr._

import org.orbeon.saxon.expr.parser.PathMap

import org.orbeon.saxon.expr.parser.RetainedStaticContext

import org.orbeon.saxon.expr.sort.LocalOrderComparer

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om._

import org.orbeon.saxon.pattern.NodeSetPattern

import org.orbeon.saxon.pattern.Pattern

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.EmptyIterator

import org.orbeon.saxon.tree.util.Navigator

import org.orbeon.saxon.value.AtomicValue

import KeyFn._

// all errors are due // class not found KeyDefinition, KeyDefinitionSet,KeyManager
object KeyFn {

  def internalKeyCall(/*keyManager: KeyManager,  // class not found KeyDefinition, KeyDefinitionSet,KeyManager
                      keySet: KeyDefinitionSet,*/
                      name: String,
                      value: Expression,
                      doc: Expression,
                      rsc: RetainedStaticContext): Expression = {
    val fn: KeyFn =
      SystemFunction.makeFunction("key", rsc, 3).asInstanceOf[KeyFn]
    assert(fn != null)
   /* fn.staticKeySet = keySet*/
    try fn.fixArguments(new StringLiteral(name), value, doc)
    catch {
      case e: XPathException => {}

    }
    fn.makeFunctionCall(new StringLiteral(name), value, doc)
  }

  class SubtreeFilter(private var origin: NodeInfo)
    extends ItemMappingFunction {

    def mapItem(item: Item): NodeInfo =
      if (Navigator.isAncestorOrSelf(origin, item.asInstanceOf[NodeInfo])) {
        item.asInstanceOf[NodeInfo]
      } else {
        null
      }

  }

  private def getContextRoot(context: XPathContext): NodeInfo = {
    val contextItem: Item = context.getContextItem
    if (contextItem == null) {
      throw new XPathException(
        "Cannot call the key() function when there is no context item",
        "XTDE1270",
        context)
    } else if (!(contextItem.isInstanceOf[NodeInfo])) {
      throw new XPathException(
        "Cannot call the key() function when the context item is not a node",
        "XTDE1270",
        context)
    }
    contextItem.asInstanceOf[NodeInfo].getRoot
  }

  private def getOrigin(context: XPathContext, argument2: Sequence): Item = {
    var arg2: Item = null
    try arg2 = argument2.head
    catch {
      case e: XPathException => {
        val code: String = e.getErrorCodeLocalPart
        if ("XPDY0002" == code && argument2.isInstanceOf[RootExpression]) {
          throw new XPathException(
            "Cannot call the key() function when there is no context node",
            "XTDE1270",
            context)
        } else if ("XPDY0050" == code) {
          throw new XPathException(
            "In the key() function," +
              " the node supplied in the third argument (or the context node if absent)" +
              " must be in a tree whose root is a document node",
            "XTDE1270",
            context
          )
        } else if ("XPTY0020" == code || "XPTY0019" == code) {
          throw new XPathException(
            "Cannot call the key() function when the context item is an atomic value",
            "XTDE1270",
            context)
        }
        throw e
      }

    }
    arg2
  }

  def search(/*keyManager: KeyManager,*/
             context: XPathContext,
             sought: Sequence,
             origin: NodeInfo /*,
                       selectedKeySet: KeyDefinitionSet*/): Sequence = {
    val doc: NodeInfo = origin.getRoot
    /*    if (selectedKeySet.isComposite) {
          val soughtKey: SequenceIterator = sought.iterate()
          val all: SequenceIterator = keyManager.selectByCompositeKey(
            selectedKeySet,
            doc.getTreeInfo,
            soughtKey,
            context)
          if (origin == doc) {
            new LazySequence(all)
          }
          new LazySequence(new ItemMappingIterator(all, new SubtreeFilter(origin)))
        } else {*/
    var allResults: SequenceIterator = null
    val keys: SequenceIterator = sought.iterate()
    var keyValue: AtomicValue = null
    while (({
      keyValue = keys.next().asInstanceOf[AtomicValue]
      keyValue
    }) != null) {
      /* val someResults: SequenceIterator = keyManager.selectByKey(
         selectedKeySet,
         doc.getTreeInfo,
         keyValue,
         context)*/
      /*allResults =
        if (allResults == null) null*/
      new UnionEnumeration(allResults,
        null,
        LocalOrderComparer.getInstance)
    }
    if (allResults == null) {
      allResults = EmptyIterator.ofNodes
    }
    if (origin == doc) {
      new LazySequence(allResults)
    }
    new LazySequence(
      new ItemMappingIterator(allResults, new SubtreeFilter(origin)))
  }

}

class KeyFn extends SystemFunction with StatefulSystemFunction {

  def getNamespaceResolver: NamespaceResolver = getRetainedStaticContext

  override def getSpecialProperties(arguments: Array[Expression]): Int = {
    var prop: Int = StaticProperty.ORDERED_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET |
      StaticProperty.NO_NODES_NEWLY_CREATED
    if ((getArity == 2) ||
      (arguments(2).getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) !=
        0) {
      prop |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    prop
  }

  override def copy(): SystemFunction = {
    val k2: KeyFn = SystemFunction
      .makeFunction(getFunctionName.getLocalPart,
        getRetainedStaticContext,
        getArity)
      .asInstanceOf[KeyFn]
    /*k2.staticKeySet = staticKeySet*/
    k2
  }

  override def fixArguments(arguments: Expression*): Expression = {
    arguments(0) match {
      case literal: StringLiteral =>
        val keyName: String = literal.getStringValue
      case _ =>
    }
    null
  }

  def addToPathMap(pathMap: PathMap,
                   pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet =
  /*  if (staticKeySet != null) {
      val result: PathMap.PathMapNodeSet = new PathMap.PathMapNodeSet()
      for (kd <- staticKeySet.getKeyDefinitions) {
        val pat: Pattern = kd.getMatch
        if (pat.isInstanceOf[NodeSetPattern]) {
          val selector: Expression =
            pat.asInstanceOf[NodeSetPattern].getSelectionExpression
          val selected: PathMap.PathMapNodeSet =
            selector.addToPathMap(pathMap, pathMapNodeSet)
          val use: Expression = kd.getUse
          val used: PathMap.PathMapNodeSet =
            use.addToPathMap(pathMap, selected)
          result.addNodeSet(selected)
        } else {
          throw new IllegalStateException("Can't add key() call to pathmap")
        }
      }
      result
    } else {*/
    throw new IllegalStateException(
      "Can't add dynamic key() call to pathmap")

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    var origin: NodeInfo = null
    origin =
      if (arguments.length == 3)
        getOrigin(context, arguments(2)).asInstanceOf[NodeInfo]
      else getContextRoot(context)
    if (origin.getRoot.getNodeKind != Type.DOCUMENT) {
      throw new XPathException(
        "In the key() function," +
          " the node supplied in the third argument (or the context node if absent)" +
          " must be in a tree whose root is a document node",
        "XTDE1270",
        context
      )
    }
    null
  }

  /* private def getKeyDefinitionSet(keyManager: KeyManager,
                                   keyName: String): KeyDefinitionSet = {
     var selectedKeySet: KeyDefinitionSet = null
     var qName: StructuredQName = null
     qName = StructuredQName.fromLexicalQName(keyName,
       false,
       true,
       getNamespaceResolver)
     selectedKeySet = keyManager.getKeyDefinitionSet(qName)
     if (selectedKeySet == null) {
       throw new XPathException("Key '" + keyName + "' has not been defined",
         "XTDE1260")
     }
     selectedKeySet
   }*/

}
