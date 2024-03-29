////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.functions

import java.io.Closeable
import java.net.{URI, URISyntaxException}
import java.util.{ArrayList, Iterator}

import org.orbeon.saxon.expr._
import org.orbeon.saxon.functions.CollectionFn._
import org.orbeon.saxon.lib.{CollectionFinder, Feature, Resource, ResourceCollection}
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om._
import org.orbeon.saxon.resource.AbstractResourceCollection
import org.orbeon.saxon.style.StylesheetPackage
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.wrapper.SpaceStrippedDocument
import org.orbeon.saxon.utils.Controller
import org.orbeon.saxon.value.{ExternalObject, ObjectValue}


object CollectionFn {

  /**
    * URI representing a collection that is always empty, regardless of any collection URI resolver
    */
  /**
   * URI representing a collection that is always empty, regardless of any collection URI resolver
   */
  val EMPTY_COLLECTION_URI: String = "http://saxon.sf.net/collection/empty"

  val EMPTY_COLLECTION: ResourceCollection = new EmptyCollection(
    EMPTY_COLLECTION_URI)

  private class EmptyCollection(private var collectionUri: String)
      extends ResourceCollection {

    def getCollectionURI: String = collectionUri

    def getResourceURIs(context: XPathContext): Iterator[String] =
      new ArrayList[String]().iterator

    def getResources(context: XPathContext): Iterator[Resource] =
      new ArrayList[Resource]().iterator

    def isStable(context: XPathContext): Boolean = true
    def stripWhitespace(rules: SpaceStrippingRule): Boolean = false
  }

}

class CollectionFn extends SystemFunction {

 override def getSpecialProperties(arguments: Array[Expression]): Int = // they will all be "new" documents. We can't even assume that they will be distinct.
    (super.getSpecialProperties(arguments) & ~StaticProperty.NO_NODES_NEWLY_CREATED) |
      StaticProperty.PEER_NODESET

  private def getAbsoluteCollectionURI(href: String,
                                       context: XPathContext): String = {
    var hrefStr = href
    var absoluteURI: String = null
    if (hrefStr == null) {
      absoluteURI = context.getConfiguration.getDefaultCollection
    } else {
      var uri: URI = null
      try uri = new URI(hrefStr)
      catch {
        case _: URISyntaxException =>
          hrefStr = IriToUri.iriToUri(hrefStr).toString
          uri = new URI(hrefStr)
      }
      if (uri.isAbsolute) {
        absoluteURI = uri.toString
      } else {
        val base: String = getRetainedStaticContext.getStaticBaseUriString
        if (base != null) {
          absoluteURI = new URI(base).resolve(hrefStr).toString
        } else {
          throw new XPathException(
            "Relative collection URI cannot be resolved: no base URI available",
            "FODC0002")
        }
      }
    }
    absoluteURI
  }

  private def getSequenceIterator(collection: ResourceCollection,
                                  context: XPathContext): SequenceIterator = {
    val sources: Iterator[_ <: Resource] = collection.getResources(context)
    new SequenceIterator {
      def next(): Item =
        if (sources.hasNext) {
          new ObjectValue[Resource](sources.next())
        } else {
          null
        }

     override def close(): Unit = {
        sources match {
          case closeable: Closeable =>
            closeable.close()
          case _ =>
        }
      }
    }
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    var href: String = null
    if (getArity == 0) {
// No arguments supplied: this gets the default collection
      href = context.getController.getDefaultCollection
    } else {
      val arg: Item = arguments(0).head
      href =
        if (arg == null) context.getController.getDefaultCollection
        else arg.getStringValue
    }
    if (href == null) {
      throw new XPathException("No default collection has been defined",
                               "FODC0002")
    }
    val absoluteURI: String = getAbsoluteCollectionURI(href, context)
    val packageData: PackageData = getRetainedStaticContext.getPackageData
    var whitespaceRule: SpaceStrippingRule =
      NoElementsSpaceStrippingRule.getInstance
    var collectionKey: String = absoluteURI
    if (packageData.isXSLT) {
      whitespaceRule =
        packageData.asInstanceOf[StylesheetPackage].getSpaceStrippingRule
      if (whitespaceRule != NoElementsSpaceStrippingRule.getInstance) {
        collectionKey = packageData
            .asInstanceOf[StylesheetPackage]
            .getPackageName+
            " " +
            absoluteURI // packageData.asInstanceOf[StylesheetPackage].getPackageVersion*/
      }
    }
    var cachedCollection: GroundedValue = context.getController
      .getUserData("saxon:collections", collectionKey)
      .asInstanceOf[GroundedValue]
    if (cachedCollection != null)
      return cachedCollection
    val collectionFinder: CollectionFinder =
      context.getController.getCollectionFinder
    var collection: ResourceCollection =
      collectionFinder.findCollection(context, absoluteURI)
    if (collection == null) {
      collection = new EmptyCollection(EMPTY_COLLECTION_URI)
    }
    if (packageData.isInstanceOf[StylesheetPackage] &&
        whitespaceRule != NoElementsSpaceStrippingRule.getInstance) {
      if (collection.isInstanceOf[AbstractResourceCollection]) {
        val alreadyStripped: Boolean =
          collection.stripWhitespace(whitespaceRule)
        if (alreadyStripped) {
          whitespaceRule = null
        }
      }
    }
// Get an iterator over the resources in the collection
    val sourceSeq: SequenceIterator = getSequenceIterator(collection, context)
// Get an iterator over the items representing the resources
    var result: SequenceIterator =
      context.getConfiguration.getMultithreadedItemMappingIterator(
        sourceSeq,
        item1 =>
          item1
            .asInstanceOf[ExternalObject[Resource]]
            .getObject
            .getItem(context))
// In XSLT, apply space-stripping to document nodes in the collection
    if (whitespaceRule != null) {
      val rule: SpaceStrippingRule = whitespaceRule
      val stripper: ItemMappingFunction = item => {
        item match {
          case info: NodeInfo if info.getNodeKind == Type.DOCUMENT =>
            val treeInfo = info.getTreeInfo
            if (treeInfo.getSpaceStrippingRule != rule)
              new SpaceStrippedDocument(treeInfo, rule).getRootNode
          case _ =>
        }
        item
      }
      result = new ItemMappingIterator(result, stripper)
    }
// If the collection is stable, cache the result
    if (collection.isStable(context) || context.getConfiguration.getBooleanProperty(Feature.STABLE_COLLECTION_URI)) {
      val controller: Controller = context.getController
      val docPool: DocumentPool = controller.getDocumentPool
      cachedCollection = result.materialize
      val iter: SequenceIterator = cachedCollection.iterate()
      var item: Item = null
      while ({
        item = iter.next()
        item
      } != null) item match {
        case nodeInfo: NodeInfo if nodeInfo.getNodeKind == Type.DOCUMENT =>
          val docUri = nodeInfo.getSystemId
          val docKey = new DocumentURI(docUri)
          val info =
            item match {
              case treeInfo: TreeInfo => treeInfo
              case _                  => new GenericTreeInfo(controller.getConfiguration, nodeInfo)
            }
          docPool.add(info, docKey)
        case _ =>
      }
      context.getController.setUserData("saxon:collections", collectionKey, cachedCollection)
      return cachedCollection
    }
    new LazySequence(result)
  }
// See if the collection has been cached
// Call the user-supplied CollectionFinder to get the ResourceCollection
// In XSLT, worry about whitespace stripping
// See if the collection has been cached
// Call the user-supplied CollectionFinder to get the ResourceCollection
// In XSLT, worry about whitespace stripping
}
