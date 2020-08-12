////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.resource

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.functions.DocumentFn
import net.sf.saxon.functions.URIQueryParameters
import net.sf.saxon.lib.ParseOptions
import net.sf.saxon.lib.Resource
import net.sf.saxon.lib.Validation
import net.sf.saxon.om.AxisInfo
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.SpaceStrippingRule
import net.sf.saxon.om.TreeInfo
import net.sf.saxon.pattern.NodeKindTest
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AxisIterator
import net.sf.saxon.tree.jiter.MappingJavaIterator
import javax.xml.transform.Source
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.net.URI
import java.net.URISyntaxException
import java.nio.charset.Charset
import java.util.ArrayList
import java.util.Iterator
import java.util.List

import CatalogCollection._
import net.sf.saxon.resource.AbstractResourceCollection.InputDetails




object CatalogCollection {

  def makeStringBuilderFromStream(in: InputStream,
                                  encoding: String): StringBuilder = {
    val is: InputStreamReader =
      new InputStreamReader(in, Charset.forName(encoding))
    val sb: StringBuilder = new StringBuilder()
    val br: BufferedReader = new BufferedReader(is)
    var read: String = br.readLine()
    while (read != null) {
      sb.append(read)
      read = br.readLine()
    }
    br.close()
    sb
  }

}

class CatalogCollection //TODO we might know the catalog File already
(config: Configuration, collectionURI: String)
    extends AbstractResourceCollection(config) {

  private var stable: Boolean = _

  private var whitespaceRules: SpaceStrippingRule = _

  this.setCollectionURI(collectionURI) 

  def getResourceURIs(context: XPathContext): Iterator[String] = {
    StandardCollectionFinder.checkNotNull(collectionURI, context)
    catalogContents(collectionURI, context)
  }

  def getResources(context: XPathContext): Iterator[Resource] = {
    StandardCollectionFinder.checkNotNull(collectionURI, context)
    val resourceURIs: Iterator[String] = getResourceURIs(context)
    new MappingJavaIterator(
      resourceURIs,
      (in:String) =>
        try if (in.startsWith("data:")) {
          val basicResource: Resource = DataURIScheme.decode(new URI(in))
          makeTypedResource(context.getConfiguration, basicResource)
        } else {
          val id: InputDetails = getInputDetails(in)
          id.parseOptions =
            new ParseOptions(context.getConfiguration.getParseOptions)
          id.parseOptions.setSpaceStrippingRule(whitespaceRules)
          id.resourceUri = in
          makeResource(context.getConfiguration, id)
        } catch {
          case e: XPathException => {
            val onError: Int =
              if (params == null) URIQueryParameters.ON_ERROR_FAIL
              else params.getOnError
            if (onError == URIQueryParameters.ON_ERROR_FAIL) {
              new FailedResource(in, e)
            } else if (onError == URIQueryParameters.ON_ERROR_WARNING) {
              context.getController.warning(
                "collection(): failed to parse " + in + ": " + e.getMessage,
                e.getErrorCodeLocalPart,
                null)
              null
            } else {
              null
            }
          }

      }
    )
  }

  override def isStable(context: XPathContext): Boolean = stable

   def catalogContents(href: String,
                                context: XPathContext): Iterator[String] = {
    val source: Source = DocumentFn.resolveURI(href, null, null, context)
    val options: ParseOptions = new ParseOptions()
    options.setSchemaValidationMode(Validation.SKIP)
    options.setDTDValidationMode(Validation.SKIP)
    val catalog: TreeInfo =
      context.getConfiguration.buildDocumentTree(source, options)
    if (catalog == null) {
// we failed to read the catalogue
      val err: XPathException = new XPathException(
        "Failed to load collection catalog " + href)
      err.setErrorCode("FODC0004")
      err.setXPathContext(context)
      throw err
    }
    val iter: AxisIterator =
      catalog.getRootNode.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
    val top: NodeInfo = iter.next()
    if (top == null ||
        !("collection" == top.getLocalPart && top.getURI.isEmpty)) {
      var message: String = null
      message =
        if (top == null) "No outermost element found in collection catalog"
        else
          "Outermost element of collection catalog should be Q{}collection " +
            "(found Q{" +
            top.getURI +
            "}" +
            top.getLocalPart +
            ")"
      val err: XPathException = new XPathException(message)
      err.setErrorCode("FODC0004")
      err.setXPathContext(context)
      throw err
    }
    iter.close()
    val stableAtt: String = top.getAttributeValue("", "stable")
    if (stableAtt != null) {
      if ("true" == stableAtt) {
        stable = true
      } else if ("false" == stableAtt) {
        stable = false
      } else {
        val err: XPathException = new XPathException(
          "The 'stable' attribute of element <collection> must be true or false")
        err.setErrorCode("FODC0004")
        err.setXPathContext(context)
        throw err
      }
    }
    val documents: AxisIterator =
      top.iterateAxis(AxisInfo.CHILD, NodeKindTest.ELEMENT)
    val result: List[String] = new ArrayList[String]()
    var item: NodeInfo = null
    while ((item = documents.next()) != null) {
      if (!("doc" == item.getLocalPart && item.getURI.isEmpty)) {
        val err: XPathException = new XPathException(
          "Children of <collection> element must be <doc> elements")
        err.setErrorCode("FODC0004")
        err.setXPathContext(context)
        throw err
      }
      val hrefAtt: String = item.getAttributeValue("", "href")
      if (hrefAtt == null) {
        val err: XPathException = new XPathException(
          "A <doc> element in the collection catalog has no @href attribute")
        err.setErrorCode("FODC0004")
        err.setXPathContext(context)
        throw err
      }
      var uri: String = null
      try uri = new URI(item.getBaseURI).resolve(hrefAtt).toString
      catch {
        case e: URISyntaxException => {
          val err: XPathException = new XPathException(
            "Invalid base URI or href URI in collection catalog: (" +
              item.getBaseURI +
              ", " +
              hrefAtt +
              ")")
          err.setErrorCode("FODC0004")
          err.setXPathContext(context)
          throw err
        }

      }
      result.add(uri)
    }
    result.iterator()
  }
// Now return an iterator over the documents that it refers to
// Now return an iterator over the documents that it refers to

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
