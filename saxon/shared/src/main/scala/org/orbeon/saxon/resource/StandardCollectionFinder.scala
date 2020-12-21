////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.resource

import java.net.{URI, URISyntaxException}
import java.util.{HashMap, Map}

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.{ResolveURI, URIQueryParameters}
import org.orbeon.saxon.lib.{CollectionFinder, ResourceCollection}
import org.orbeon.saxon.resource.StandardCollectionFinder._
import org.orbeon.saxon.trans.{Err, XPathException}



/**
 * Default implementation of the CollectionFinder interface. The standard CollectionFinder recognizes four
 * types of collection:
 * <p>
 * <ol>
 * <li>Any URI may be explicitly registered and associated with an instance of {@link ResourceCollection}</li>
 * <li>If the file: URI scheme is used, and the relevant file identifies a directory, the directory
 * is treated as a collection: it is returned as an instance of {@link DirectoryCollection}</li>
 * <li>If the URI ends with ".jar" or ".zip", or more generally, if the method {@link #isJarFileURI(String)} returns
 * true, the URI is treated as identifying a JAR or ZIP archive, whose contents form the
 * resources in the collection: it is returned as an instance of {@link JarCollection}</li>
 * <li>In all other cases, the URI is treated as the URI of an XML document listing the URIs
 * of the resources in the collection, which are then retrieved using the {@link javax.xml.transform.URIResolver}</li>
 * </ol>
 */
object StandardCollectionFinder {

  def checkNotNull(collectionURI: String, context: XPathContext): Unit = {
    if (collectionURI == null) {
      val err = new XPathException(
        "No default collection has been defined")
      err.setErrorCode("FODC0002")
      err.setXPathContext(context)
      throw err
    }
  }

//  def checkFileExists(file: File,
//                      resolvedURI: URI,
//                      context: XPathContext): Unit = {
//    if (!file.exists()) {
//      val err = new XPathException(
//        "The file or directory " + resolvedURI + " does not exist")
//      err.setErrorCode("FODC0002")
//      err.setXPathContext(context)
//      throw err
//    }
//  }
}

class StandardCollectionFinder extends CollectionFinder {

  private var registeredCollections: Map[String, ResourceCollection] = new HashMap(2)

  def registerCollection(collectionURI: String,
                         collection: ResourceCollection): Unit = {
    registeredCollections.put(collectionURI, collection)
  }

  def findCollection(context: XPathContext,
                     collectionURI: String): ResourceCollection = {
    var collectionURI1 = collectionURI
    checkNotNull(collectionURI, context)

    val registeredCollection = registeredCollections.get(collectionURI)
    if (registeredCollection != null)
      return registeredCollection

    var params: URIQueryParameters = null
    try {
      val relativeURI = new URI(ResolveURI.escapeSpaces(collectionURI))
      val query = relativeURI.getQuery
      if (query != null) {
        val q = collectionURI.indexOf('?')
        params = new URIQueryParameters(query, context.getConfiguration)
        collectionURI1 = ResolveURI.escapeSpaces(collectionURI.substring(0, q))
      }
    } catch {
      case _: URISyntaxException => {
        val err = new XPathException(
          "Invalid relative URI " + Err.wrap(collectionURI1, Err.VALUE) +
            " passed to collection() function")
        err.setErrorCode("FODC0004")
        err.setXPathContext(context)
        throw err
      }

    }
    var resolvedURI: URI = null
    resolvedURI = new URI(collectionURI1)
    if (! context.getConfiguration.getAllowedUriTest.test(resolvedURI)) {
      throw new XPathException(
        "URI scheme '" + resolvedURI.getScheme + "' has been disallowed")
    }
//    if ("file" == resolvedURI.getScheme) {
//      val file: File = new File(resolvedURI)
//      checkFileExists(file, resolvedURI, context)
//      if (file.isDirectory) {
//        return new DirectoryCollection(context.getConfiguration,
//          collectionURI1,
//          file,
//          params)
//      }
//    }
//    if (isJarFileURI(collectionURI1)) {
//      return new JarCollection(context, collectionURI1, params)
//    }
    new CatalogCollection(context.getConfiguration, collectionURI1)
  }

  // check if file is a zip file
  // otherwise assume the URI identifies a collection catalog
  // check if file is a zip file
  // otherwise assume the URI identifies a collection catalog

  def isJarFileURI(collectionURI: String): Boolean =
    collectionURI.endsWith(".jar") || collectionURI.endsWith(".zip") ||
      collectionURI.startsWith("jar:")
}
