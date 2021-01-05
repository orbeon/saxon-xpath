//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//package org.orbeon.saxon.resource
//
//import org.orbeon.saxon.utils.Configuration
//import org.orbeon.saxon.expr.XPathContext
//import org.orbeon.saxon.functions.URIQueryParameters
//import org.orbeon.saxon.lib.ParseOptions
//import org.orbeon.saxon.lib.Resource
//import org.orbeon.saxon.om.GroundedValue
//import org.orbeon.saxon.om.SpaceStrippingRule
//import org.orbeon.saxon.trans.XPathException
//import org.orbeon.saxon.tree.jiter.MappingJavaIterator
//import org.orbeon.saxon.value.BooleanValue
//import org.orbeon.saxon.value.DateTimeValue
//import org.orbeon.saxon.value.Int64Value
//import org.orbeon.saxon.value.StringValue
//import java.io.File
//import java.io.FilenameFilter
//import java.io.IOException
//import java.net.URI
//import java.net.URISyntaxException
//import java.util._
//
//import DirectoryCollection._
//import org.orbeon.saxon.resource.AbstractResourceCollection.InputDetails
//
//
//
//
//object DirectoryCollection {
//
//  private class DirectoryIterator(
//      private var directories: Stack[Iterator[File]],
//      private var recurse: Boolean,
//      private var filter: FilenameFilter)
//      extends Iterator[String] {
//
//    private var nextt: String = null
//
//    advance()
//
//    def hasNext: Boolean = nextt != null
//
//    def next(): String = {
//      val s: String = nextt
//      advance()
//      s
//    }
//
//    override def remove(): Unit = {
//      throw new UnsupportedOperationException
//    }
//
//    private def advance(): Unit = {
//      if (directories.isEmpty) {
//        nextt = null
//      } else {
//        var files: Iterator[File] = directories.peek()
//        while (!files.hasNext) {
//          directories.pop()
//          if (directories.isEmpty) {
//            nextt = null
//            return
//          }
//          files = directories.peek()
//        }
//        val nextFile: File = files.next()
//        if (nextFile.isDirectory) {
//          if (recurse) {
//            directories.push(
//              Arrays.asList(nextFile.listFiles(filter): _*).iterator)
//          }
//          advance()
//        } else {
//          nextt = nextFile.toURI().toString
//        }
//      }
//    }
//
//  }
//
//}
//
//class DirectoryCollection /**
//  * Create a directory collection
//  * @param collectionURI the collection URI
//  * @param dirFile the directory containing the files
//  * @param params query parameters supplied as part of the URI
//  */
//(config: Configuration,
// collectionURI: String,
// private var dirFile: File,
// params: URIQueryParameters)
//    extends AbstractResourceCollection(config) {
//
//  private var whitespaceRules: SpaceStrippingRule = _
//
//  if (collectionURI == null) {
//    throw new NullPointerException()
//  }
//
//  this.setCollectionURI(collectionURI)
//
//  this.setParams(if (params == null) new URIQueryParameters("", config) else params)
//
//  /**
//    * Supply information about the whitespace stripping rules that apply to this collection.
//    * This method will only be called when the collection() function is invoked from XSLT.
//    *
//    * @param rules the space-stripping rules that apply to this collection, derived from
//    *              the xsl:strip-space and xsl:preserve-space declarations in the stylesheet
//    *              package containing the call to the collection() function.
//    * @return true if the collection finder intends to take responsibility for whitespace
//    * stripping according to these rules; false if it wishes Saxon itself to post-process
//    * any returned XML documents to strip whitespace. Returning true may either indicate
//    * that the collection finder will strip whitespace before returning a document, or it
//    * may indicate that it does not wish the space stripping rules to be applied.  The
//    * default (returned by this method if not overridden) is false.
//    */
//  override def stripWhitespace(rules: SpaceStrippingRule): Boolean = {
//    this.whitespaceRules = rules
//    true
//  }
//
//  def getResourceURIs(context: XPathContext): Iterator[String] =
//    directoryContents(dirFile, params)
//
//  def getResources(context: XPathContext): Iterator[Resource] = {
//    val options: ParseOptions = optionsFromQueryParameters(params, context)
//    options.setSpaceStrippingRule(whitespaceRules)
//    val metadataParam: java.lang.Boolean = params.getMetaData
//    val metadata: Boolean = metadataParam != null && metadataParam
//    val resourceURIs: Iterator[String] = getResourceURIs(context)
//    new MappingJavaIterator(
//      resourceURIs,
//      (in:String) =>
//        try {
//          val details: InputDetails = getInputDetails(in)
//          details.resourceUri = in
//          details.parseOptions = options
//          if (params.getContentType != null) {
//            details.contentType = params.getContentType
//          }
//          val resource: Resource =
//            makeResource(context.getConfiguration, details)
//          if (resource != null) {
//            if (metadata) {
//              makeMetadataResource(resource, details)
//            } else {
//              resource
//            }
//          } else
//            null
//        } catch {
//          case e: XPathException => {
//            val onError: Int = params.getOnError
//            if (onError == URIQueryParameters.ON_ERROR_FAIL) {
//              new FailedResource(in, e)
//            } else if (onError == URIQueryParameters.ON_ERROR_WARNING) {
//              context.getController.warning(
//                "collection(): failed to parse " + in + ": " + e.getMessage,
//                e.getErrorCodeLocalPart,
//                null)
//              null
//            } else {
//              null
//            }
//          }
//
//      }
//    )
//  }
//
//  private def makeMetadataResource(resource: Resource,
//                                   details: InputDetails): MetadataResource = {
//    val properties: Map[String, GroundedValue] =
//      new HashMap[String, GroundedValue]()
//    try {
//      val uri: URI = new URI(resource.getResourceURI)
//      if (details.contentType != null) {
//        properties.put("content-type",
//                       StringValue.makeStringValue(details.contentType))
//      }
//      if (details.encoding != null) {
//        properties.put("encoding",
//                       StringValue.makeStringValue(details.encoding))
//      }
//      if ("file" == uri.getScheme) {
//        val file: File = new File(uri)
//        properties.put("path", StringValue.makeStringValue(file.getPath))
//        properties.put("absolute-path",
//                       StringValue.makeStringValue(file.getAbsolutePath))
//        properties.put("canonical-path",
//                       StringValue.makeStringValue(file.getCanonicalPath))
//        properties.put("can-read", BooleanValue.get(file.canRead()))
//        properties.put("can-write", BooleanValue.get(file.canWrite()))
//        properties.put("can-execute", BooleanValue.get(file.canExecute()))
//        properties.put("is-hidden", BooleanValue.get(file.isHidden))
//        try properties.put("last-modified",
//                           DateTimeValue.fromJavaTime(file.lastModified()))
//        catch {
//          case e: XPathException => {}
//
//        }
//        properties.put("length", new Int64Value(file.length))
//      }
//    } catch {
//      case e @ (_: URISyntaxException | _: IOException) => {}
//
//    }
//    new MetadataResource(resource.getResourceURI, resource, properties)
//  }
//
//   def directoryContents(
//      directory: File,
//      params: URIQueryParameters): Iterator[String] = {
//    var filter: FilenameFilter = null
//    var recurse: Boolean = false
//    if (params != null) {
//      val f: FilenameFilter = params.getFilenameFilter
//      if (f != null) {
//        filter = f
//      }
//      val r: java.lang.Boolean = params.getRecurse
//      if (r != null) {
//        recurse = r
//      }
//    }
//    val directories: Stack[Iterator[File]] = new Stack[Iterator[File]]()
//    directories.push(Arrays.asList(directory.listFiles(filter): _*).iterator)
//    new DirectoryIterator(directories, recurse, filter)
//  }
//
//}
//
//// Copyright (c) 2018-2020 Saxonica Limited
//// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
//// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
//// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///**
//  * This class represents a resource collection containing all, or selected, files within a filestore
//  * directory.
//  */
