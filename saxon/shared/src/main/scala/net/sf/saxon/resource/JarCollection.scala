////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.resource

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.functions.URIQueryParameters
import net.sf.saxon.lib.ParseOptions
import net.sf.saxon.lib.Resource
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.SpaceStrippingRule
import net.sf.saxon.trans.UncheckedXPathException
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.Base64BinaryValue
import net.sf.saxon.value.DateTimeValue
import net.sf.saxon.value.Int64Value
import net.sf.saxon.value.StringValue
import java.io._
import java.net.MalformedURLException
import java.net.URL
import java.net.URLConnection
import java.util._
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream

import net.sf.saxon.resource.AbstractResourceCollection.InputDetails

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.util.control.Breaks._




class JarCollection(private var context: XPathContext,
                    @BeanProperty var collectionUri: String,
                    params: URIQueryParameters)
    extends AbstractResourceCollection(context.getConfiguration) {

  private var whitespaceRules: SpaceStrippingRule = _

  this.setParams(params)

  /**
    * Supply information about the whitespace stripping rules that apply to this collection.
    * This method will only be called when the collection() function is invoked from XSLT.
    *
    * @param rules the space-stripping rules that apply to this collection, derived from
    *              the xsl:strip-space and xsl:preserve-space declarations in the stylesheet
    *              package containing the call to the collection() function.
    * @return true if the collection finder intends to take responsibility for whitespace
    * stripping according to these rules; false if it wishes Saxon itself to post-process
    * any returned XML documents to strip whitespace. Returning true may either indicate
    * that the collection finder will strip whitespace before returning a document, or it
    * may indicate that it does not wish the space stripping rules to be applied.  The
    * default (returned by this method if not overridden) is false.
    */
  override def stripWhitespace(rules: SpaceStrippingRule): Boolean = {
    this.whitespaceRules = rules
    true
  }

  def getResourceURIs(context: XPathContext): Iterator[String] = {
    var filter: FilenameFilter = null
    var recurse: Boolean = false
    if (params != null) {
      val f: FilenameFilter = params.getFilenameFilter
      if (f != null) {
        filter = f
      }
      val r: java.lang.Boolean = params.getRecurse
      if (r != null) {
        recurse = r
      }
    }
    val zipInputStream: ZipInputStream = getZipInputStream
    val result: List[String] = new ArrayList[String]()
    var entry: ZipEntry = null
    var dirStr: String = ""
    while ((entry = zipInputStream.getNextEntry) != null) {
      if (entry.isDirectory) {
        dirStr = entry.getName
      }
      if (!entry.isDirectory) {
        val entryName: String = entry.getName
        if (filter != null) {
          if (dirStr.==("") || !entryName.contains(dirStr)) {
            dirStr =
              if (entryName.contains("/"))
                entryName.substring(0, entryName.lastIndexOf("/"))
              else ""
          }
          if (filter.accept(new File(dirStr), entryName)) {
            result.add(makeResourceURI(entryName))
          }
        } else {
          result.add(makeResourceURI(entryName))
        }
      }
      entry = zipInputStream.getNextEntry
    }
    result.iterator()
  }

  private def getZipInputStream(): ZipInputStream = {
    var url: URL = null
    url = new URL(collectionUri)
    var connection: URLConnection = null
    connection = url.openConnection()
    var stream: InputStream = null
    stream = connection.getInputStream
    new ZipInputStream(stream)
  }

  def getResources(context: XPathContext): Iterator[Resource] = {
    var filter: FilenameFilter = null
    var recurse: Boolean = false
    if (params != null) {
      val f: FilenameFilter = params.getFilenameFilter
      if (f != null) {
        filter = f
      }
      val r: java.lang.Boolean = params.getRecurse
      if (r != null) {
        recurse = r
      }
    }
    val zipInputStream: ZipInputStream = getZipInputStream
    new JarIterator(this.context, zipInputStream, filter)
  }

  private def makeResourceURI(entryName: String): String =
    (if (collectionUri.startsWith("jar:")) "" else "jar:") +
      collectionUri +
      "!/" +
      entryName

  private class JarIterator(private var context: XPathContext,
                            private var zipInputStream: ZipInputStream,
                            private var filter: FilenameFilter)
      extends Iterator[Resource]
      with Closeable {

    private var nextt: Resource = null

    private var dirStr: String = ""

    private var options: ParseOptions =
      optionsFromQueryParameters(params, context)

    private var metadata: Boolean = metadataParam != null && metadataParam

    this.options.setSpaceStrippingRule(whitespaceRules)

    val metadataParam: java.lang.Boolean =
      if (params == null) null else params.getMetaData

    advance()

    def hasNext(): Boolean = {
      val more: Boolean = nextt != null
      if (!more) {
        zipInputStream.close()
      }
      more
    }

    def next(): Resource = {
      val current: Resource = nextt
      advance()
      current
    }

    override def remove(): Unit = {
      throw new UnsupportedOperationException()
    }

    private def advance(): Unit = {
      while (true) {
        var entry: ZipEntry = null
        breakable {
        try {
          entry = zipInputStream.getNextEntry
          if (entry == null) {
            nextt = null
            return
          }
        } catch {
          case e: IOException => {
            nextt = new FailedResource(null, new XPathException(e))
            break
          }

        }
      }
        if (entry.isDirectory) {
          dirStr = entry.getName
        } else {
          val entryName: String = entry.getName
          if (filter != null) {
            if (dirStr.==("") || !entryName.contains(dirStr)) {
              dirStr =
                if (entryName.contains("/"))
                  entryName.substring(0, entryName.lastIndexOf("/"))
                else ""
            }
            if (!filter.accept(new File(dirStr), entryName)) {
            }
          }
          var resourceURI: String = null
          try {
            val is: InputStream = zipInputStream
            val output: ByteArrayOutputStream = new ByteArrayOutputStream()
            try {
              val buffer: Array[Byte] = Array.ofDim[Byte](4096)
              var len: Int = 0
              len = is.read(buffer)
              while (len > 0) {
                output.write(buffer, 0, len)
                len = is.read(buffer)
              }
            } catch {
              case err: IOException =>
                throw new UncheckedXPathException(new XPathException(err))

            } finally // we must always close the output file
              try output.close()
              catch {
                case e: IOException =>
                  nextt = new FailedResource(null, new XPathException(e))

              }
            val details: InputDetails = new InputDetails()
            details.binaryContent = output.toByteArray()
            details.contentType =
              if (params != null && params.getContentType != null)
                params.getContentType
              else guessContentTypeFromName(entry.getName)
            if (details.contentType == null) {
              val bais: ByteArrayInputStream = new ByteArrayInputStream(
                details.binaryContent)
              details.contentType = guessContentTypeFromContent(bais)
              try bais.close()
              catch {
                case e: IOException => details.contentType = null

              }
            }
            details.parseOptions = options
            resourceURI = makeResourceURI(entry.getName)
            details.resourceUri = resourceURI
            nextt = makeResource(context.getConfiguration, details)
            if (metadata) {
              val properties: Map[String, GroundedValue] = makeProperties(
                entry)
              nextt = new MetadataResource(resourceURI, nextt, properties)
            }
            return
          } catch {
            case e: XPathException => nextt = new FailedResource(resourceURI, e)

          }
        }
      }
    }

    override def close(): Unit = {
      zipInputStream.close()
    }

  }

   def makeProperties(entry: ZipEntry): Map[String, GroundedValue] = {
    val map: HashMap[String, GroundedValue] =
      new HashMap[String, GroundedValue](10)
    map.put("comment", StringValue.makeStringValue(entry.getComment))
    map.put("compressed-size", new Int64Value(entry.getCompressedSize))
    map.put("crc", new Int64Value(entry.getCrc))
    val extra: Array[Byte] = entry.getExtra
    if (extra != null) {
      map.put("extra", new Base64BinaryValue(extra))
    }
    map.put("compression-method", new Int64Value(entry.getMethod))
    map.put("entry-name", StringValue.makeStringValue(entry.getName))
    map.put("size", new Int64Value(entry.getSize))
    try map.put("last-modified", DateTimeValue.fromJavaTime(entry.getTime))
    catch {
      case err: XPathException => {}

    }
    map
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A JarCollection represents a collection of resources held in a JAR or ZIP archive, accessed typically
  * using a URI using the (Java-defined) "jar" URI scheme, or simply a "file" URI where the target
  * file is a JAR or ZIP file.
  */
