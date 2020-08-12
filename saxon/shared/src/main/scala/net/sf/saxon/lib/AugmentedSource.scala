////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import java.util.List

import javax.xml.transform.Source
import javax.xml.transform.sax.SAXSource
import net.sf.saxon.event.FilterFactory
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om.{StructuredQName, TreeModel}
import org.xml.sax.{EntityResolver, XMLReader}

//remove if not needed

object AugmentedSource {

  def makeAugmentedSource(source: Source): AugmentedSource = {
    if (source.isInstanceOf[AugmentedSource]) {
      source.asInstanceOf[AugmentedSource]
    }
    new AugmentedSource(source)
  }

}

class AugmentedSource private (private var source: Source) extends Source {

  private var options: ParseOptions = new ParseOptions()

  private var systemID: String = _

  if (source.isInstanceOf[AugmentedSource]) {
    throw new IllegalArgumentException(
      "Contained source must not be an AugmentedSource")
  }

  def this(source: Source, options: ParseOptions) = {
    this(source)
    if (source.isInstanceOf[AugmentedSource]) {
      throw new IllegalArgumentException(
        "Contained source must not be an AugmentedSource")
    }
    this.source = source
    this.options = options
  }

  def addFilter(filter: FilterFactory): Unit = {
    options.addFilter(filter)
  }

  def getFilters(): List[FilterFactory] = options.getFilters

  def getContainedSource(): Source = source

  def getParseOptions(): ParseOptions = options

  def setModel(model: TreeModel): Unit = {
    options.setModel(model)
  }

  def getModel(): TreeModel = options.getModel

  def setSchemaValidationMode(option: Int): Unit = {
    options.setSchemaValidationMode(option)
  }

  def getSchemaValidation(): Int = options.getSchemaValidationMode

  def setTopLevelElement(elementName: StructuredQName): Unit = {
    options.setTopLevelElement(elementName)
  }

  def getTopLevelElement(): StructuredQName = options.getTopLevelElement

  def setTopLevelType(`type`: SchemaType): Unit = {
    options.setTopLevelType(`type`)
  }

  def getTopLevelType(): SchemaType = options.getTopLevelType

  def setDTDValidationMode(option: Int): Unit = {
    options.setDTDValidationMode(option)
  }

  def getDTDValidation(): Int = options.getDTDValidationMode

  def setLineNumbering(lineNumbering: Boolean): Unit = {
    options.setLineNumbering(lineNumbering)
  }

  def isLineNumbering(): Boolean = options.isLineNumbering

  def isLineNumberingSet(): Boolean = options.isLineNumberingSet

  def setXMLReader(parser: XMLReader): Unit = {
    options.setXMLReader(parser)
    if (source.isInstanceOf[SAXSource]) {
      source.asInstanceOf[SAXSource].setXMLReader(parser)
    }
  }

  /*@Nullable*/

  def getXMLReader(): XMLReader = {
    val parser: XMLReader = options.getXMLReader
    if (parser != null) {
      parser
    } else if (source.isInstanceOf[SAXSource]) {
      source.asInstanceOf[SAXSource].getXMLReader
    } else {
      null
    }
  }

  def setWrapDocument(wrap: java.lang.Boolean): Unit = {
    options.setWrapDocument(wrap)
  }

  def getWrapDocument(): java.lang.Boolean = options.getWrapDocument

  def setSystemId(id: String): Unit = {
    systemID = id
  }

  def getSystemId(): String =
    if (systemID != null) systemID else source.getSystemId

  /**
    * <p>Set state of XInclude processing.</p>
    * <p>If XInclude markup is found in the document instance, should it be
    * processed as specified in <a href="http://www.w3.org/TR/xinclude/">
    * XML Inclusions (XInclude) Version 1.0</a>.</p>
    * <p>XInclude processing defaults to <code>false</code>.</p>
    *
    * @param state Set XInclude processing to <code>true</code> or
    *              <code>false</code>
    * @since 8.9
    */
  def setXIncludeAware(state: Boolean): Unit = {
    options.setXIncludeAware(state)
  }

  def isXIncludeAwareSet(): Boolean = options.isXIncludeAwareSet

  def isXIncludeAware(): Boolean = options.isXIncludeAware

  def setEntityResolver(resolver: EntityResolver): Unit = {
    options.setEntityResolver(resolver)
  }

  def getEntityResolver(): EntityResolver = options.getEntityResolver

  def setErrorReporter(listener: ErrorReporter): Unit = {
    options.setErrorReporter(listener)
  }

  def getErrorReporter(): ErrorReporter = options.getErrorReporter

  def setPleaseCloseAfterUse(close: Boolean): Unit = {
    options.setPleaseCloseAfterUse(close)
  }

  def isPleaseCloseAfterUse(): Boolean = options.isPleaseCloseAfterUse

  def close(): Unit = {
    ParseOptions.close(source)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is an extension of the JAXP Source interface. The class can be used
  * wherever a JAXP Source object can be used, and it provides additional information
  * about the way that the Source is to be processed: for example, it indicates
  * whether or not it should be validated against a schema. Other options that can
  * be set include the SAX XMLReader to be used, and the choice of whether a source
  * in the form of an existing tree should be copied or wrapped.
  * <p>Internally, an AugmentedSource combines an underlying Source object with a
  * {@link ParseOptions} object holding the selected options. Many Saxon interfaces allow
  * the ParseOptions to be supplied directly, making this class unnecessary; but it is useful
  * when passing a Source to a JAXP interface that does not allow further options to be
  * supplied.</p>
  * <p>Note that in general a <code>Source</code> object can only be used once; it is
  * consumed by use. An augmentedSource object is consumed by use if the underlying
  * source object is consumed by use.</p>
  *
  * @since 8.8
  */
