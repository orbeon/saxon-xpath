////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import java.io.IOException
import java.util._

import javax.xml.transform.Source
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamSource
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.event.{Builder, FilterFactory}
import org.orbeon.saxon.expr.accum.Accumulator
import org.orbeon.saxon.model.{SchemaType, ValidationParams}
import org.orbeon.saxon.om.{SpaceStrippingRule, StructuredQName, TreeModel}
import org.orbeon.saxon.trans.Maker
import org.xml.sax.{EntityResolver, ErrorHandler, InputSource, XMLReader}

import scala.beans.{BeanProperty, BooleanBeanProperty}

//remove if not needed

object ParseOptions {

  def close(source: Source): Unit = {
    try if (source.isInstanceOf[StreamSource]) {
      val ss: StreamSource = source.asInstanceOf[StreamSource]
      if (ss.getInputStream != null) {
        ss.getInputStream.close()
      }
      if (ss.getReader != null) {
        ss.getReader.close()
      }
    } else if (source.isInstanceOf[SAXSource]) {
      val is: InputSource = source.asInstanceOf[SAXSource].getInputSource
      if (is != null) {
        if (is.getByteStream != null) {
          is.getByteStream.close()
        }
        if (is.getCharacterStream != null) {
          is.getCharacterStream.close()
        }
      }
    } else if (source.isInstanceOf[AugmentedSource]) {
      source.asInstanceOf[AugmentedSource].close()
    } catch {
      case err: IOException => {}

    }
  }

}

class ParseOptions {

  private var schemaValidation: Int = Validation.DEFAULT

  private var dtdValidation: Int = Validation.DEFAULT

  @BeanProperty
  var topLevelElement: StructuredQName = _

  @BeanProperty
  var topLevelType: SchemaType = _

  /*@Nullable*/

  @transient private var parser: XMLReader = null

  private var parserMaker: Maker[XMLReader] = _

  /*@Nullable*/

  @BeanProperty
  var wrapDocument: java.lang.Boolean = null

  /*@Nullable*/

  private var treeModel: TreeModel = null

  //private int stripSpace = Whitespace.UNSPECIFIED;
  @BeanProperty
  var spaceStrippingRule: SpaceStrippingRule = null

  /*@Nullable*/

  private var lineNumbering: java.lang.Boolean = null

  private var pleaseClose: Boolean = false

  var errorReporter: ErrorReporter = null

  /*@Nullable*/

  @BeanProperty
  var entityResolver: EntityResolver = null

  /*@Nullable*/

  @BeanProperty
  var errorHandler: ErrorHandler = null

  /*@Nullable*/

  @BeanProperty
  var filters: List[FilterFactory] = null

  @BooleanBeanProperty
  var continueAfterValidationErrors: Boolean = false

  @BooleanBeanProperty
  var addCommentsAfterValidationErrors: Boolean = false

  @BooleanBeanProperty
  var expandAttributeDefaults: Boolean = true

  @BooleanBeanProperty
  var useXsiSchemaLocation: Boolean = true

  @BooleanBeanProperty
  var checkEntityReferences: Boolean = false

  @BooleanBeanProperty
  var stable: Boolean = true

  @BeanProperty
  var validationErrorLimit: Int = java.lang.Integer.MAX_VALUE

  /*@Nullable*/

  @BeanProperty
  var validationParams: ValidationParams = null

  /*@Nullable*/

  @BeanProperty
  var validationStatisticsRecipient: ValidationStatisticsRecipient = null

  @BeanProperty
  var parserFeatures: Map[String, Boolean] = null

  @BeanProperty
  var parserProperties: Map[String, Any] = null

  @BeanProperty
  var invalidityHandler: InvalidityHandler = null

  // null means "all"
  @BeanProperty
  var applicableAccumulators: Set[_ <: Accumulator] = null

  def this(p: ParseOptions) = {
    this()
    schemaValidation = p.schemaValidation
    validationParams = p.validationParams
    setDTDValidationMode(p.dtdValidation)
    topLevelElement = p.topLevelElement
    topLevelType = p.topLevelType
    parserMaker = p.getXMLReaderMaker
    parser = p.parser
    wrapDocument = p.wrapDocument
    treeModel = p.treeModel
    spaceStrippingRule = p.spaceStrippingRule
    lineNumbering = p.lineNumbering
    pleaseClose = p.pleaseClose
    errorHandler = p.errorHandler
    errorReporter = p.errorReporter
    entityResolver = p.entityResolver
    invalidityHandler = p.invalidityHandler
    stable = p.stable
    if (p.filters != null) {
      filters = new ArrayList(p.filters)
    }
    this.expandAttributeDefaults = p.expandAttributeDefaults
    useXsiSchemaLocation = p.useXsiSchemaLocation
    validationErrorLimit = p.validationErrorLimit
    continueAfterValidationErrors = p.continueAfterValidationErrors
    addCommentsAfterValidationErrors = p.addCommentsAfterValidationErrors
    if (p.parserFeatures != null) {
      parserFeatures = new HashMap(p.parserFeatures)
    }
    if (p.parserProperties != null) {
      parserProperties = new HashMap(p.parserProperties)
    }
    applicableAccumulators = p.applicableAccumulators
    checkEntityReferences = p.checkEntityReferences
    validationStatisticsRecipient = p.validationStatisticsRecipient
  }

  def merge(options: ParseOptions): Unit = {
    if (options.dtdValidation != Validation.DEFAULT) {
      setDTDValidationMode(options.dtdValidation)
    }
    if (options.schemaValidation != Validation.DEFAULT) {
      schemaValidation = options.schemaValidation
    }
    if (options.invalidityHandler != null) {
      invalidityHandler = options.invalidityHandler
    }
    if (options.topLevelElement != null) {
      topLevelElement = options.topLevelElement
    }
    if (options.topLevelType != null) {
      topLevelType = options.topLevelType
    }
    if (options.parser != null) {
      parser = options.parser
    }
    if (options.wrapDocument != null) {
      wrapDocument = options.wrapDocument
    }
    if (options.treeModel != null) {
      treeModel = options.treeModel
    }
    if (options.spaceStrippingRule != null) {
      spaceStrippingRule = options.spaceStrippingRule
    }
    if (options.lineNumbering != null) {
      lineNumbering = options.lineNumbering
    }
    if (options.pleaseClose) {
      pleaseClose = true
    }
    if (options.errorReporter != null) {
      errorReporter = options.errorReporter
    }
    if (options.entityResolver != null) {
      entityResolver = options.entityResolver
    }
    if (options.filters != null) {
      if (filters == null) {
        filters = new ArrayList()
      }
      filters.addAll(options.filters)
    }
    if (options.parserFeatures != null) {
      if (parserFeatures == null) {
        parserFeatures = new HashMap()
      }
      parserFeatures.putAll(options.parserFeatures)
    }
    if (options.parserProperties != null) {
      if (parserProperties == null) {
        parserProperties = new HashMap()
      }
      parserProperties.putAll(options.parserProperties)
    }
    if (!options.expandAttributeDefaults) {
      // expand defaults unless the other options says don't
      this.expandAttributeDefaults = false
    }
    if (!options.useXsiSchemaLocation) {
      // expand defaults unless the other options says don't
      useXsiSchemaLocation = false
    }
    if (options.addCommentsAfterValidationErrors) {
      // add comments if either set of options requests it
      addCommentsAfterValidationErrors = true
    }
    validationErrorLimit =
      java.lang.Math.min(validationErrorLimit, options.validationErrorLimit)
  }

  def applyDefaults(config: Configuration): Unit = {
    if (dtdValidation == Validation.DEFAULT) {
      setDTDValidationMode(
        if (config.isValidation) Validation.STRICT else Validation.SKIP)
    }
    if (schemaValidation == Validation.DEFAULT) {
      schemaValidation = config.getSchemaValidationMode
    }
    if (treeModel == null) {
      treeModel = TreeModel.getTreeModel(config.getTreeModel)
    }
    if (spaceStrippingRule == null) {
      spaceStrippingRule = config.getParseOptions.getSpaceStrippingRule
    }
    if (lineNumbering == null) {
      lineNumbering = config.isLineNumbering
    }
    if (errorReporter == null) {
      this.errorReporter = config.makeErrorReporter
    }
  }

  def addFilter(filterFactory: FilterFactory): Unit = {
    if (filters == null) {
      filters = new ArrayList(5)
    }
    filters.add(filterFactory)
  }

  def setTreeModel(model: Int): Unit = {
    treeModel = TreeModel.getTreeModel(model)
  }

  /**
   * Add a parser feature to a map, which will be applied to the XML parser later
   *
   * @param uri   The features as a URIs
   * @param value The value given to the feature as boolean
   */
  def addParserFeature(uri: String, value: Boolean): Unit = {
    if (parserFeatures == null) {
      parserFeatures = new HashMap()
    }
    parserFeatures.put(uri, value)
  }

  /**
   * Add a parser property to a map, which is applied to the XML parser later
   *
   * @param uri   The properties as a URIs
   * @param value The value given to the properties as a string
   */
  def addParserProperties(uri: String, value: Any): Unit = {
    if (parserProperties == null) {
      parserProperties = new HashMap()
    }
    parserProperties.put(uri, value)
  }

  /**
   * Get a particular parser feature added
   *
   * @param uri The feature name as a URIs
   * @return The feature value as boolean
   */
  def getParserFeature(uri: String): Boolean = parserFeatures.get(uri)

  /**
   * Get a particular parser property added
   *
   * @param name The properties as a URIs
   * @return The property value (which may be any object)
   */
  def getParserProperty(name: String): Any = parserProperties.get(name)

  def getTreeModel: Int = {
    if (treeModel == null) {
      Builder.UNSPECIFIED_TREE_MODEL
    }
    treeModel.getSymbolicValue
  }

  def setModel(model: TreeModel): Unit = {
    treeModel = model
  }

  def getModel: TreeModel =
    if (treeModel == null) TreeModel.TINY_TREE else treeModel

  def setSchemaValidationMode(option: Int): Unit = {
    schemaValidation = option
  }

  def getSchemaValidationMode: Int = schemaValidation

  def setDTDValidationMode(option: Int): Unit = {
    dtdValidation = option
    addParserFeature("http://xml.org/sax/features/validation",
      option == Validation.STRICT || option == Validation.LAX)
  }

  def getDTDValidationMode: Int = dtdValidation

  def setLineNumbering(lineNumbering: Boolean): Unit = {
    this.lineNumbering = lineNumbering
  }

  def isLineNumbering: Boolean = lineNumbering != null && lineNumbering

  def isLineNumberingSet: Boolean = lineNumbering != null

  def setXMLReader(parser: XMLReader): Unit = {
    this.parser = parser
  }

  /*@Nullable*/

  def getXMLReader: XMLReader = parser

  def setXMLReaderMaker(parserMaker: Maker[XMLReader]): Unit = {
    this.parserMaker = parserMaker
  }

  /*@Nullable*/

  def getXMLReaderMaker: Maker[XMLReader] = parserMaker

  def obtainXMLReader(): XMLReader =
    if (parserMaker != null) {
      parserMaker.make()
    } else if (parser != null) {
      parser
    } else {
      null
    }

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
    addParserFeature("http://apache.org/xml/features/xinclude", state)
  }

  def isXIncludeAwareSet: Boolean =
    parserFeatures != null &&
      parserFeatures.get("http://apache.org/xml/features/xinclude") ne null

  def isXIncludeAware: Boolean =
    if (parserFeatures == null) {
      false
    } else {
      val b: java.lang.Boolean =
        parserFeatures.get("http://apache.org/xml/features/xinclude")
      b != null && b
    }

  def setErrorReporter(reporter: ErrorReporter): Unit = {
    var reporterVar = reporter
    if (reporterVar == null) {
      reporterVar = new StandardErrorReporter()
    }
    errorReporter = reporterVar
  }

  def getErrorReporter: ErrorReporter = errorReporter

  def setPleaseCloseAfterUse(close: Boolean): Unit = {
    pleaseClose = close
  }

  def isPleaseCloseAfterUse: Boolean = pleaseClose

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class defines options for parsing and/or validating a source document. Some of the options
 * are relevant only when parsing, some only when validating, but they are combined into a single
 * class because the two operations are often performed together.
 */
