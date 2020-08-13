
package net.sf.saxon.utils

import java.util.Properties

import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.expr.StaticContext
import net.sf.saxon.expr.parser.RetainedStaticContext
import net.sf.saxon.expr.sort.AtomicMatchKey
import net.sf.saxon.expr.sort.SimpleCollation
import net.sf.saxon.lib.ModuleURIResolver
import net.sf.saxon.lib.StringCollator
import net.sf.saxon.model.ExternalObjectType
import net.sf.saxon.regex.RegularExpression
import org.xml.sax.XMLReader
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource


/**
 * This interface provides access to methods whose implementation depends on the chosen platform
 * (typically Java or .NET)
 */
trait Platform {

  def initialize(config: Configuration): Unit
  def isJava(): Boolean
  def isDotNet(): Boolean
  def getPlatformVersion(): String
  def getPlatformSuffix(): String
  def loadParser(): XMLReader
  def loadParserForXmlFragments(): XMLReader

  def getParserSource(pipe: PipelineConfiguration,
                      input: StreamSource,
                      validation: Int,
                      dtdValidation: Boolean): Source

  /*@Nullable*/
  def makeCollation(config: Configuration,
                    props: Properties,
                    uri: String): StringCollator

  def canReturnCollationKeys(collation: StringCollator): Boolean

  def getCollationKey(namedCollation: SimpleCollation,
                      value: String): AtomicMatchKey

  /**
   * Indicate whether the ICU library is available and supports Collations
   *
   * @return true if the ICU library class for collations appears to be loaded
   */
  def hasICUCollator(): Boolean

  /**
   * Indicate whether the ICU library is available and supports Numberers
   *
   * @return true if the ICU library class for rule-based numbering appears to be loaded
   */
  def hasICUNumberer(): Boolean

  def makeUcaCollator(uri: String, config: Configuration): StringCollator

  def compileRegularExpression(config: Configuration,
                               regex: CharSequence,
                               flags: String,
                               hostLanguage: String,
                               warnings: List[String]): RegularExpression

  def getExternalObjectType(config: Configuration, uri: String, localName: String): ExternalObjectType
  def getInstallationDirectory(edition: String, config: Configuration): String
  def registerAllBuiltInObjectModels(config: Configuration): Unit
  def setDefaultSAXParserFactory(config: Configuration): Unit
  def JAXPStaticContextCheck(retainedStaticContext: RetainedStaticContext, sc: StaticContext): Boolean
  def makeStandardModuleURIResolver(config: Configuration): ModuleURIResolver

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
