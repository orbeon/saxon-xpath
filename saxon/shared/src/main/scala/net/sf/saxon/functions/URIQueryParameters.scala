////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.lib.Validation

import net.sf.saxon.om.AllElementsSpaceStrippingRule

import net.sf.saxon.om.IgnorableSpaceStrippingRule

import net.sf.saxon.om.NoElementsSpaceStrippingRule

import net.sf.saxon.om.SpaceStrippingRule

import net.sf.saxon.regex.ARegularExpression

import net.sf.saxon.regex.JavaRegularExpression

import net.sf.saxon.regex.RegularExpression

import net.sf.saxon.trans.Instantiator

import net.sf.saxon.trans.Maker

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import org.xml.sax.XMLReader

import java.io.File

import java.io.FilenameFilter

import java.util.ArrayList

import java.util.StringTokenizer

import URIQueryParameters._




object URIQueryParameters {

  val ON_ERROR_FAIL: Int = 1

  val ON_ERROR_WARNING: Int = 2

  val ON_ERROR_IGNORE: Int = 3

  def makeGlobFilter(value: String): FilenameFilter = {
    val sb: FastStringBuffer = new FastStringBuffer(value.length + 6)
    sb.cat('^')
    for (i <- 0 until value.length) {
      val c: Char = value.charAt(i)
      if (c == '.') {
// replace "." with "\."
        sb.append("\\.")
      } else if (c == '*') {
// replace "*" with ".*"
        sb.append(".*")
      } else if (c == '?') {
// replace "?" with ".?"
        sb.append(".?")
      } else {
        sb.cat(c)
      }
    }
    sb.cat('$')
    new RegexFilter(new JavaRegularExpression(sb, ""))
  }

  class RegexFilter(regex: RegularExpression) extends FilenameFilter {

    private var pattern: RegularExpression = regex

    def accept(dir: File, name: String): Boolean =
      new File(dir, name).isDirectory || pattern.matches(name)

    /**
      * Test whether a name matches the pattern (regardless whether it is a directory or not)
      *
      * @param name the name (last component) of the file
      * @return true if the name matches the pattern.
      */
    def matches(name: String): Boolean = pattern.matches(name)

  }

}

class URIQueryParameters(query: String, config: Configuration) {

  /*@Nullable*/

  var filter: FilenameFilter = null

  var recurse: java.lang.Boolean = null

  var validation: java.lang.Integer = null

  var strippingRule: SpaceStrippingRule = null

  var onError: java.lang.Integer = null

  var parserMaker: Maker[XMLReader] = null

  var xinclude: java.lang.Boolean = null

  var stable: java.lang.Boolean = null

  var metadata: java.lang.Boolean = null

  var contentType: String = null

  if (query != null) {
    val t: StringTokenizer = new StringTokenizer(query, ";&")
    while (t.hasMoreTokens()) {
      val tok: String = t.nextToken()
      val eq: Int = tok.indexOf('=')
      if (eq > 0 && eq < (tok.length - 1)) {
        val keyword: String = tok.substring(0, eq)
        val value: String = tok.substring(eq + 1)
        processParameter(config, keyword, value)
      }
    }
  }

  private def processParameter(config: Configuration,
                               keyword: String,
                               value: String): Unit = {
    if (keyword.==("select")) {
      filter = makeGlobFilter(value)
    } else if (keyword.==("match")) {
      val regex: ARegularExpression =
        new ARegularExpression(value, "", "XP", new ArrayList(), config)
      filter = new RegexFilter(regex)
    } else if (keyword.==("recurse")) {
      recurse = "yes" == value
    } else if (keyword.==("validation")) {
      val v: Int = Validation.getCode(value)
      if (v != Validation.INVALID) {
        validation = v
      }
    } else if (keyword.==("strip-space")) {
      value match {
        case "yes" => strippingRule = AllElementsSpaceStrippingRule.getInstance
        case "ignorable" =>
          strippingRule = IgnorableSpaceStrippingRule.getInstance
        case "no" => strippingRule = NoElementsSpaceStrippingRule.getInstance

      }
    } else if (keyword.==("stable")) {
      if (value.==("yes")) {
        stable = true
      } else if (value.==("no")) {
        stable = false
      }
    } else if (keyword.==("metadata")) {
      if (value.==("yes")) {
        metadata = true
      } else if (value.==("no")) {
        metadata = false
      }
    } else if (keyword.==("xinclude")) {
      if (value.==("yes")) {
        xinclude = true
      } else if (value.==("no")) {
        xinclude = false
      }
    } else if (keyword.==("content-type")) {
      contentType = value
    } else if (keyword.==("on-error")) {
      value match {
        case "warning" => onError = ON_ERROR_WARNING
        case "ignore" => onError = ON_ERROR_IGNORE
        case "fail" => onError = ON_ERROR_FAIL

      }
    } else if (keyword.==("parser") && config != null) {
      parserMaker = new Instantiator(value, config)
    }
  }

  def getSpaceStrippingRule: SpaceStrippingRule = strippingRule

  def getValidationMode: java.lang.Integer = validation

  def getFilenameFilter: FilenameFilter = filter

  def getRecurse: java.lang.Boolean = recurse

  def getOnError: java.lang.Integer = onError

  def getXInclude: java.lang.Boolean = xinclude

  def getMetaData: java.lang.Boolean = metadata

  def getContentType: String = contentType

  def getStable: java.lang.Boolean = stable

  def getXMLReaderMaker: Maker[XMLReader] = parserMaker

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A set of query parameters on a URI passed to the collection() or document() function
  */
