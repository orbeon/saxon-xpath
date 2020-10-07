////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.javasax

import java.text.{Collator, ParseException, RuleBasedCollator}
import java.util.{Comparator, Locale, Properties}

import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.expr.sort.{AlphanumericCollator, CaseFirstCollator, CodepointCollator, SimpleCollation}
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.trans.XPathException

/**
 * A JavaCollationFactory allows a Collation to be created given
 * a set of properties that the collation should have. This class creates a collator using
 * the facilities of the Java platform; there is a corresponding class that uses the .NET
 * platform.
 */
object JavaCollationFactory {
  /**
   * Make a collator with given properties
   *
   * @param config the Configuration
   * @param uri    the Collation URI
   * @param props  the desired properties of the collation
   * @return a collation with these properties
   */
    /*@Nullable*/ @throws[XPathException]
    def makeCollation(config: Configuration, uri: String, props: Properties) = {
      var collator : Collator= null
      var stringCollator:StringCollator= null
      // If a specific collation class is requested, this overrides everything else
      val classAtt = props.getProperty("class")
      if (classAtt != null) {
        val comparator = config.getInstance(classAtt, null)
        if (comparator.isInstanceOf[Collator]) collator = comparator.asInstanceOf[Collator]
        else if (comparator.isInstanceOf[StringCollator]) stringCollator = comparator.asInstanceOf[StringCollator]
        else if (comparator.isInstanceOf[Comparator[_]]) stringCollator = new SimpleCollation(uri, comparator.asInstanceOf[Comparator[CharSequence]])
        else throw new XPathException("Requested collation class " + classAtt + " is not a Comparator")
      }
      // If rules are specified, create a RuleBasedCollator
      if (collator == null && stringCollator == null) {
        val rulesAtt = props.getProperty("rules")
        if (rulesAtt != null) try collator = new RuleBasedCollator(rulesAtt)
        catch {
          case e: ParseException =>
            throw new XPathException("Invalid collation rules: " + e.getMessage)
        }
        // Start with the lang attribute
        if (collator == null) {
          val langAtt = props.getProperty("lang")
          if (langAtt != null) collator = Collator.getInstance(getLocale(langAtt))
          else collator = Collator.getInstance // use default locale
        }
      }
      if (collator != null) { // See if there is a strength attribute
        val strengthAtt = props.getProperty("strength")
        if (strengthAtt != null) strengthAtt match {
          case "primary" =>
            collator.setStrength(Collator.PRIMARY)
          case "secondary" =>
            collator.setStrength(Collator.SECONDARY)
          case "tertiary" =>
            collator.setStrength(Collator.TERTIARY)
          case "identical" =>
            collator.setStrength(Collator.IDENTICAL)
          case _ =>
            throw new XPathException("strength must be primary, secondary, tertiary, or identical")
        }
        // Look for the properties ignore-case, ignore-modifiers, ignore-width
        var ignore = props.getProperty("ignore-width")
        if (ignore != null) if (ignore == "yes" && strengthAtt == null) collator.setStrength(Collator.TERTIARY)
        else if (ignore == "no") {
          // no-op
        }
        else throw new XPathException("ignore-width must be yes or no")
        ignore = props.getProperty("ignore-case")
        if (ignore != null && strengthAtt == null) ignore match {
          case "yes" =>
            collator.setStrength(Collator.SECONDARY)
          case "no" =>
          case _ =>
            throw new XPathException("ignore-case must be yes or no")
        }
        ignore = props.getProperty("ignore-modifiers")
        if (ignore != null) if (ignore == "yes" && strengthAtt == null) collator.setStrength(Collator.PRIMARY)
        else if (ignore == "no") {
        }
        else throw new XPathException("ignore-modifiers must be yes or no")
        // The ignore-symbols property is ignored
        // See if there is a decomposition attribute
        val decompositionAtt = props.getProperty("decomposition")
        if (decompositionAtt != null) decompositionAtt match {
          case "none" =>
            collator.setDecomposition(Collator.NO_DECOMPOSITION)
          case "standard" =>
            collator.setDecomposition(Collator.CANONICAL_DECOMPOSITION)
          case "full" =>
            collator.setDecomposition(Collator.FULL_DECOMPOSITION)
          case _ =>
            throw new XPathException("decomposition must be none, standard, or full")
        }
      }
      if (stringCollator == null) stringCollator = new SimpleCollation(uri, collator.asInstanceOf[Comparator[CharSequence]])
      // See if there is a case-order property
      val caseOrder = props.getProperty("case-order")
      if (caseOrder != null && !("#default" == caseOrder)) { // force base collator to ignore case differences
        if (collator != null) collator.setStrength(Collator.SECONDARY)
        stringCollator = CaseFirstCollator.makeCaseOrderedCollator(uri, stringCollator, caseOrder)
      }
      // See if there is an alphanumeric property
      val alphanumeric = props.getProperty("alphanumeric")
      if (alphanumeric != null && !("no" == alphanumeric)) alphanumeric match {
        case "yes" =>
          stringCollator = new AlphanumericCollator(stringCollator)
        case "codepoint" =>
          stringCollator = new AlphanumericCollator(CodepointCollator.getInstance)
        case _ =>
          throw new XPathException("alphanumeric must be yes, no, or codepoint")
      }
      stringCollator
    }

  /**
   * Get a locale given a language code in XML format
   */
  private def getLocale(lang: String) = {
    val hyphen = lang.indexOf("-")
    var language :String = null
    var country :String= null
    if (hyphen < 1) {
      language = lang
      country = ""
    }
    else {
      language = lang.substring(0, hyphen)
      country = lang.substring(hyphen + 1)
    }
    new Locale(language, country)
  }
}


/**
 * The class is a never instantiated
 */
abstract class JavaCollationFactory private(){
}