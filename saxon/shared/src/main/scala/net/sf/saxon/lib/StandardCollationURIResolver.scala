////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Version

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import java.net.URI

import java.net.URISyntaxException

import java.util.Properties

import java.util.StringTokenizer

import StandardCollationURIResolver._


object StandardCollationURIResolver {

  private val theInstance: StandardCollationURIResolver =
    new StandardCollationURIResolver()

  def getInstance(): StandardCollationURIResolver = theInstance

}

class StandardCollationURIResolver extends CollationURIResolver {

  /*@Nullable*/

  def resolve(uri: String, config: Configuration): StringCollator =
    if (uri.==("http://saxon.sf.net/collation")) {
      Version.platform.makeCollation(config, new Properties(), uri)
    } else if (uri.startsWith("http://saxon.sf.net/collation?")) {
      var uuri: URI = null
      uuri = new URI(uri)
      val props: Properties = new Properties()
      val query: String = uuri.getRawQuery
      val queryTokenizer: StringTokenizer = new StringTokenizer(query, ";&")
      while (queryTokenizer.hasMoreElements()) {
        val param: String = queryTokenizer.nextToken()
        val eq: Int = param.indexOf('=')
        if (eq > 0 && eq < param.length - 1) {
          val kw: String = param.substring(0, eq)
          val `val`: String = AnyURIValue.decode(param.substring(eq + 1))
          props.setProperty(kw, `val`)
        }
      }
      Version.platform.makeCollation(config, props, uri)
    } else if (uri.startsWith("http://www.w3.org/2013/collation/UCA")) {
      val uca: StringCollator = Version.platform.makeUcaCollator(uri, config)
      if (uca != null) {
        return uca
      }
      if (uri.contains("fallback=no")) {
        null
      } else {
        var uuri: URI = null
        uuri = new URI(uri)
        val props: Properties = new Properties()
        val query: String = AnyURIValue.decode(uuri.getRawQuery)
        for (param <- query.split(";")) {
          val tokens: Array[String] = param.split("=")
          if (tokens.length == 2) {
            var kw: String = tokens(0)
            var `val`: String = tokens(1)
            if (kw.==("fallback")) {
              if (`val`.==("no")) {
                return null
              } else if (`val`.!=("yes")) {
                // effect is implementation-defined, but it seems best to reject it
                return null
              }
            }
            kw match {
              case "strength" =>
                `val` match {
                  case "1" => `val` = "primary"
                  case "2" => `val` = "secondary"
                  case "3" => `val` = "tertiary"
                  case "quaternary" | "4" | "5" => `val` = "identical"

                }
              case "caseFirst" =>
                kw = "case-order"
                // Should check correct?
                `val` += "-first"
              case "numeric" => kw = "alphanumeric"

            }
            props.setProperty(kw, `val`)
          }
        }
        Version.platform.makeCollation(config, props, uri)
      }
    } else {
      null
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * StandardCollationURIResolver allows a Collation to be created given
 * a URI starting with "http://saxon.sf.net/collation" followed by a set of query parameters.
 */
