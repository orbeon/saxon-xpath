////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans

import net.sf.saxon.utils.Configuration

import net.sf.saxon.utils.Version

import org.apache.xml.resolver.CatalogManager

import org.apache.xml.resolver.helpers.Debug

import javax.xml.transform.TransformerException




object XmlCatalogResolver {

  def setCatalog(catalog: String,
                 config: Configuration,
                 isTracing: Boolean): Unit = {
    System.setProperty("xml.catalog.files", catalog)
    Version.platform.setDefaultSAXParserFactory(config)
    if (isTracing) {
// Customize the resolver to write messages to the Saxon logging destination
      CatalogManager.getStaticManager.debug = new Debug() {
        override def message(level: Int, message: String): Unit = {
          if (level <= getDebug) {
            config.getStandardErrorOutput.println(message)
          }
        }

        override def message(level: Int, message: String, spec: String): Unit = {
          if (level <= getDebug) {
            config.getStandardErrorOutput.println(message + ": " + spec)
          }
        }

        override def message(level: Int,
                             message: String,
                             spec1: String,
                             spec2: String): Unit = {
          if (level <= getDebug) {
            config.getStandardErrorOutput.println(message + ": " + spec1)
            config.getStandardErrorOutput.println("\t" + spec2)
          }
        }
      }
      if (CatalogManager.getStaticManager.getVerbosity < 2) {
        CatalogManager.getStaticManager.setVerbosity(2)
      }
    }
    config.setSourceParserClass(
      "org.apache.xml.resolver.tools.ResolvingXMLReader")
    config.setStyleParserClass(
      "org.apache.xml.resolver.tools.ResolvingXMLReader")
    config.setURIResolver(
      config.makeURIResolver("org.apache.xml.resolver.tools.CatalogResolver"))
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Provides the interface to the Apache catalog resolver. This is in a separate class to ensure that no failure
  * occurs if the resolver code is not on the classpath, unless catalog resolution is explicitly requested.
  * The catalog file we assume is a URI
  */
