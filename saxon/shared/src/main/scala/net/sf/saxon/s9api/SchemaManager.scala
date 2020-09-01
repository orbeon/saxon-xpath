////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.s9api

import net.sf.saxon.lib.ErrorReporter

import net.sf.saxon.lib.SchemaURIResolver

import javax.xml.transform.ErrorListener

import javax.xml.transform.Source




/**
  * The SchemaManager is used to load schema documents, and to set options for the way in which they are loaded.
  * At present all the resulting schema components are held in a single pool owned by the Processor object.
  * <p>To obtain a <code>SchemaManager</code>, use the method {@link net.sf.saxon.s9api.Processor#getSchemaManager()}.</p>
  * <p>In a schema-aware Processor there is exactly one
  * <code>SchemaManager</code> (in a non-schema-aware Processor there is none).</p>
  * <p>The cache of compiled schema definitions can include only one schema
  * component (for example a type, or an element declaration) with any given name.
  * An attempt to compile two different schemas in the same namespace will usually
  * therefore fail.</p>
  * <p>As soon as a type definition or element declaration is used for the first
  * time in a validation episode, it is marked as being "sealed": this prevents subsequent
  * modifications to the component. Examples of modifications that are thereby disallowed
  * include adding to the substitution group of an existing element declaration, adding subtypes
  * to an existing type, or redefining components using &lt;xs:redefine&gt;</p>
  */
abstract class SchemaManager {

  def setXsdVersion(version: String): Unit

  def getXsdVersion: String

  def setErrorListener(listener: ErrorListener): Unit

  /*@Nullable*/

  def getErrorListener: ErrorListener

  def setErrorReporter(reporter: ErrorReporter): Unit

  def getErrorReporter: ErrorReporter

  def setSchemaURIResolver(resolver: SchemaURIResolver): Unit

  def getSchemaURIResolver: SchemaURIResolver

  def load(source: Source): Unit

  def importComponents(source: Source): Unit

  def exportComponents(destination: Destination): Unit

  def newSchemaValidator(): SchemaValidator

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
