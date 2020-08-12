////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Holds a set of namespace bindings as a simple immutable map from prefixes to URIs.
 *
 * <p>A NamespaceMap never physically contains a binding for the XML namespace,
 * but some interfaces behave as if it did.</p>
 *
 * <p>The map may or may not contain a binding for the default namespace, represented
 * by the prefix "" (zero-length string)</p>
 *
 * <p>The map must not contain any namespace undeclarations: that is, the namespace will
 * never be "" (zero-length string)</p>
 */

package net.sf.saxon.om


object NamespaceDeltaMap {

  var EMPTY_MAP: NamespaceDeltaMap = new NamespaceDeltaMap()

  /**
   * Get a namespace map containing no namespace bindings
   *
   * @return an empty namespace map
   */
  def emptyMap(): NamespaceDeltaMap = EMPTY_MAP

}

class NamespaceDeltaMap()
  extends NamespaceMap
    with NamespaceBindingSet
    with NamespaceResolver {

  override def newInstance(): NamespaceMap = new NamespaceDeltaMap()

  override def allowsNamespaceUndeclarations(): Boolean = true

  override def put(prefix: String, uri: String): NamespaceDeltaMap =
    super.put(prefix, uri).asInstanceOf[NamespaceDeltaMap]

  override def remove(prefix: String): NamespaceDeltaMap =
    super.remove(prefix).asInstanceOf[NamespaceDeltaMap]

}


