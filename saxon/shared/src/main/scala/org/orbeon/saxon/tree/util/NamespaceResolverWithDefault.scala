////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.util

import org.orbeon.saxon.om.NamespaceResolver

import java.util.ArrayList

import java.util.Iterator


/**
  * This class is a NamespaceResolver that modifies an underyling NamespaceResolver
  * by changing the mapping of the null prefix to be a specified namespace, rather than
  * the one used by the underlying namespace resolver.
  */
class NamespaceResolverWithDefault(base: NamespaceResolver, defaultNamespace: String)
    extends NamespaceResolver {

  /*@Nullable*/
  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    if (useDefault && prefix.isEmpty)
      defaultNamespace
    else
      base.getURIForPrefix(prefix, useDefault)

  def iteratePrefixes: Iterator[String] = {
    val list = new ArrayList[String](10)
    val it = base.iteratePrefixes
    while (it.hasNext) {
      val p = it.next()
      if (p.length != 0)
        list.add(p)
    }
    list.add("")
    list.iterator
  }
}

