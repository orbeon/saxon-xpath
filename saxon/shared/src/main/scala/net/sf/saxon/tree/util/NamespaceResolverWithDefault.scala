////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.util

import net.sf.saxon.om.NamespaceResolver

import java.util.ArrayList

import java.util.Iterator




/**
  * This class is a NamespaceResolver that modifies an underyling NamespaceResolver
  * by changing the mapping of the null prefix to be a specified namespace, rather than
  * the one used by the underlying namespace resolver.
  */
class NamespaceResolverWithDefault(base: NamespaceResolver,
                                   private var defaultNamespace: String)
    extends NamespaceResolver {

  private var baseResolver: NamespaceResolver = base

  /*@Nullable*/

  def getURIForPrefix(prefix: String, useDefault: Boolean): String =
    if (useDefault && prefix.isEmpty) {
      defaultNamespace
    } else {
      baseResolver.getURIForPrefix(prefix, useDefault)
    }

  def iteratePrefixes: Iterator[String] = {
    val list: ArrayList[String] = new ArrayList[String](10)
    var it: Iterator[String] = baseResolver.iteratePrefixes
    while (it.hasNext) {
      val p: String = it.next()
      if (p.length != 0) {
        list.add(p)
      }
    }
    list.add("")
    list.iterator
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
