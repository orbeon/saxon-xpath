////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.om

import java.util.Iterator

import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.tree.jiter.MonoIterator

import scala.beans.BeanProperty


object NamespaceBinding {

  val XML: NamespaceBinding =
    new NamespaceBinding("xml", NamespaceConstant.XML)

  val DEFAULT_UNDECLARATION: NamespaceBinding = new NamespaceBinding("", "")

  val EMPTY_ARRAY: Array[NamespaceBinding] = new Array[NamespaceBinding](0)

  def makeNamespaceBinding(prefix: CharSequence,
                           uri: CharSequence): NamespaceBinding =
    if (prefix.length == 0 && uri.length == 0)
      DEFAULT_UNDECLARATION
    else if (prefix.==("xml") && uri == NamespaceConstant.XML)
      XML
    else
      new NamespaceBinding(prefix.toString, uri.toString)
}

/**
  * Represents the binding of a prefix to a URI. Also, in some contexts, represents an unbinding, by
  * virtue of the URI being set to a zero length string.
  *
  * @since 9.4
  */
class NamespaceBinding(prefStr: String, uriStr: String)
    extends NamespaceBindingSet {

  @BeanProperty var prefix: String = prefStr
  private val uri: String = uriStr

  if (prefix == null || uri == null)
    throw new NullPointerException()

  def getURI(prefix: String): String =
    if (prefix == this.prefix) uri else null

  def getURI: String = uri
  def isXmlNamespace: Boolean = prefix == "xml"
  def isDefaultUndeclaration: Boolean = prefix.isEmpty && uri.isEmpty

  /**
    * Returns an iterator over this singleton set of namespace bindings.
    *
    * @return an Iterator.
    */
  def iterator: Iterator[NamespaceBinding] = new MonoIterator[NamespaceBinding](this)

  /**
    * Test if this namespace binding is the same as another
    *
    * @param obj the comparand
    * @return true if the comparand is a Namespace binding of the same prefix to the same URI
    */
  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[NamespaceBinding] &&
      prefix == obj.asInstanceOf[NamespaceBinding].getPrefix &&
      uri == obj.asInstanceOf[NamespaceBinding].getURI

  override def hashCode: Int = prefix.hashCode ^ uri.hashCode
  override def toString: String = prefix + "=" + uri
}
