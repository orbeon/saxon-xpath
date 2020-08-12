////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.lib.NamespaceConstant
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om._
import net.sf.saxon.s9api.Location
import net.sf.saxon.trans.XPathException
import java.util
import scala.util.control.Breaks._

/**
 * <tt>NamespaceReducer</tt> is a {@link ProxyReceiver} responsible for removing duplicate namespace
 * declarations. It also ensures that an {@code xmlns=""} undeclaration is output when
 * necessary. Used on its own, the {@code NamespaceReducer} simply eliminates unwanted
 * namespace declarations. It can also be subclassed, in which case the subclass
 * can use the services of the {@code NamespaceReducer} to resolve QNames.
 * <p>The {@code NamespaceReducer} also validates namespace-sensitive content.</p>
 */
class NamespaceReducer(val next: Receiver)

/**
 * Create a NamespaceReducer
 *
 * @param next the Receiver to which events will be passed after namespace reduction
 */
  extends ProxyReceiver(next) with NamespaceResolver {
  private var namespaces = new Array[NamespaceBinding](50) // all namespace codes currently declared
  private var namespacesSize = 0 // all namespaces currently declared
  private var countStack = new Array[Int](50)
  private var depth = 0
  private var disinheritStack = new Array[Boolean](50)
  private var pendingUndeclarations : Array[NamespaceBinding] = null

  /**
   * startElement. This call removes redundant namespace declarations, and
   * possibly adds an xmlns="" undeclaration.
   */
  @throws[XPathException]
  override def startElement(elemName: NodeName, `type`: SchemaType, attributes: AttributeMap, namespaceMap: NamespaceMap, location: Location, properties: Int) = {
    nextReceiver.startElement(elemName, `type`, attributes, namespaceMap, location, properties)
    if (ReceiverOption.contains(properties, ReceiverOption.REFUSE_NAMESPACES)) { // Typically XQuery: the element does not inherit namespaces from its parent
      pendingUndeclarations = util.Arrays.copyOf(namespaces, namespacesSize)
    }
    else if (depth > 0 && disinheritStack(depth - 1)) { // If the parent element specified inherit=no, keep a list of namespaces that need to be
      // undeclared. Note (bug 20340) that namespaces are still inherited from grandparent elements
      val undeclarations = new util.ArrayList[NamespaceBinding](namespacesSize)
      var k = namespacesSize
      breakable{
      for (d <- depth - 1 to 0 by -1) {
        if (!disinheritStack(d)) break //todo: break is not supported
        for (i <- 0 until countStack(d)) {
          undeclarations.add(namespaces({
            k -= 1;
            k
          }))
        }
      }
    }
      pendingUndeclarations = undeclarations.toArray(NamespaceBinding.EMPTY_ARRAY)
    }
    else pendingUndeclarations = null
    // Record the current height of the namespace list so it can be reset at endElement time
    countStack(depth) = 0
    disinheritStack(depth) = ReceiverOption.contains(properties, ReceiverOption.DISINHERIT_NAMESPACES)
    if ( {
      depth += 1; depth
    } >= countStack.length) {
      countStack = util.Arrays.copyOf(countStack, depth * 2)
      disinheritStack = util.Arrays.copyOf(disinheritStack, depth * 2)
    }
  }

  /**
   * Determine whether a namespace declaration is needed
   *
   * @param nsBinding the namespace binding
   * @return true if the namespace is needed: that is, if it not the XML namespace, is not a duplicate,
   *         and is not a redundant xmlns="".
   */
  private def isNeeded(nsBinding: NamespaceBinding): Boolean = {
    if (nsBinding.isXmlNamespace) { // Ignore the XML namespace
      return false
    }
    // First cancel any pending undeclaration of this namespace prefix (there may be more than one)
    val prefix = nsBinding.getPrefix
    if (pendingUndeclarations != null) for (p <- 0 until pendingUndeclarations.length) {
      val nb = pendingUndeclarations(p)
      if (nb != null && prefix == nb.getPrefix) {
        pendingUndeclarations(p) = null
        //break;
      }
    }
    for (i <- namespacesSize - 1 to 0 by -1) {
      if (namespaces(i) == nsBinding) { // it's a duplicate so we don't need it
        return false
      }
      if (namespaces(i).getPrefix == nsBinding.getPrefix) { // same prefix, different URI.
        return true
      }
    }
    // we need it unless it's a redundant xmlns=""
    !nsBinding.isDefaultUndeclaration
  }

  /**
   * Add a namespace declaration to the stack
   *
   * @param nsBinding the namespace code to be added
   */
  private def addToStack(nsBinding: NamespaceBinding) = { // expand the stack if necessary
    if (namespacesSize + 1 >= namespaces.length) namespaces = util.Arrays.copyOf(namespaces, namespacesSize * 2)
    namespaces({
      namespacesSize += 1; namespacesSize - 1
    }) = nsBinding
  }

  /**
   * Ask whether the namespace reducer is disinheriting namespaces at the current level
   *
   * @return true if namespaces are being disinherited
   */
  def isDisinheritingNamespaces = depth > 0 && disinheritStack(depth - 1)

  /**
   * endElement: Discard the namespaces declared on this element.
   */
  @throws[XPathException]
  override def endElement() = {
    if ( {
      depth -= 1; depth + 1
    } == 0) throw new IllegalStateException("Attempt to output end tag with no matching start tag")
    namespacesSize -= countStack(depth)
    nextReceiver.endElement()
  }

  /**
   * Get the namespace URI corresponding to a given prefix. Return null
   * if the prefix is not in scope.
   *
   * @param prefix     the namespace prefix
   * @param useDefault true if the default namespace is to be used when the
   *                   prefix is ""
   * @return the uri for the namespace, or null if the prefix is not in scope
   */
  /*@Nullable*/ override def getURIForPrefix(prefix: String, useDefault: Boolean): String = {
    if (prefix.isEmpty && !useDefault) return NamespaceConstant.NULL
    else if ("xml" == prefix) return NamespaceConstant.XML
    else for (i <- namespacesSize - 1 to 0 by -1) {
      if (namespaces(i).getPrefix == prefix) return namespaces(i).getURI
    }
    if (prefix.isEmpty) NamespaceConstant.NULL
    else null
  }

  /**
   * Get an iterator over all the prefixes declared in this namespace context. This will include
   * the default namespace (prefix="") and the XML namespace where appropriate
   */
  override def iteratePrefixes = {
    val prefixes = new util.ArrayList[String](namespacesSize)
    for (i <- namespacesSize - 1 to 0 by -1) {
      val prefix = namespaces(i).getPrefix
      if (!prefixes.contains(prefix)) prefixes.add(prefix)
    }
    prefixes.add("xml")
    prefixes.iterator
  }
}