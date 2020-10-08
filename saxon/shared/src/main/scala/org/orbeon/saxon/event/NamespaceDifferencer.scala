////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.lib.SaxonOutputKeys
import org.orbeon.saxon.model.SchemaType
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location

//import scala.collection.compat._
import java.util.Properties

import scala.jdk.CollectionConverters._


/**
 * <p><tt>NamespaceDifferencer</tt> is a {@link ProxyReceiver} responsible for removing duplicate namespace
 * declarations. It also ensures that namespace undeclarations are emitted when necessary.</p>
 *
 * <p>The NamespaceDifferencer assumes that in the input event stream, all in-scope namespaces for every element
 * are accounted for in the call on namespace(). In the output event stream, the namespace() call represents
 * namespace declarations rather than in-scope namespaces. So (a) redundant namespaces are removed,
 * and (b) namespace undeclarations are added where necessary. A namespace undeclaration for the default
 * namespace is always added if the parent element has a default namespace and the child element does not;
 * namespace undeclarations for other namespaces are emitted only when the serialization option undeclare-namespaces
 * is set.</p>
 *
 * <p>The {@code NamespaceDifferencer} is part of the serialization pipeline, responsible for translating result trees
 * to serialized XML. As such, it is not concerned with operations such as namespace fixup and namespace
 * inheritance that are part of the result tree construction process.</p>
 *
 * <p>The {@code NamespaceDifferencer} is also needed when writing output to tree models such as DOM and JDOM
 * that require local namespace declarations to be provided for each element node.</p>
 */
class NamespaceDifferencer(next: Receiver, details: Properties)
  extends ProxyReceiver(next) {

  private val undeclareNamespaces = "yes" == details.getProperty(SaxonOutputKeys.UNDECLARE_PREFIXES)

  private var namespaceStack: List[NamespaceMap] = Nil

  private var currentElement: NodeName = _

  namespaceStack ::= NamespaceMap.emptyMap

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    currentElement = elemName
    val parentMap: NamespaceMap = namespaceStack.head
    namespaceStack ::= namespaces
    val delta: NamespaceMap =
      getDifferences(namespaces, parentMap, currentElement.hasURI(""))
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      delta,
      location,
      properties)
  }

  override def endElement(): Unit = {
    namespaceStack = namespaceStack.tail
    super.endElement()
  }

  private def getDifferences(thisMap: NamespaceMap,
                             parentMap: NamespaceMap,
                             elementInDefaultNamespace: Boolean): NamespaceMap = {
    if (thisMap != parentMap) {
      var delta = NamespaceDeltaMap.emptyMap()
      for (nb <- thisMap.asScala) {
        val parentUri = parentMap.getURI(nb.getPrefix)
        if (parentUri == null) {
          delta = delta.put(nb.getPrefix, nb.getURI)
        } else if (parentUri != nb.getURI) {
          delta = delta.put(nb.getPrefix, nb.getURI)
        }
      }
      if (undeclareNamespaces) {
        for (nb <- parentMap.asScala if thisMap.getURI(nb.getPrefix) == null)
          delta = delta.put(nb.getPrefix, "")
      } else {
        // undeclare the default namespace if the child element is in the default namespace
        if (elementInDefaultNamespace && !parentMap.getDefaultNamespace.isEmpty && thisMap.getDefaultNamespace.isEmpty)
          delta = delta.put("", "")
      }
      return delta
    }
    NamespaceMap.emptyMap
  }
}
