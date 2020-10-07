////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om._

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException




class NamePoolConverter(next: Receiver,
                        var oldPool: NamePool,
                        var newPool: NamePool)
    extends ProxyReceiver(next) {

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    checkType(`type`)
    val fp: Int =
      newPool.allocateFingerprint(elemName.getURI, elemName.getLocalPart)
    val newElemName: CodedName = new CodedName(fp, elemName.getPrefix, newPool)
    var newAtts: AttributeMap = EmptyAttributeMap.getInstance
    for (att <- attributes) {
      checkType(att.getType)
      val afp: Int = newPool.allocateFingerprint(att.getNodeName.getURI,
                                                 att.getNodeName.getLocalPart)
      val newAttName: NodeName =
        new CodedName(afp, att.getNodeName.getPrefix, newPool)
      newAtts = newAtts.put(
        new AttributeInfo(newAttName,
                          att.getType,
                          att.getValue,
                          att.getLocation,
                          att.getProperties))
    }
    nextReceiver.startElement(newElemName,
                              `type`,
                              newAtts,
                              namespaces,
                              location,
                              properties)
  }

  private def checkType(`type`: SchemaType): Unit = {
    if ((`type`.getFingerprint & NamePool.USER_DEFINED_MASK) !=
          0) {
      throw new UnsupportedOperationException(
        "Cannot convert a user-typed node to a different name pool")
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is a filter that passes all Receiver events through unchanged,
  * except that it changes namecodes to allow for the source and the destination
  * using different NamePools. This is necessary when a stylesheet has been constructed
  * as a general document (e.g. as the result of a transformation) and is passed to
  * newTemplates() to be compiled as a stylesheet.
  *
  * <p>The type annotations of nodes passed through this filter must be built-in types
  * in the XSD namespace, because user-defined types belong to a specific Configuration
  * and cannot readily be transferred. In practice the class is used only for untyped trees.</p>
  */
