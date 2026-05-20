////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.event.ReceiverOption
import org.orbeon.saxon.model.{MissingComponentException, SimpleType}
import org.orbeon.saxon.s9api.Location

/**

  * This class contains immutable information about an attribute. An `AttributeInfo` is not a node:
  * it has no identity and no navigation capability to the parent element or to any other nodes on the tree.
  */
object AttributeInfo {

  class Deleted(att: AttributeInfo)
    extends AttributeInfo(
      att.nodeName,
      att.`type`,
      att.value,
      att.location,
      att.properties
    )
}

class AttributeInfo(
  var nodeName   : NodeName, // xxx can we use `val`
  var `type`     : SimpleType,
  var value      : String,
  var location   : Location,
  var properties : Int
) {

  def isId: Boolean =
    try
      StandardNames.XML_ID_NAME == nodeName ||
        ReceiverOption.contains(properties, ReceiverOption.IS_ID) ||
        `type`.isIdType
    catch {
      case _: MissingComponentException => false
    }

  def withNodeName(newName: NodeName): AttributeInfo =
    new AttributeInfo(newName, `type`, value, location, properties)
}

