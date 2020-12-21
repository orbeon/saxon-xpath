////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.util

import java.util.Arrays

import org.orbeon.saxon.event.ReceiverOption
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.model.{BuiltInAtomicType, SimpleType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.utils.Configuration
import org.xml.sax.Attributes


/**
 * AttributeCollectionImpl is an implementation of the SAX2 interface Attributes.
 * <p>As well as providing the information required by the SAX2 interface, an
 * AttributeCollection can hold type information (as needed to support the JAXP 1.3
 * {@link javax.xml.validation.ValidatorHandler} interface), and location information
 * for debugging. The location information is used in the case of attributes on a result
 * tree to identify the location in the query or stylesheet from which they were
 * generated.</p>
 */
class AttributeCollectionImpl(private var config: Configuration,
                              initialSize: Int)
  extends Attributes {

  // case of an empty attribute collection.
  private var names: Array[NodeName] = new Array[NodeName](initialSize)
  private var values: Array[String] = new Array[String](initialSize)
  private var locations: Array[Location] = new Array[Location](initialSize)
  private var props: Array[Int] = new Array[Int](initialSize)
  private var used: Int = 0

  // the types array can be null even if used>0; this indicates that all attributes are untyped
  private var types: Array[SimpleType] = null

  def addAttribute(nodeName: NodeName,
                   `type`: SimpleType,
                   value: String,
                   locationId: Location,
                   properties: Int): Unit = {
    if (values == null) {
      names = Array.ofDim[NodeName](5)
      values = Array.ofDim[String](5)
      props = Array.ofDim[Int](5)
      locations = Array.ofDim[Location](5)
      if (`type` != BuiltInAtomicType.UNTYPED_ATOMIC) {
        types = Array.ofDim[SimpleType](5)
      }
      used = 0
    }
    if (values.length == used) {
      val newsize: Int = if (used == 0) 5 else used * 2
      names = Arrays.copyOf(names, newsize)
      values = Arrays.copyOf(values, newsize)
      props = Arrays.copyOf(props, newsize)
      locations = Arrays.copyOf(locations, newsize)
      if (types != null) {
        types = Arrays.copyOf(types, newsize)
      }
    }
    val n: Int = used
    names(n) = nodeName
    props(n) = properties
    locations(n) = locationId.saveLocation
    setTypeAnnotation(n, `type`)
    values({
      used += 1; used - 1
    }) = value
  }

  def setAttribute(index: Int,
                   nodeName: NodeName,
                   `type`: SimpleType,
                   value: String,
                   locationId: Location,
                   properties: Int): Unit = {
    names(index) = nodeName
    props(index) = properties
    locations(index) = locationId
    setTypeAnnotation(index, `type`)
    values(index) = value
  }

  def getLength: Int = if (values == null) 0 else used

  def getTypeAnnotation(index: Int): SimpleType = {
    if (types == null)
      return BuiltInAtomicType.UNTYPED_ATOMIC
    if (index < 0 || index >= used)
      return BuiltInAtomicType.UNTYPED_ATOMIC
    types(index)
  }

  def getLocation(index: Int): Location = {
    if (locations == null)
      return Loc.NONE
    if (index < 0 || index >= used)
      return Loc.NONE
    locations(index)
  }

  def getProperties(index: Int): Int = {
    if (props == null)
      return ReceiverOption.NONE
    if (index < 0 || index >= used)
      return ReceiverOption.NONE
    props(index)
  }

  /*@Nullable*/

  def getQName(index: Int): String = {
    if (names == null)
      return null
    if (index < 0 || index >= used)
      return null
    names(index).getDisplayName
  }

  /*@Nullable*/

  def getLocalName(index: Int): String = {
    if (names == null)
      return null
    if (index < 0 || index >= used)
      return null
    names(index).getLocalPart
  }

  /*@Nullable*/

  def getURI(index: Int): String = {
    if (names == null)
      return null
    if (index < 0 || index >= used)
      return null
    names(index).getURI
  }

  /*@NotNull*/

  def getType(index: Int): String = {
    val typeCode: Int = getTypeAnnotation(index).getFingerprint
    typeCode match {
      case StandardNames.XS_ID => "ID"
      case StandardNames.XS_IDREF => "IDREF"
      case StandardNames.XS_NMTOKEN => "NMTOKEN"
      case StandardNames.XS_ENTITY => "ENTITY"
      case StandardNames.XS_IDREFS => "IDREFS"
      case StandardNames.XS_NMTOKENS => "NMTOKENS"
      case StandardNames.XS_ENTITIES => "ENTITIES"
      case _ => "CDATA"
    }
  }

  /*@Nullable*/

  def getType(uri: String, localname: String): String = {
    val index: Int = findByName(uri, localname)
    if (index < 0) null else getType(index)
  }

  /*@Nullable*/

  def getValue(index: Int): String = {
    if (values == null)
      return null
    if (index < 0 || index >= used)
      return null
    values(index)
  }

  /*@Nullable*/

  def getValue(uri: String, localname: String): String = {
    val index: Int = findByName(uri, localname)
    if (index < 0) null else getValue(index)
  }

  def getIndex(qname: String): Int = {
    if (names == null)
      return -1
    if (qname.indexOf(':') < 0)
      return findByName("", qname)
    // Searching using prefix+localname is not recommended, but SAX allows it...
    var parts: Array[String] = null
    try parts = NameChecker.getQNameParts(qname)
    catch {
      case _: QNameException =>
        return -1
    }
    val prefix: String = parts(0)
    if (prefix.isEmpty) {
      return findByName("", qname)
    } else {
      val localName: String = parts(1)
      for (i <- 0 until used if names(i) != null) {
        val lname: String = names(i).getLocalPart
        val ppref: String = names(i).getPrefix
        if (localName == lname && prefix == ppref) {
          return i
        }
      }
      -1
    }
  }

  def getIndex(uri: String, localname: String): Int =
    findByName(uri, localname)

  /*@NotNull*/

  def getType(name: String): String = {
    val index: Int = getIndex(name)
    getType(index)
  }

  /*@Nullable*/

  def getValue(name: String): String = {
    val index: Int = getIndex(name)
    getValue(index)
  }

  private def findByName(uri: String, localName: String): Int = {
    if (names == null || config == null) {
      // indicates an empty attribute set
      return -1
    }
    for (i <- 0 until used
         if names(i) != null && names(i).hasURI(uri) && localName == names(i).getLocalPart) {
      return i
    }
    -1
  }

  def setTypeAnnotation(index: Int, `type`: SimpleType): Unit = {
    if (`type` == BuiltInAtomicType.UNTYPED_ATOMIC) {
      if (types != null) {
        types(index) = `type`
      }
    } else {
      if (types == null) {
        types = Array.ofDim[SimpleType](names.length)
        Arrays.fill(types.asInstanceOf[Array[AnyRef]], BuiltInAtomicType.UNTYPED_ATOMIC)
        types(index) = `type`
      } else {
        types(index) = `type`
      }
    }
  }

  def setAttribute(attribute: AttributeInfo): Unit = {
    val name: NodeName = attribute.getNodeName
    val index: Int = getIndex(name.getURI, name.getLocalPart)
    if (index < 0) {
      addAttribute(name,
        attribute.getType,
        attribute.getValue,
        attribute.getLocation,
        attribute.getProperties)
    } else {
      setAttribute(index,
        name,
        attribute.getType,
        attribute.getValue,
        attribute.getLocation,
        attribute.getProperties)
    }
  }
}
