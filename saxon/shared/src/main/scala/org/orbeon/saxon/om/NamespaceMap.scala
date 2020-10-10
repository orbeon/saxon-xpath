////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import java.{util => ju}

import org.orbeon.saxon.lib.NamespaceConstant

import scala.util.control.Breaks._

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

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
object NamespaceMap {

  private val emptyArray: Array[String] = Array.empty
  private val EMPTY_MAP: NamespaceMap = new NamespaceMap

  /**
   * Get a namespace map containing no namespace bindings
   *
   * @return an empty namespace map
   */
  def emptyMap: NamespaceMap = EMPTY_MAP

  /**
   * Get a namespace map containing a single namespace binding
   *
   * @param prefix the namespace prefix
   * @param uri    the namespace uri
   * @return a map containing the single binding; or an empty map if the binding is
   *         the standard binding of the XML namespace
   * @throws IllegalArgumentException for an invalid mapping or if the namespace URI is empty
   */
  def of(prefix: String, uri: String): NamespaceMap = {
    val map = new NamespaceMap
    if (map.isPointlessMapping(prefix, uri)) return EMPTY_MAP
    map.prefixes = Array[String](prefix)
    map.uris = Array[String](uri)
    map
  }

  /**
   * Create a NamespaceMap that captures all the information in a given NamespaceResolver
   *
   * @param resolver the NamespaceResolver
   */
  def fromNamespaceResolver(resolver: NamespaceResolver): NamespaceMap = {
    val iter = resolver.iteratePrefixes
    val bindings = new ju.ArrayList[NamespaceBinding]
    while (iter.hasNext) {
      val prefix = iter.next
      val uri = resolver.getURIForPrefix(prefix, useDefault = true)
      bindings.add(new NamespaceBinding(prefix, uri))
    }
    new NamespaceMap(bindings)
  }
}

class NamespaceMap() extends NamespaceBindingSet with NamespaceResolver {

  var prefixes: Array[String] = null // always sorted, for binary search
  var uris: Array[String] = null
  prefixes = NamespaceMap.emptyArray
  uris = NamespaceMap.emptyArray

  def newInstance = new NamespaceMap

  /**
   * Create a namespace map from a list of namespace bindings
   *
   * @param bindings the list of namespace bindings. If there is more that one
   *                 binding for the same prefix, the last one wins. Any binding
   *                 of the prefix "xml" to the XML namespace is ignored, but
   *                 an incorrect binding of the XML namespace causes an exception.
   * @throws IllegalArgumentException if the "xml" prefix is bound to the wrong
   *                                  namespace, or if any other prefix is bound to the XML namespace
   */
  def this(bindings: ju.List[NamespaceBinding]) = {
    this()
    val bindingArray = bindings.toArray(NamespaceBinding.EMPTY_ARRAY)
    bindingArray.sortBy((res: NamespaceBinding) => res.getPrefix)
    var bindsXmlNamespace = false
    prefixes = new Array[String](bindingArray.length)
    uris = new Array[String](bindingArray.length)
    for (i <- 0 until bindingArray.length) {
      prefixes(i) = bindingArray(i).getPrefix
      uris(i) = bindingArray(i).getURI
      if (prefixes(i) == "xml") {
        bindsXmlNamespace = true
        if (!(uris(i) == NamespaceConstant.XML)) throw new IllegalArgumentException("Binds xml prefix to the wrong namespace")
      }
      else if (uris(i) == NamespaceConstant.XML) throw new IllegalArgumentException("Binds xml namespace to the wrong prefix")
    }
    if (bindsXmlNamespace) remove("xml")
  }

  def allowsNamespaceUndeclarations = false

  /**
   * Get the number of entries in the map
   *
   * @return the number of prefix-uri bindings (excluding any binding for the XML namespace)
   */
  def size: Int = prefixes.length

  /**
   * Ask if the map contains only the binding
   *
   * @return true if the map contains no bindings
   */
  def isEmpty: Boolean = prefixes.length == 0

  /**
   * Get the URI associated with a given prefix. If the supplied prefix is "xml", the
   * XML namespace {@link NamespaceConstant#XML} is returned, even if the map is empty.
   *
   * @param prefix the required prefix (may be an empty string to get the default namespace)
   * @return the associated URI, or null if no mapping is present. Note that we return null
   *         when no mapping is present, even for the case where the prefix is the empty string.
   */
  override def getURI(prefix: String): String = {
    if (prefix == "xml") return NamespaceConstant.XML
    val position = ju.Arrays.binarySearch(prefixes.asInstanceOf[Array[AnyRef]], prefix.asInstanceOf[AnyRef])
    if (position >= 0) uris(position)
    else null
  }

  /**
   * Get the default namespace
   *
   * @return the namespace bound to the prefix "" if there is one, otherwise "".
   */
  def getDefaultNamespace: String = { // If the prefix "" is present, it will be the first in alphabetical order
    if (prefixes.length > 0 && prefixes(0).isEmpty) uris(0)
    else ""
  }

  /**
   * Add a new entry to the map, or replace an existing entry. An attempt
   * to add a binding of the "xml" prefix to the XML namespace is silently ignored.
   *
   * @param prefix the prefix whose entry is to be added or replaced. May be zero-length
   *               to represent the default namespace
   * @param uri    the URI to be associated with this prefix; if zero-length, any
   *               existing mapping for the prefix is removed.
   * @return a new map containing the added or replaced entry (or this map, unchanged,
   *         if the prefix-uri mapping was already present in the old map).
   * @throws IllegalArgumentException if an attempt is made to create an incorrect
   *                                  mapping for the "xml" prefix or URI.
   */
  def put(prefix: String, uri: String): NamespaceMap = {
    if (isPointlessMapping(prefix, uri)) return this
    val position = ju.Arrays.binarySearch(prefixes.asInstanceOf[Array[AnyRef]], prefix.asInstanceOf[AnyRef])
    if (position >= 0) { // An entry for this prefix already exists
      if (uris(position) == uri) { // No change
        this
      }
      else if (uri.isEmpty) { // Delete the entry for the prefix
        val n2 = newInstance
        n2.prefixes = new Array[String](prefixes.length - 1)
        System.arraycopy(prefixes, 0, n2.prefixes, 0, position)
        System.arraycopy(prefixes, position + 1, n2.prefixes, position + 1, prefixes.length - position)
        n2.uris = new Array[String](uris.length - 1)
        System.arraycopy(uris, 0, n2.uris, 0, position)
        System.arraycopy(uris, position + 1, n2.uris, position + 1, uris.length - position)
        n2
      }
      else { // Replace the entry for the prefix
        val n2 = newInstance
        n2.prefixes = ju.Arrays.copyOf(prefixes, prefixes.length)
        n2.uris = ju.Arrays.copyOf(uris, uris.length)
        n2.uris(position) = uri
        n2
      }
    }
    else { // No existing entry for the prefix exists
      val insertionPoint = -position - 1
      val p2 = new Array[String](prefixes.length + 1)
      val u2 = new Array[String](uris.length + 1)
      System.arraycopy(prefixes, 0, p2, 0, insertionPoint)
      System.arraycopy(uris, 0, u2, 0, insertionPoint)
      p2(insertionPoint) = prefix
      u2(insertionPoint) = uri
      System.arraycopy(prefixes, insertionPoint, p2, insertionPoint + 1, prefixes.length - insertionPoint)
      System.arraycopy(uris, insertionPoint, u2, insertionPoint + 1, prefixes.length - insertionPoint)
      val n2 = newInstance
      n2.prefixes = p2
      n2.uris = u2
      n2
    }
  }

  private def isPointlessMapping(prefix: String, uri: String): Boolean = {
    if (prefix == "xml") {
      if (!(uri == NamespaceConstant.XML)) throw new IllegalArgumentException("Invalid URI for xml prefix")
      return true
    }
    else if (uri == NamespaceConstant.XML) throw new IllegalArgumentException("Invalid prefix for XML namespace")
    //        if (uri.isEmpty && !allowsNamespaceUndeclarations()) {
    //            throw new IllegalArgumentException("URI must not be zero-length");
    //        }
    false
  }

  /**
   * Add or remove a namespace binding
   *
   * @param prefix the namespace prefix ("" for the default namespace)
   * @param uri    the namespace URI to which the prefix is bound; or "" to indicate that an existing
   *               binding for the prefix is to be removed
   */
  def bind(prefix: String, uri: String): NamespaceMap = if (uri.isEmpty) remove(prefix)
  else put(prefix, uri)

  /**
   * Remove an entry from the map
   *
   * @param prefix the entry to be removed from the map
   * @return a new map in which the relevant entry has been removed, or this map (unchanged)
   *         if the requested entry was not present
   */
  def remove(prefix: String): NamespaceMap = {
    val position = ju.Arrays.binarySearch(prefixes.asInstanceOf[Array[AnyRef]], prefix)
    if (position >= 0) {
      val p2 = new Array[String](prefixes.length - 1)
      val u2 = new Array[String](uris.length - 1)
      System.arraycopy(prefixes, 0, p2, 0, position)
      System.arraycopy(uris, 0, u2, 0, position)
      System.arraycopy(prefixes, position + 1, p2, position, prefixes.length - position - 1)
      System.arraycopy(uris, position + 1, u2, position, uris.length - position - 1)
      val n2 = newInstance
      n2.prefixes = p2
      n2.uris = u2
      n2
    }
    else this
  }

  /**
   * Merge the prefix/uri pairs in the supplied delta with the prefix/uri pairs
   * in this namespace map, to create a new namespace map. If a prefix is present
   * in both maps, then the one in delta takes precedence
   *
   * @param delta prefix/uri pairs to be merged into this map
   * @return a new map, the result of the merge
   */
  def putAll(delta: NamespaceMap): NamespaceMap = if (this eq delta) this
  else if (isEmpty) delta
  else if (delta.isEmpty) this
  else { // Merge of two sorted arrays to produce a sorted array
    val p1 = prefixes
    val u1 = uris
    val p2 = delta.prefixes
    val u2 = delta.uris
    val p3 = new ju.ArrayList[String](p1.length + p2.length)
    val u3 = new ju.ArrayList[String](p1.length + p2.length)
    var i1 = 0
    var i2 = 0
    breakable {
      while (true) {
        val c = p1(i1).compareTo(p2(i2))
        if (c < 0) {
          p3.add(p1(i1))
          u3.add(u1(i1))
          i1 += 1
          if (i1 >= p1.length) break()
        }
        else if (c > 0) {
          p3.add(p2(i2))
          u3.add(u2(i2))
          i2 += 1
          if (i2 >= p2.length) break()
        }
        else {
          p3.add(p2(i2))
          u3.add(u2(i2))
          i1 += 1
          i2 += 1
          if (i1 >= p1.length || i2 >= p2.length) break()
        }
      }
    }
    while (i1 < p1.length) {
      p3.add(p1(i1))
      u3.add(u1(i1))
      i1 += 1
    }
    while ( {
      i2 < p2.length
    }) {
      p3.add(p2(i2))
      u3.add(u2(i2))
      i2 += 1
    }
    val n2 = new NamespaceMap
    n2.prefixes = p3.toArray(Array.empty[String])
    n2.uris = u3.toArray(Array.empty[String])
    n2
  }

  def addAll(namespaces: NamespaceBindingSet): NamespaceMap =
    namespaces match {
    case map: NamespaceMap => putAll(map)
    case _ =>
      var map = this

      for (nb <- namespaces.asScala)
        map = map.put(nb.getPrefix, nb.getURI)

      map
  }

  /**
   * Create a map containing all namespace declarations in this map, plus any namespace
   * declarations and minus any namespace undeclarations in the delta map
   *
   * @param delta a map of namespace declarations and undeclarations to be applied
   * @return a map combining the namespace declarations in this map with the declarations
   *         and undeclarations in the { @code delta} map.
   */
  def applyDifferences(delta: NamespaceDeltaMap): NamespaceMap = if (delta.isEmpty) this
  else {
    val p1 = this.prefixes
    val u1 = this.uris
    val p2 = delta.prefixes
    val u2 = delta.uris
    val prefixes = new ju.ArrayList[String](p1.length + p2.length)
    val uris = new ju.ArrayList[String](p1.length + p2.length)
    var i1 = 0
    var i2 = 0
    while (i1 < p1.length && i2 < p2.length) {
      val c = p1(i1).compareTo(p2(i2))
      if (c < 0) {
        prefixes.add(p1(i1))
        uris.add(u1(i1))
        i1 += 1
      }
      else if (c > 0) {
        if (!u2(i2).isEmpty) {
          prefixes.add(p2(i2))
          uris.add(u2(i2))
        }
        i2 += 1
      }
      else {
        if (!u2(i2).isEmpty) {
          prefixes.add(p2(i2))
          uris.add(u2(i2))
        }
        i1 += 1
        i2 += 1
      }
    }
    while ( {
      i1 < p1.length
    }) {
      prefixes.add(p1(i1))
      uris.add(u1(i1))
      i1 += 1
    }
    while ( {
      i2 < p2.length
    }) {
      if (!u2(i2).isEmpty) {
        prefixes.add(p2(i2))
        uris.add(u2(i2))
      }
      i2 += 1
    }
    val n2 = new NamespaceMap
    n2.prefixes = prefixes.toArray(Array[String]())
    n2.uris = uris.toArray(Array[String]())
    n2
  }

  /**
   * Get an iterator over the namespace bindings defined in this namespace map
   *
   * @return an iterator over the namespace bindings. (In the current implementation
   *         they will be in alphabetical order of namespace prefix.)
   */
  override def iterator: ju.Iterator[NamespaceBinding] = new ju.Iterator[NamespaceBinding]() {
    private[om] var i = 0

    def hasNext: Boolean =
      i < prefixes.length

    def next(): NamespaceBinding = {
      val nb = new NamespaceBinding(prefixes(i), uris(i))
      i += 1
      nb
    }

    override def remove(): Unit =
      throw new UnsupportedOperationException("remove")
  }

  /**
   * Get all the namespace bindings defined in this namespace map as an array
   *
   * @return the array of namespace bindings
   */
  def getNamespaceBindings: Array[NamespaceBinding] = {
    val result = new Array[NamespaceBinding](prefixes.length)
    for (i <- prefixes.indices) {
      result(i) = new NamespaceBinding(prefixes(i), uris(i))
    }
    result
  }

  /**
   * Get the differences between this NamespaceMap and another NamespaceMap, as an array
   * of namespace declarations and undeclarations
   *
   * @param other             typically the namespaces on the parent element, in which case the method
   *                          returns the namespace declarations and undeclarations corresponding to the
   *                          difference between this child element and its parent.
   * @param addUndeclarations if true, then when a namespace is declared in the { @code other}
   *                          map but not in { @code this} map, a namespace undeclaration
   *                          (binding the prefix to the dummy URI "") will be included
   *                          in the result. If false, namespace undeclarations are included
   *                          in the result only for the default namespace (prefix = "").
   */
  def getDifferences(other: NamespaceMap, addUndeclarations: Boolean): Array[NamespaceBinding] = {
    val result = new ju.ArrayList[NamespaceBinding]
    var i = 0
    var j = 0
    while (true) { // Merge and combine the two sorted lists of prefix/uri pairs
      if (i < prefixes.length && j < other.prefixes.length) {
        val c = prefixes(i).compareTo(other.prefixes(j))
        if (c < 0) { // prefix in this namespace map, absent from other
          result.add(new NamespaceBinding(prefixes(i), uris(i)))
          i += 1
        }
        else if (c == 0) { // prefix present in both maps
          if (uris(i) == other.uris(j)) {
            // URI is the same; this declaration is redundant, so omit it from the result
          }
          else { // URI is different; use the URI appearing in this map in preference
            result.add(new NamespaceBinding(prefixes(i), uris(i)))
          }
          i += 1
          j += 1
        }
        else { // prefix present in other map, absent from this: maybe add an undeclaration
          if (addUndeclarations || prefixes(i).isEmpty) result.add(new NamespaceBinding(other.prefixes(j), ""))
          j += 1
        }
      }
      else if (i < prefixes.length) {
        result.add(new NamespaceBinding(prefixes(i), uris(i)))
        i += 1
      }
      else if (j < other.prefixes.length) { // prefix present in other map, absent from this: add an undeclaration
        result.add(new NamespaceBinding(other.prefixes(j), ""))
        j += 1
      }
      else return result.toArray(NamespaceBinding.EMPTY_ARRAY)
    }
    null
  }

  /**
   * Get the namespace URI corresponding to a given prefix. Return null
   * if the prefix is not in scope.
   *
   * @param prefix     the namespace prefix. May be the zero-length string, indicating
   *                   that there is no prefix. This indicates either the default namespace or the
   *                   null namespace, depending on the value of useDefault. The prefix "xml" is always
   *                   recognized as corresponding to the XML namespace { @link NamespaceConstant#XML}
   * @param useDefault true if the default namespace is to be used when the
   *                   prefix is "". If false, the method returns "" when the prefix is "". The default
   *                   namespace is a property of the NamespaceResolver; in general it corresponds to
   *                   the "default namespace for elements and types", but that cannot be assumed.
   * @return the uri for the namespace, or null if the prefix is not in scope.
   *         The "null namespace" is represented by the pseudo-URI "".
   */
  def getURIForPrefix(prefix: String, useDefault: Boolean): String = {
    if (prefix == "xml") return NamespaceConstant.XML
    if (prefix == "") if (useDefault) return getDefaultNamespace
    else return ""
    getURI(prefix)
  }

  /**
   * Get an iterator over the prefixes defined in this namespace map, including
   * the "xml" prefix.
   *
   * @return an iterator over the prefixes. (In the current implementation
   *         they will be in alphabetical order, except that the "xml" prefix will always come last.)
   */
  def iteratePrefixes: ju.Iterator[String] = {
    // ORBEON: Original was making a copy.
    (prefixes.iterator ++ Iterator("xml")).asJava
  }

  /**
   * Get the prefixes present in the NamespaceMap, as an array, excluding the "xml" prefix
   *
   * @return the prefixes present in the map, not including the "xml" prefix
   */
  def getPrefixArray: Array[String] = prefixes

  def getURIsAsArray: Array[String] = uris

  override def toString: String = {
    val sb = new StringBuilder

    for (nb <- this.asScala) {
      sb.append(nb.getPrefix).append("=").append(nb.getURI).append(" ")
    }
    sb.toString
  }

  override def hashCode: Int =
    ju.Arrays.hashCode(prefixes.asInstanceOf[Array[AnyRef]]) ^ ju.Arrays.hashCode(uris.asInstanceOf[Array[AnyRef]])

  override def equals(obj: Any): Boolean = {
    obj match {
      case o: NamespaceMap if this eq o => true
      case o: NamespaceMap =>
        ju.Arrays.equals(prefixes.asInstanceOf[Array[AnyRef]], o.prefixes.asInstanceOf[Array[AnyRef]]) &&
        ju.Arrays.equals(uris.asInstanceOf[Array[AnyRef]],     o.uris.asInstanceOf[Array[AnyRef]])
      case _ => false
    }
  }
}