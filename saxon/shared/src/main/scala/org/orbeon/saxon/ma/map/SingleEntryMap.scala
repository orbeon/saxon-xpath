////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.map

import org.orbeon.saxon.model.{AtomicType, ItemType, TypeHierarchy, UType}
import org.orbeon.saxon.om.{GroundedValue, SequenceTool}
import org.orbeon.saxon.tree.iter.{AtomicIterator, SingleAtomicIterator}
import org.orbeon.saxon.tree.jiter.MonoIterator
import org.orbeon.saxon.value.{AtomicValue, SequenceType}


class SingleEntryMap(var key: AtomicValue, var value: GroundedValue)
  extends MapItem {

  /**
   * Get an entry from the Map
   *
   * @param key the value of the key
   * @return the value associated with the given key, or null if the key is not present in the map.
   */
  override def get(key: AtomicValue): GroundedValue =
    if (this.key.asMapKey() == key.asMapKey()) value else null

  /**
   * Get the size of the map
   *
   * @return the number of keys/entries present in this map
   */
  override def size(): Int = 1

  /**
   * Ask whether the map is empty
   *
   * @return true if and only if the size of the map is zero
   */
  override def isEmpty: Boolean = false

  /**
   * Get the set of all key values in the map.
   *
   * @return a set containing all the key values present in the map, in unpredictable order
   */
  override def keys(): AtomicIterator[AtomicValue] = new SingleAtomicIterator(key)

  /**
   * Get the set of all key-value pairs in the map
   *
   * @return an iterable containing all the key-value pairs
   */
  override def keyValuePairs(): java.lang.Iterable[KeyValuePair] = new MonoIterator(new KeyValuePair(key, value)).asInstanceOf[java.lang.Iterable[KeyValuePair]]

  /**
   * Create a new map containing the existing entries in the map plus an additional entry,
   * without modifying the original. If there is already an entry with the specified key,
   * this entry is replaced by the new entry.
   *
   * @param key   the key of the new entry
   * @param value the value associated with the new entry
   * @return the new map containing the additional entry
   */
  override def addEntry(key: AtomicValue, value: GroundedValue): MapItem =
    toHashTrieMap.addEntry(key, value)

  /**
   * Remove an entry from the map
   *
   * @param key the key of the entry to be removed
   * @return a new map in which the requested entry has been removed; or this map
   *         unchanged if the specified key was not present
   */
  override def remove(key: AtomicValue): MapItem =
    if ((get(key) == null)) this else new HashTrieMap()

  /**
   * Ask whether the map conforms to a given map type
   *
   * @param keyType   the required keyType
   * @param valueType the required valueType
   * @param th        the type hierarchy cache for the configuration
   * @return true if the map conforms to the required type
   */
  override def conforms(keyType: AtomicType,
                        valueType: SequenceType,
                        th: TypeHierarchy): Boolean =
    keyType.matches(key, th) && valueType.matches(value, th)

  /**
   * Get the type of the map. This method is used largely for diagnostics, to report
   * the type of a map when it differs from the required type.
   *
   * @param th the type hierarchy cache
   * @return the type of this map
   */
  override def getItemType(th: TypeHierarchy): ItemType =
    new MapType(
      key.getItemType,
      SequenceType.makeSequenceType(SequenceTool.getItemType(value, th),
        SequenceTool.getCardinality(value)))

  /**
   * Get the lowest common item type of the keys in the map
   *
   * @return the most specific type to which all the keys belong. If the map is
   *         empty, return UType.VOID
   */
  override def getKeyUType(): UType = key.getUType

  private def toHashTrieMap: HashTrieMap = {
    val target: HashTrieMap = new HashTrieMap()
    target.initialPut(key, value)
    target
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A key and a corresponding value to be held in a Map. A key-value pair also acts as a singleton
 * map in its own right.
 */
// Copyright (c) 2010-2020 Saxonica Limited
