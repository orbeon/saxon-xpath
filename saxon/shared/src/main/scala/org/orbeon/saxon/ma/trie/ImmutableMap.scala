////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.ma.trie

import java.util.Iterator




trait ImmutableMap[K, V] extends java.lang.Iterable[Tuple2[K, V]] {

  /**
    * Add a new entry to the map. If an entry already exists with the
    * given key, the returned map will contain this entry, but not the
    * existing entry.
    *
    * @param key   the key to use to retrieve this item
    * @param value the value stored for this item
    * @return a new map with this item added
    */
  def put(key: K, value: V): ImmutableMap[K, V]

  /**
    * Remove an entry from the map. If no entry exists with the given
    * key, the returned map will have identical contents to the original
    * map (and may, in fact, be the original map itself).
    *
    * @param key the key for the entry to remove
    * @return a new map with the entry with the given key removed (or
    *         a map with the original contents if no entry was found
    *         for the given key).
    */
  def remove(key: K): ImmutableMap[K, V]

  /**
    * Retrieve a stored value from the map based on the key for the
    * associated entry. If no entry exists with the given key, we
    * return None.
    *
    * @param key the key for the entry to retrieve
    * @return Some(value) if an entry exists with the given key, or
    *         None if no entry with the given key was found.
    */
  def get(key: K): V

  def iterator: Iterator[Tuple2[K, V]]

}

// Copyright (c) 2012 Michael Froh.
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Original author: Michael Froh (published on Github). Released under MPL 2.0
// by Saxonica Limited with permission from the author
