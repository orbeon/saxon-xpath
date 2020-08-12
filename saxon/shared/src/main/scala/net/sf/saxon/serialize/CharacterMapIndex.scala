////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.serialize

import net.sf.saxon.event.Receiver

import net.sf.saxon.lib.SerializerFactory

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trans.XPathException

import java.util._




class CharacterMapIndex extends java.lang.Iterable[CharacterMap] {

  private var index: HashMap[StructuredQName, CharacterMap] = new HashMap(10)

  def getCharacterMap(name: StructuredQName): CharacterMap = index.get(name)

  def putCharacterMap(name: StructuredQName, charMap: CharacterMap): Unit = {
    index.put(name, charMap)
  }

  def iterator(): Iterator[CharacterMap] = index.values.iterator()

  def isEmpty(): Boolean = index.isEmpty

  def copy(): CharacterMapIndex = {
    val copy: CharacterMapIndex = new CharacterMapIndex()
    copy.index = new HashMap(this.index)
    copy
  }

  def makeCharacterMapExpander(useMaps: String,
                               next: Receiver,
                               sf: SerializerFactory): CharacterMapExpander = {
    var characterMapExpander: CharacterMapExpander = null
    val characterMaps: List[CharacterMap] = new ArrayList[CharacterMap](5)
    val st: StringTokenizer = new StringTokenizer(useMaps, " \t\n\r", false)
    while (st.hasMoreTokens()) {
      val expandedName: String = st.nextToken()
      val qName: StructuredQName = StructuredQName.fromClarkName(expandedName)
      val map: CharacterMap = getCharacterMap(qName)
      if (map == null) {
        throw new XPathException(
          "Character map '" + expandedName + "' has not been defined",
          "SEPM0016")
      }
      characterMaps.add(map)
    }
    if (!characterMaps.isEmpty) {
      characterMapExpander = sf.newCharacterMapExpander(next)
      if (characterMaps.size == 1) {
        characterMapExpander.setCharacterMap(characterMaps.get(0))
      } else {
        characterMapExpander.setCharacterMap(new CharacterMap(characterMaps))
      }
    }
    characterMapExpander
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class represents a set of named character maps. Each character map in the set is identified by a unique
  * QName.
  */
