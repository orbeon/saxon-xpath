////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.lib.ParseOptions

import org.orbeon.saxon.lib.Validation

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.AxisInfo

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.pattern.NameTest

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.AxisIterator

import org.orbeon.saxon.z.IntArraySet

import org.orbeon.saxon.z.IntHashMap

import org.orbeon.saxon.z.IntToIntHashMap

import org.orbeon.saxon.z.IntToIntMap

import javax.xml.transform.stream.StreamSource

import java.io.InputStream

import java.util.ArrayList

import scala.util.control.Breaks._

object CaseVariants {

  private var monoVariants: IntToIntMap = null

  private var polyVariants: IntHashMap[Array[Int]] = null

  def build(): Unit = {
    monoVariants = new IntToIntHashMap(2500)
    polyVariants = new IntHashMap(100)
    for (entry <- UnicodeData.caseVariantsData.split(";")) {
      val colon = entry.indexOf(':')
      val icode = java.lang.Integer.parseInt(entry.substring(0, colon), 16)
      val vhex = entry.substring(colon + 1).split(",")
      val vint = Array.ofDim[Int](vhex.length)
      for (i <- vhex.indices)
        vint(i) = java.lang.Integer.parseInt(vhex(i), 16)
      if (vhex.length == 1)
        monoVariants.put(icode, vint(0))
      else
        polyVariants.put(icode, vint)
    }
  }

  def getCaseVariants(code: Int): Array[Int] = synchronized {
    if (monoVariants == null) {
      build()
    }
    val mono: Int = monoVariants.get(code)
    if (mono != monoVariants.defaultValue) {
      Array(mono)
    } else {
      val result: Array[Int] = polyVariants.get(code)
      if (result == null) {
        IntArraySet.EMPTY_INT_ARRAY
      } else {
        result
      }
    }
  }

  /*@NotNull*/

  var ROMAN_VARIANTS: Array[Int] = Array(0x0130, 0x0131, 0x212A, 0x017F)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
