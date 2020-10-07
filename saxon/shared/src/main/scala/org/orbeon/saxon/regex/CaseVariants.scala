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
    val in: InputStream = Configuration.locateResource("casevariants.xml",
      new ArrayList(),
      new ArrayList())
    if (in == null) {
      throw new RuntimeException("Unable to read casevariants.xml file")
    }
    val config: Configuration = new Configuration()
    val options: ParseOptions = new ParseOptions()
    options.setSchemaValidationMode(Validation.SKIP)
    options.setDTDValidationMode(Validation.SKIP)
    var doc: NodeInfo = null
    doc = config
      .buildDocumentTree(new StreamSource(in, "casevariants.xml"), options)
      .getRootNode
    val iter: AxisIterator = doc.iterateAxis(
      AxisInfo.DESCENDANT,
      new NameTest(Type.ELEMENT, "", "c", config.getNamePool))
    breakable {
      while (true) {
        val item: NodeInfo = iter.next()
        if (item == null) {
          break()
        }
        val code: String = item.getAttributeValue("", "n")
        val icode: Int = java.lang.Integer.parseInt(code, 16)
        val variants: String = item.getAttributeValue("", "v")
        val vhex: Array[String] = variants.split(",")
        val vint: Array[Int] = Array.ofDim[Int](vhex.length)
        for (i <- 0 until vhex.length) {
          vint(i) = java.lang.Integer.parseInt(vhex(i), 16)
        }
        if (vhex.length == 1) {
          monoVariants.put(icode, vint(0))
        } else {
          polyVariants.put(icode, vint)
        }
      }
    }
  }

  def getCaseVariants(code: Int): Array[Int] = synchronized {
    if (monoVariants == null) {
      build()
    }
    val mono: Int = monoVariants.get(code)
    if (mono != monoVariants.getDefaultValue) {
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
