////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex

import net.sf.saxon.utils.Configuration

import net.sf.saxon.lib.ParseOptions

import net.sf.saxon.lib.Validation

import net.sf.saxon.model.Type

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.pattern.NameTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.z.IntArraySet

import net.sf.saxon.z.IntHashMap

import net.sf.saxon.z.IntToIntHashMap

import net.sf.saxon.z.IntToIntMap

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
