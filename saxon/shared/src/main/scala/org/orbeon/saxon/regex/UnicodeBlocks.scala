////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex

import java.util.{ArrayList, HashMap, Map}

import javax.xml.transform.stream.StreamSource
import org.orbeon.saxon.lib.{ParseOptions, Validation}
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.{AllElementsSpaceStrippingRule, AxisInfo, NodeInfo}
import org.orbeon.saxon.pattern.{NameTest, NodeKindTest}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.z.{IntBlockSet, IntSet}

import scala.util.control.Breaks._


object UnicodeBlocks {

  private var blocks: Map[String, IntSet] = null

  def getBlock(name: String): IntSet = {
    if (blocks == null)
      readBlocks(new Configuration)
    var cc = blocks.get(name)
    if (cc != null)
      return cc
    cc = blocks.get(normalizeBlockName(name))
    cc
  }

  private def normalizeBlockName(name: String): String = {
    val fsb = new FastStringBuffer(name.length)
    for (i <- 0 until name.length) {
      val c = name.charAt(i)
      c match {
        case ' ' | '\t' | '\r' | '\n' | '_' => // no action
        case _ => fsb.cat(c)
      }
    }
    fsb.toString
  }

  private def readBlocks(config: Configuration): Unit = {
    synchronized {
      blocks = new HashMap(250)
      val in = Configuration.locateResource("unicodeBlocks.xml", new ArrayList)
      if (in == null)
        throw new RESyntaxException("Unable to read unicodeBlocks.xml file")
      val options = new ParseOptions
      options.setSchemaValidationMode(Validation.SKIP)
      options.setDTDValidationMode(Validation.SKIP)
      options.setSpaceStrippingRule(AllElementsSpaceStrippingRule.getInstance)
      var doc = config.buildDocumentTree(new StreamSource(in, "unicodeBlocks.xml"), options)
      val iter = doc.getRootNode.iterateAxis(
        AxisInfo.DESCENDANT,
        new NameTest(Type.ELEMENT, "", "block", config.getNamePool))
      breakable {
        while (true) {
          val item: NodeInfo = iter.next()
          if (item == null)
            break()
          val blockName: String =
            normalizeBlockName(item.getAttributeValue("", "name"))
          var range: IntSet = null
          for (rangeElement <- item.children(NodeKindTest.ELEMENT)) {
            val from = java.lang.Integer.parseInt(
              rangeElement.getAttributeValue("", "from").substring(2),
              16)
            val to = java.lang.Integer.parseInt(
              rangeElement.getAttributeValue("", "to").substring(2),
              16)
            val cr = new IntBlockSet(from, to)
            range =
              if (range == null)
                cr
              else if (range.isInstanceOf[IntBlockSet])
                range.mutableCopy().union(cr)
              else
                range.union(cr)
          }
          blocks.put(blockName, range)
        }
      }
    }
  }
}
