////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.tiny

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.model.Type

import net.sf.saxon.om.AttributeInfo

import net.sf.saxon.trans.XPathException

import net.sf.saxon.z.IntHashMap

import java.util.Arrays

import TinyBuilderCondensed._
import scala.util.control.Breaks._



object TinyBuilderCondensed {

  private def isEqual(a: CharSequence, b: CharSequence): Boolean =
    if (a.getClass == b.getClass) {
      a == b
    } else {
      a.toString == b.toString
    }

}

/**
 * Variant of the TinyBuilder to create a tiny tree in which multiple text nodes or attribute
 * nodes sharing the same string value economize on space by only holding the value once.
 */
class TinyBuilderCondensed(pipeConf: PipelineConfiguration)
  extends TinyBuilder(pipeConf) {

  var textValues: IntHashMap[Array[Int]] = new IntHashMap(100)

  override def endElement(): Unit = {
    // First do the endElement() in the normal way
    val tree: TinyTree = getTree
    super.endElement()
    // This might have created a TEXTUAL_ELEMENT node
    val last: Int = tree.numberOfNodes - 1
    val sameDepth: Boolean = tree.depth(last) == getCurrentDepth
    if (sameDepth) {
      val isTextualElement: Boolean = tree.nodeKind(last) == Type.TEXTUAL_ELEMENT
      // Alternatively, see if there was a final text node
      val hasFinalTextNode: Boolean = tree.nodeKind(last) == Type.TEXT
      if ((isTextualElement || hasFinalTextNode) && (tree.beta(last) <= 256)) {
        // Get the string value of this final node: works both for text nodes and textual element nodes
        val chars: CharSequence = TinyTextImpl.getStringValue(tree, last)
        // don't, the only consequence is that we get less compression)
        val hash: Int = chars.hashCode
        var nodes: Array[Int] = textValues.get(hash)
        if (nodes != null) {
          // We've seen a previous node with this hash value
          val used: Int = nodes(0)
          breakable {
            for (i <- 1 until used) {
              val nodeNr: Int = nodes(i)
              if (nodeNr == 0) {
                break
              } else if (isEqual(chars,
                TinyTextImpl.getStringValue(tree, nodeNr))) {
                // the latest text node is equal to some previous text node
                val length: Int = tree.alpha(last)
                tree.alpha(last) = tree.alpha(nodeNr)
                tree.beta(last) = tree.beta(nodeNr)
                tree.getCharacterBuffer.setLength(length)
                return
              }
            }
          }
        } else {
          // Haven't seen this value before; add an entry to the hash table
          nodes = Array.ofDim[Int](4)
          nodes(0) = 1
          textValues.put(hash, nodes)
        }
        if (nodes(0) + 1 > nodes.length) {
          val n2: Array[Int] = Arrays.copyOf(nodes, nodes.length * 2)
          textValues.put(hash, n2)
          nodes = n2
        }
        // Add this node to the list of distinct nodes with this hash code
        nodes({
          nodes(0) += 1; nodes(0) - 1
        }) = last
      }
      // We rely on all relevant implementations of CharSequence having compatible hashcodes (but if they
      // We rely on all relevant implementations of CharSequence having compatible hashcodes (but if they
    }
  }

  // When ending an element, consider whether the just-completed text node can be commoned-up with
  // any other text nodes. (Don't bother if its more than 256 chars, as it's then likely to be unique)
  // We do this at endElement() time because we need to make sure that adjacent text nodes are concatenated first.
  // When ending an element, consider whether the just-completed text node can be commoned-up with
  // any other text nodes. (Don't bother if its more than 256 chars, as it's then likely to be unique)
  // We do this at endElement() time because we need to make sure that adjacent text nodes are concatenated first.

  override def getAttValue(att: AttributeInfo): String =
    super.getAttValue(att).intern()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
