////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.tiny

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * Statistics on the size of TinyTree instances, kept so that the system can learn how much space to allocate to new trees
  */
class Statistics {

  private var treesCreated: Int = 5

  @BeanProperty
  var averageNodes: Double = 4000.0

  @BeanProperty
  var averageAttributes: Double = 100.0

  @BeanProperty
  var averageNamespaces: Double = 20.0

  @BeanProperty
  var averageCharacters: Double = 4000.0

  def this(nodes: Int, atts: Int, namespaces: Int, chars: Int) = {
    this()
    this.averageNodes = nodes
    this.averageAttributes = atts
    this.averageNamespaces = namespaces
    this.averageCharacters = chars
  }

  def updateStatistics(numberOfNodes: Int,
                       numberOfAttributes: Int,
                       numberOfNamespaces: Int,
                       chars: Int): Unit = {
    synchronized {
      val n0: Int = treesCreated
      if (n0 < 1000000) {
// it should have stabilized by then, and we don't want to overflow
        val n1: Int = treesCreated + 1
        treesCreated = n1
        averageNodes = ((averageNodes * n0) + numberOfNodes) / n1
        if (averageNodes < 10.0) {
          averageNodes = 10.0
        }
        averageAttributes = ((averageAttributes * n0) + numberOfAttributes) / n1
        if (averageAttributes < 10.0) {
          averageAttributes = 10.0
        }
        averageNamespaces = ((averageNamespaces * n0) + numberOfNamespaces) / n1
        if (averageNamespaces < 5.0) {
          averageNamespaces = 5.0
        }
        averageCharacters = ((averageCharacters * n0) + chars) / n1
        if (averageCharacters < 100.0) {
          averageCharacters = 100.0
        }
      }
    }
  }

  override def toString(): String =
    treesCreated.toString + "(" + averageNodes.toString + "," + averageAttributes.toString +
      "," +
      averageNamespaces.toString +
      "," +
      averageCharacters.toString +
      ")"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
