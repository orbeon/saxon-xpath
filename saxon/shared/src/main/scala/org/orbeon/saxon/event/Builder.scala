////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * The abstract Builder class is responsible for taking a stream of SAX events
  * and constructing a Document tree. There is one concrete subclass for each
  * tree implementation.
  *
  */

package org.orbeon.saxon.event

import org.orbeon.saxon.om.{NamePool, NodeInfo}
import org.orbeon.saxon.trans.CommandLineOptions
import org.orbeon.saxon.tree.tiny.TinyDocumentImpl
import org.orbeon.saxon.utils.Configuration


object Builder {

  /**
    * Constant denoting a request for the default tree model
    */ /**
    * Constant denoting a request for the default tree model
    */
  val UNSPECIFIED_TREE_MODEL: Int = -1

  /**
    * Constant denoting the "linked tree" in which each node is represented as an object
    */ /**
    * Constant denoting the "linked tree" in which each node is represented as an object
    */
  val LINKED_TREE: Int = 0

  /**
    * Constant denoting the "tiny tree" in which the tree is represented internally using arrays of integers
    */ /**
    * Constant denoting the "tiny tree" in which the tree is represented internally using arrays of integers
    */
  val TINY_TREE: Int = 1

  /**
    * Constant denoting the "tiny tree condensed", a variant of the tiny tree in which text and attribute nodes
    * sharing the same string value use shared storage for the value.
    */ /**
    * Constant denoting the "tiny tree condensed", a variant of the tiny tree in which text and attribute nodes
    * sharing the same string value use shared storage for the value.
    */
  val TINY_TREE_CONDENSED: Int = 2
  val JDOM_TREE: Int = 3
  val JDOM2_TREE: Int = 4
  val AXIOM_TREE: Int = 5
  val DOMINO_TREE: Int = 6

  /**
    * Constant denoting the "mutable linked tree" in which each node is represented as an object
    */ /**
    * Constant denoting the "mutable linked tree" in which each node is represented as an object
    */
  val MUTABLE_LINKED_TREE: Int = 7

}

abstract class Builder extends Receiver {

  var pipe: PipelineConfiguration = _
  var config: Configuration = _
  var namePool: NamePool = _
  var systemId: String = _
  var baseURI: String = _
  var uniformBaseURI: Boolean = true
  var currentRoot: NodeInfo = _
  var lineNumbering: Boolean = false
  var useEventLocation: Boolean = true
  var started: Boolean = false
  var timing: Boolean = false
  var isOpen: Boolean = false
  private var startTime: Long = _

  def this(pipe: PipelineConfiguration) = {
    this()
    this.pipe = pipe
    config = pipe.getConfiguration
    lineNumbering = config.isLineNumbering
    namePool = config.getNamePool
  }

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
    config = pipe.getConfiguration
    lineNumbering = lineNumbering || config.isLineNumbering
    namePool = config.getNamePool
  }

  /*@NotNull*/
  def getPipelineConfiguration: PipelineConfiguration = pipe

  /*@NotNull*/
  def getConfiguration: Configuration = config

  /*@Nullable*/
  def getBuilderMonitor: BuilderMonitor = null

  def setUseEventLocation(useEventLocation: Boolean): Unit =
    this.useEventLocation = useEventLocation

  def isUseEventLocation: Boolean = useEventLocation

  def setSystemId(systemId: String): Unit =
    this.systemId = systemId

  /*@Nullable*/
  def getSystemId: String = systemId

  def setBaseURI(baseURI: String): Unit =
    this.baseURI = baseURI

  /*@Nullable*/

  def getBaseURI: String = baseURI

  def setLineNumbering(lineNumbering: Boolean): Unit =
    this.lineNumbering = lineNumbering

  def setTiming(on: Boolean): Unit =
    timing = on

  def isTiming: Boolean = timing

  def open(): Unit = {
    if (timing && !isOpen) {
      getConfiguration.getStandardErrorOutput.println(
        "Building tree for " + getSystemId + " using " + getClass)
      startTime = System.nanoTime()
    }
    isOpen = true
  }

  def close(): Unit = {
    if (timing && isOpen) {
      val endTime: Long = System.nanoTime()
      getConfiguration.getStandardErrorOutput.println(
        "Tree built in " +
          CommandLineOptions.showExecutionTimeNano(endTime - startTime))
      currentRoot match {
        case impl: TinyDocumentImpl =>
          impl.showSize()
        case _ =>
      }
      startTime = endTime
    }
    isOpen = false
  }

  override def usesTypeAnnotations: Boolean = true

  /*@Nullable*/
  def getCurrentRoot: NodeInfo = currentRoot

  def reset(): Unit = {
    systemId = null
    baseURI = null
    currentRoot = null
    lineNumbering = false
    started = false
    timing = false
    isOpen = false
  }
}