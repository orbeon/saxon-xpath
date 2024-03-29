////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.event.{Builder, PipelineConfiguration}
import org.orbeon.saxon.tree.linked.LinkedTreeBuilder
import org.orbeon.saxon.tree.tiny.{TinyBuilder, TinyBuilderCondensed}


/**
  * A TreeModel represents an implementation of the Saxon NodeInfo interface, which itself
  * is essentially an implementation of the XDM model defined in W3C specifications (except
  * that Saxon's NodeInfo understands the 13 XPath axes, rather than merely supporting
  * parent and child properties).
 *
  * This class serves two purposes: it acts as a factory for obtaining a Builder which
  * can be used to build trees using this tree model; and it provides static constants
  * that can be used to identify the built-in tree models.
  */
object TreeModel {

  /**
    * The TinyTree implementation. This is normally the default implementation
    * of the tree model.
    */
  val TINY_TREE: TreeModel = new TinyTree

  /**
    * The CondensedTinyTree implementation. This is a variant of the TinyTree that
    * uses less memory but takes a little longer to build. Run-time performance
    * is the same as the TinyTree.
    */
  val TINY_TREE_CONDENSED: TreeModel = new TinyTreeCondensed

  /**
    * The LinkedTree. This takes more memory than the TinyTree, but offers flexibility
    * for storing user-defined data in each element node; it is also mutable, supporting
    * XQuery Update.
    */
  val LINKED_TREE: TreeModel = new LinkedTree

  def getTreeModel(symbolicValue: Int): TreeModel = symbolicValue match {
    case Builder.TINY_TREE           => TreeModel.TINY_TREE
    case Builder.TINY_TREE_CONDENSED => TreeModel.TINY_TREE_CONDENSED
    case Builder.LINKED_TREE         => TreeModel.LINKED_TREE
    case _                           => throw new IllegalArgumentException("tree model " + symbolicValue)
  }

  private class TinyTree extends TreeModel {

    def makeBuilder(pipe: PipelineConfiguration): Builder = {
      val builder = new TinyBuilder(pipe)
      builder.setStatistics(pipe.getConfiguration.getTreeStatistics.SOURCE_DOCUMENT_STATISTICS)
      builder
    }

    override def getSymbolicValue: Int = Builder.TINY_TREE
    override def isSchemaAware: Boolean = true
    override def getName: String = "TinyTree"
  }

  private class TinyTreeCondensed extends TreeModel {

    def makeBuilder(pipe: PipelineConfiguration): Builder = {
      val tbc = new TinyBuilderCondensed(pipe)
      tbc.setStatistics(pipe.getConfiguration.getTreeStatistics.SOURCE_DOCUMENT_STATISTICS)
      tbc
    }

    override def getSymbolicValue: Int = Builder.TINY_TREE_CONDENSED
    override def isSchemaAware: Boolean = true
    override def getName: String = "TinyTreeCondensed"
  }

  private class LinkedTree extends TreeModel {

    /*@NotNull*/
    def makeBuilder(pipe: PipelineConfiguration): Builder =
      new LinkedTreeBuilder(pipe)

    override def getSymbolicValue: Int = Builder.LINKED_TREE
    override def isSchemaAware: Boolean = true
    override def getName: String = "LinkedTree"
  }
}

abstract class TreeModel {
  def makeBuilder(pipe: PipelineConfiguration): Builder

  def getSymbolicValue: Int     = Builder.UNSPECIFIED_TREE_MODEL
  def isMutable       : Boolean = false
  def isSchemaAware   : Boolean = false
  def getName         : String  = toString
}
