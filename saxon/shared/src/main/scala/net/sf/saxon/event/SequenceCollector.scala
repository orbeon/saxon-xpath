////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.utils.Controller

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.s9api.HostLanguage

import net.sf.saxon.tree.iter.EmptyIterator

import net.sf.saxon.tree.iter.ListIterator

import net.sf.saxon.value.EmptySequence

import net.sf.saxon.value.SequenceExtent

import java.util.ArrayList

import java.util.List

import SequenceCollector._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object SequenceCollector {

  /*@Nullable*/

  def allocateSequenceOutputter(context: XPathContext,
                                hostLang: HostLanguage.HostLanguage): Outputter = {
    val controller: Controller = context.getController
    val pipe: PipelineConfiguration = controller.makePipelineConfiguration
    pipe.setHostLanguage(hostLang)
    val collector: SequenceCollector = new SequenceCollector(pipe, 20)
    new ComplexContentOutputter(collector)
  }

}

class SequenceCollector(pipe: PipelineConfiguration, estimatedSize: Int)
    extends SequenceWriter(pipe) {

  @BeanProperty
  var list: List[Item] = new ArrayList(estimatedSize)

  def this(pipe: PipelineConfiguration) = this(pipe, 50)

  def reset(): Unit = {
    list = new ArrayList(Math.min(list.size + 10, 50))
  }

  def write(item: Item): Unit = {
    list.add(item)
  }

  def getSequence: Sequence = list.size match {
    case 0 => EmptySequence.getInstance
    case 1 => //noinspection unchecked
      list.get(0)
    case _ => new SequenceExtent(list)

  }

  def iterate(): SequenceIterator =
    if (list.isEmpty) {
      EmptyIterator.emptyIterator
    } else {
      new ListIterator(list)
    }

  def getFirstItem: Item =
    if (list.isEmpty) {
      null
    } else {
      list.get(0)
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This receiver is used when writing a sequence of atomic values and nodes, that
  * is, when xsl:variable is used with content and an "as" attribute. The receiver
  * builds the sequence and provides access to it. Note that the event sequence can include calls such as
  * startElement and endElement that require trees to be built. If nodes such as attributes
  * and text nodes are received while an element is being constructed, the nodes are added
  * to the tree. Otherwise, "orphan" nodes (nodes with no parent) are created and added
  * directly to the sequence.
  */
