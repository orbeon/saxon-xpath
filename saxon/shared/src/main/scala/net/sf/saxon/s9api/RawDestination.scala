////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.s9api

import net.sf.saxon.event.CloseNotifier

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.SequenceCollector

import net.sf.saxon.serialize.SerializationProperties

import net.sf.saxon.trans.XPathException




class RawDestination extends AbstractDestination {

  private var sequenceOutputter: SequenceCollector = _

  private var closed: Boolean = false

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver = {
// method.
    sequenceOutputter = new SequenceCollector(pipe)
    closed = false
    helper.onClose(() => closed = true)
    new CloseNotifier(sequenceOutputter, helper.getListeners)
  }
// The Receiver returned by this method is a SequenceOutputter. The SequenceOutputter
// builds a list of all top-level items passed to it. A top-level document or element
// node can be passed as a sequence of events, in which case a ComplexContentOutputter
// is created to build the tree represented by these events; the root document or element
// node in this tree is then added to the same list as a composed items. On completion
// the sequence represented by the list of items is available by calling the getXmlValue()
// The Receiver returned by this method is a SequenceOutputter. The SequenceOutputter
// builds a list of all top-level items passed to it. A top-level document or element
// node can be passed as a sequence of events, in which case a ComplexContentOutputter
// is created to build the tree represented by these events; the root document or element
// node in this tree is then added to the same list as a composed items. On completion
// the sequence represented by the list of items is available by calling the getXmlValue()

  def close(): Unit = {
    sequenceOutputter.close()
    closed = true
  }

  def getXdmValue(): XdmValue = {
    if (!closed) {
      throw new IllegalStateException(
        "The result sequence has not yet been closed")
    }
    XdmValue.wrap(sequenceOutputter.getSequence)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An <code>RawDestination</code> is a {@link Destination} that accepts a sequence output
  * by a stylesheet or query and returns it directly as an {@link XdmValue}, without
  * constructing an XML tree, and without serialization. It corresponds to the serialization
  * option <code>build-tree="no"</code>
  */
