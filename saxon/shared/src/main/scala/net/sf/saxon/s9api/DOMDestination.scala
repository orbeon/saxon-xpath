////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.s9api

import net.sf.saxon.dom.DOMWriter

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.Receiver

import net.sf.saxon.serialize.SerializationProperties




class DOMDestination(root: org.w3c.dom.Node) extends AbstractDestination {

  private var domWriter: DOMWriter = new DOMWriter()

  domWriter.setNode(root)

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver = {
    domWriter.setPipelineConfiguration(pipe)
    params.makeSequenceNormalizer(domWriter)
  }

  def close(): Unit = {}
// no action
// no action

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class represents a Destination (for example, the destination of the output of a transformation)
  * in which the results are written to a newly constructed DOM tree in memory. The caller must supply
  * a Document node, which will be used as the root of the constructed tree
  */
