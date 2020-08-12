////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.s9api

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.TeeOutputter

import net.sf.saxon.serialize.SerializationProperties




class TeeDestination(private var dest0: Destination,
                     private var dest1: Destination)
    extends AbstractDestination {

  /*@NotNull*/

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver =
    new TeeOutputter(dest0.getReceiver(pipe, params),
                     dest1.getReceiver(pipe, params))

  def close(): Unit = {
    dest0.close()
    dest1.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A TeeDestination allows writing to two destinations at once. For example the output of a transformation
  * can be written simultaneously to a Serializer and to a second Transformation. By chaining together a number
  * of TeeDestinations it is possible to write to any number of destinations at once.
  *
  * @since 9.1
  */
