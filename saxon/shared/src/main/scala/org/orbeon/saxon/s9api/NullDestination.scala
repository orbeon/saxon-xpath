////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.Sink

import org.orbeon.saxon.serialize.SerializationProperties




/**
  * A NullDestination is a Destination that discards all output sent to it.
  * @since 9.9
  */
class NullDestination extends AbstractDestination {

  override def getReceiver(pipe: PipelineConfiguration,
                           params: SerializationProperties): Receiver =
    new Sink(pipe)

  override def close(): Unit = ()

}

// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
