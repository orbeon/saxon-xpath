////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.trans.XPathException

import javax.xml.transform.Source

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * An implementation of the JAXP Source class that supplies a document in the form of a stream
  * of push events sent to a Receiver
  *
  * @since 9.1
  */
abstract class EventSource extends Source {

  @BeanProperty
  var systemId: String = _

  def send(out: Receiver): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
