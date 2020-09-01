////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pull

import javax.xml.transform.Source

import scala.beans.{BeanProperty, BooleanBeanProperty}




class PullSource(private var provider: PullProvider) extends Source {

  @BeanProperty
  var systemId: String = _

  if (provider.getSourceLocator != null) {
    systemId = provider.getSourceLocator.getSystemId
  }

  def getPullProvider: PullProvider = provider

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A PullSource is a JAXP Source that encapsulates a PullProvider - that is, an object
  * that supplies an XML document as a sequence of events that are read under the control
  * of the recipient. Note that although PullSource implements the JAXP Source interface,
  * it is not necessarily acceptable to every JAXP implementation that accepts a Source
  * as input: Source is essentially a marker interface and users of Source objects need
  * to understand the individual implementation.
  */
