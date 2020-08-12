////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.utils.Configuration

import net.sf.saxon.trans.XPathException

import javax.xml.transform.Source




trait SourceResolver {

  /*@Nullable*/

  def resolveSource(source: Source, config: Configuration): Source

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This interface defines a SourceResolver. A SourceResolver can be registered as
  * part of the Configuration, and enables new kinds of Source to be recognized
  * beyond those that are natively recognized by Saxon.
  * <p>The task of the SourceResolver is to take any Source as input, and to return
  * a Source that has native support in Saxon: that is, one of the classes
  * StreamSource, SAXSource, DOMSource, {@link net.sf.saxon.om.NodeInfo},
  * or {@link net.sf.saxon.pull.PullSource}</p>
  *
  * @author Michael H. Kay
  */
