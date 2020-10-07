////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.value.SequenceExtent

import javax.xml.transform.Result

import java.util.ArrayList

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}




class RawResult extends Result {

  @BeanProperty
  var systemId: String = _

  private var content: List[Item] = new ArrayList[Item]()

  def append(item: Item): Unit = {
    content.add(item)
  }

  def getResultSequence: Sequence = new SequenceExtent(content)

}

// Copyright (c) 2017-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is an implementation of the JAXP Result interface. It can be used to indicate that
  * the output of a transformation (either the principal result, or a secondary result) should be
  * delivered in "raw" form, that is, without building a tree (equivalently, without performing
  * "sequence normalization"). Once output has been written to a RawResult, it is available to
  * the caller in the form of a {@link Sequence}.
  *
  * @author Michael H. Kay
  */
