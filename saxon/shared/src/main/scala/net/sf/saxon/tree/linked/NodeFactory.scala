////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.linked

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location




trait NodeFactory {

  /*@Nullable*/

  def makeElementNode(parent: NodeInfo,
                      nameCode: NodeName,
                      elementType: SchemaType,
                      isNilled: Boolean,
                      attlist: AttributeMap,
                      namespaces: NamespaceMap,
                      pipe: PipelineConfiguration,
                      locationId: Location,
                      sequenceNumber: Int): ElementImpl

  def makeTextNode(parent: NodeInfo, content: CharSequence): TextImpl

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Interface NodeFactory. <br>
  * A Factory for nodes used to build a tree. <br>
  * Currently only allows Element nodes to be user-constructed.
  *
  * @author Michael H. Kay
  * @version 25 February 2000
  */
