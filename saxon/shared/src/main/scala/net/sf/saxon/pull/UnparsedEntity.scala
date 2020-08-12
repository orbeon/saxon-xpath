////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pull

import scala.beans.{BeanProperty, BooleanBeanProperty}




class UnparsedEntity {

  @BeanProperty
  var name: String = _

  @BeanProperty
  var systemId: String = _

  @BeanProperty
  var publicId: String = _

  @BeanProperty
  var baseURI: String = _

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is used to represent unparsed entities in the PullProvider interface
  */
