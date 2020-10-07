////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.trans.SymbolicName

import scala.beans.{BeanProperty, BooleanBeanProperty}




class ComponentBinding(name: SymbolicName, @BeanProperty var target: Component) {

  @BeanProperty
  var symbolicName: SymbolicName = name

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A ComponentBinding is a reference from one component to another; for example a variable
  * reference or function call. ComponentBindings are held in the binding vector of the calling
  * component, and are separate from the instruction/expression that contains the reference, to provide
  * a level of indirection; this means that when a component is re-bound in a using package, for example
  * to call overriding versions of templates or functions called from the component, the compiled code of
  * the calling component does not need to be changed, only the contents of the binding vector.
  *
  * <p>The ComponentBinding class is immutable.</p>
  */
