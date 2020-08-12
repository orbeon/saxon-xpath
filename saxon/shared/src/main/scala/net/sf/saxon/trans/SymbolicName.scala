////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans

import net.sf.saxon.om.StandardNames
import net.sf.saxon.om.StructuredQName

/**
 * The symbolic name of a component consists of the component kind (e.g. function, template, attribute set),
 * the QName acting as the name of the template, function etc, and in the case of functions, the arity
 */
object SymbolicName {

  class F(val structName: StructuredQName, var arity: Int) extends SymbolicName(StandardNames.XSL_FUNCTION, structName) {
    /**
     * Get the arity, in the case of function components
     *
     * @return in the case of a function, the arity, otherwise -1.
     */
    def getArity: Int = arity

    /**
     * Returns a hash code value for the object.
     */
    override def hashCode: Int = super.hashCode ^ arity

    /**
     * Indicates whether some other object is "equal to" this one.
     */
    override def equals(obj: Any): Boolean = obj.isInstanceOf[SymbolicName.F] && super.equals(obj) && obj.asInstanceOf[SymbolicName.F].arity == this.arity

    /**
     * Get the name as a string.
     *
     * @return a string typically in the form "template p:my-template" or "function f:my-function#2"
     */
    override def toString : String= { //noinspection UnnecessaryParentheses
      super.toString + "#" + arity
    }

    /**
     * Get a short name suitable for use in messages
     */
    override def getShortName : String= super.getShortName + "#" + arity
  }

}


/**
 * Create a symbolic name for a component other than a function.
 *
 * @param kind the component kind, for example { @link StandardNames#XSL_TEMPLATE}
 * @param name the QName that is the "name" of the component
 */
class SymbolicName(var kind: Int, var name: StructuredQName){
  /**
   * Returns a hash code value for the object.
   */
  override def hashCode = kind << 16 ^ name.hashCode

  /**
   * Indicates whether some other object is "equal to" this one.
   */
  override def equals(obj: Any) = obj.isInstanceOf[SymbolicName] && obj.asInstanceOf[SymbolicName].kind == this.kind && obj.asInstanceOf[SymbolicName].name == this.name

  /**
   * Get the kind of component, for example {@link StandardNames#XSL_FUNCTION} or {@link StandardNames#XSL_VARIABLE}
   *
   * @return the kind of component identified by this symbolic name
   */
  def getComponentKind = kind

  /**
   * Get the QName part of the symbolic name of the component
   *
   * @return the QName part of the name
   */
  def getComponentName = name

  /**
   * Get the name as a string.
   *
   * @return a string typically in the form "template p:my-template" or "function f:my-function#2"
   */
  override def toString = StandardNames.getLocalName(kind) + " " + name.getDisplayName

  /**
   * Get a short name suitable for use in messages
   */
  def getShortName = name.getDisplayName
}