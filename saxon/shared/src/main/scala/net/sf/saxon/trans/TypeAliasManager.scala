////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans

import net.sf.saxon.model.ItemType

import net.sf.saxon.om.StructuredQName

//import net.sf.saxon.style.ComponentDeclaration

import java.util.List




/**
  * Manager for saxon:type-alias declarations in a stylesheet.
  *
  * Saxon extension introduced in Saxon 9.8
  *
  * This is a dummy version for Saxon-HE: the feature requires Saxon-PE or higher
  */
class TypeAliasManager {

  def registerTypeAlias(name: StructuredQName, `type`: ItemType): Unit = {
    throw new UnsupportedOperationException()
  }

 /* def processDeclaration(declaration: ComponentDeclaration): Unit = {
    throw new UnsupportedOperationException()
  }

  def processAllDeclarations(topLevel: List[ComponentDeclaration]): Unit = ()*/
// No action
// No action

  def getItemType(alias: StructuredQName): ItemType = null

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
