////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.util




/**
  * This class (which has one instance per Configuration) is used to allocate unique document
  * numbers. It's a separate class so that it can act as a monitor for synchronization
  */
class DocumentNumberAllocator {

  private var nextDocumentNumber: Long = 0

// -1 is special
  private var nextStreamedDocumentNumber: Long = -2

  def allocateDocumentNumber(): Long = synchronized {
    { nextDocumentNumber += 1; nextDocumentNumber - 1 }
  }

  def allocateStreamedDocumentNumber(): Long = synchronized {
    { nextStreamedDocumentNumber -= 1; nextStreamedDocumentNumber + 1 }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
