////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.util


/**
  * This class (which has one instance per Configuration) is used to allocate unique document
  * numbers. It's a separate class so that it can act as a monitor for synchronization
  */
class DocumentNumberAllocator {

  // Changed to a long in Saxon 9.4, because a user reported an int overflowing
  // on a system that had been in live operation for several months. The effect wasn't fatal,
  // but could cause incorrect node identity tests.
  private var nextDocumentNumber: Long = 0

  // Negative document numbers are used for streamed documents. This means that streamed
  // nodes always precede unstreamed nodes in document order. We take advantage of this
  // when sorting a sequence that contains both streamed and unstreamed nodes.
  private var nextStreamedDocumentNumber: Long = -2 // -1 is special

  def allocateDocumentNumber(): Long = synchronized {
    val r = nextDocumentNumber
    nextDocumentNumber += 1
    r
  }

  def allocateStreamedDocumentNumber(): Long = synchronized {
    val r = nextStreamedDocumentNumber
    nextStreamedDocumentNumber -= 1
    r
  }
}
