////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A LastPositionFinder is an interface implemented by any SequenceIterator that is
  * able to return the position of the last item in the sequence.
  */

package org.orbeon.saxon.expr


@FunctionalInterface
trait LastPositionFinder {
  def getLength(): Int // keep `()`
}
