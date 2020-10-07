////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize.codenorm

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.z.IntHashMap

import org.orbeon.saxon.z.IntToIntMap

import java.util.BitSet

import NormalizerData._




object NormalizerData {

  val copyright: String = "Copyright (c) 1998-1999 Unicode, Inc."

  /**
    * Constant for use in getPairwiseComposition
    */ /**
    * Constant for use in getPairwiseComposition
    */
  val NOT_COMPOSITE: Int = '￿'

}

/**
  * Accesses the Normalization Data used for Forms C and D.
  * <p>Copyright (c) 1998-1999 Unicode, Inc. All Rights Reserved.<br>
  * The Unicode Consortium makes no expressed or implied warranty of any
  * kind, and assumes no liability for errors or omissions.
  * No liability is assumed for incidental and consequential damages
  * in connection with or arising out of the use of the information here.</p>
  *
  * @author Mark Davis
  */
class NormalizerData /**
  * Only accessed by NormalizerBuilder.
  */
(private var canonicalClass: IntToIntMap,
 private var decompose: IntHashMap[_],
 private var compose: IntToIntMap,
 private var isCompatibility: BitSet,
 private var isExcluded: BitSet) {

  /**
    * Gets the combining class of a character from the
    * Unicode Character Database.
    *
    * @param ch the source character
    * @return value from 0 to 255
    */
  def getCanonicalClass(ch: Int): Int = canonicalClass.get(ch)

  /**
    * Returns the composite of the two characters. If the two
    * characters don't combine, returns NOT_COMPOSITE.
    * Only has to worry about BMP characters, since those are the only ones that can ever compose.
    *
    * @param first  first character (e.g. 'c')
    * @param second second character (e.g. '�' cedilla)
    * @return composite (e.g. '�')
    */
  def getPairwiseComposition(first: Int, second: Int): Char = {
    if (first < 0 || first > 0x10FFFF || second < 0 || second > 0x10FFFF)
      return NOT_COMPOSITE.toChar
    compose.get((first << 16) | second).toChar
  }

  /**
    * Gets recursive decomposition of a character from the
    * Unicode Character Database.
    *
    * @param canonical If true
    *                  bit is on in this byte, then selects the recursive
    *                  canonical decomposition, otherwise selects
    *                  the recursive compatibility and canonical decomposition.
    * @param ch        the source character
    * @param buffer    buffer to be filled with the decomposition
    */
  def getRecursiveDecomposition(canonical: Boolean,
                                ch: Int,
                                buffer: FastStringBuffer): Unit = {
    val decomp: String = decompose.get(ch).asInstanceOf[String]
    if (decomp != null && !(canonical && isCompatibility.get(ch))) {
      for (i <- 0 until decomp.length) {
        getRecursiveDecomposition(canonical, decomp.charAt(i), buffer)
      }
    } else {
      // if no decomp, append
      buffer.appendWideChar(ch)
    }
  }

  /**
    * Just accessible for testing.
    */
  def getExcluded(ch: Char): Boolean = isExcluded.get(ch)

  /*@NotNull*/

  def getRawDecompositionMapping(ch: Char): String =
    decompose.get(ch).asInstanceOf[String]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// * The class is derived from the sample program NormalizerData.java published by the
// * Unicode consortium. That code has been modified so that instead of building the run-time
// * data structures directly, they are written to a Java "source" module, which is then
// * compiled. Also, the ability to construct a condensed version of the data tables has been
// * removed.
