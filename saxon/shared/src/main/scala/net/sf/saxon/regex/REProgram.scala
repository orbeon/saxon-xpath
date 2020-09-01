////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex

import java.util.List

import java.util.function.IntPredicate

import REProgram._




object REProgram {

  val OPT_HASBACKREFS: Int = 1

  val OPT_HASBOL: Int = 2

}

class REProgram /**
  * Constructs a program object from a character array
  * @param operation Array with RE opcode instructions in it. The "next"
  * @param parens       Count of parens in the program
  *                     pointers within the operations must already have been converted to absolute
  *                     offsets.
  * @param flags the regular expression flags
  */
(var operation: Operation, parens: Int, var flags: REFlags) {

// Prefix string optimization
  var prefix: UnicodeString = _

  var initialCharClass: IntPredicate = _

  var preconditions: List[RegexPrecondition] =
    new java.util.ArrayList[RegexPrecondition]()

  var minimumLength: Int = 0

  var fixedLength: Int = -1

// Optimization flags (REProgram.OPT_*)
  var optimizationFlags: Int = _

  var maxParens: Int = parens

  var backtrackingLimit: Int = -1

  /**
    * Sets a new regular expression program to run.  It is this method which
    * performs any special compile-time search optimizations.  Currently only
    * two optimizations are in place - one which checks for backreferences
    * (so that they can be lazily allocated) and another which attempts to
    * find an prefix anchor string so that substantial amounts of input can
    * potentially be skipped without running the actual program.
    *
    * @param operation Program instruction buffer
    */
  private def setOperation(operation: Operation): Unit = {
// Save reference to instruction array
    this.operation = operation
// Initialize other program-related variables
    this.optimizationFlags = 0
    this.prefix = null
    this.operation = operation.optimize(this, flags)
    if (operation.isInstanceOf[Operation.OpSequence]) {
      val first: Operation =
        operation.asInstanceOf[Operation.OpSequence].getOperations.get(0)
      if (first.isInstanceOf[Operation.OpBOL]) {
        optimizationFlags |= REProgram.OPT_HASBOL
      } else if (first.isInstanceOf[Operation.OpAtom]) {
        prefix = first.asInstanceOf[Operation.OpAtom].getAtom
      } else if (first.isInstanceOf[Operation.OpCharClass]) {
        initialCharClass =
          first.asInstanceOf[Operation.OpCharClass].getPredicate
      }
      addPrecondition(operation, -1, 0)
    }
    minimumLength = operation.getMinimumMatchLength
    fixedLength = operation.getMatchLength
  }
// Try various compile-time optimizations
// Try various compile-time optimizations

  def setBacktrackingLimit(limit: Int): Unit = {
    this.backtrackingLimit = limit
  }

  def getBacktrackingLimit: Int = backtrackingLimit

  private def addPrecondition(op: Operation,
                              fixedPosition: Int,
                              minPosition: Int): Unit = {
    if (op.isInstanceOf[Operation.OpAtom] || op
          .isInstanceOf[Operation.OpCharClass]) {
      preconditions.add(new RegexPrecondition(op, fixedPosition, minPosition))
    } else if (op.isInstanceOf[Operation.OpRepeat] && (op
                 .asInstanceOf[Operation.OpRepeat])
                 .min >= 1) {
      val parent: Operation.OpRepeat = op.asInstanceOf[Operation.OpRepeat]
      val child: Operation = parent.op
      if (child.isInstanceOf[Operation.OpAtom] || child
            .isInstanceOf[Operation.OpCharClass]) {
        if (parent.min == 1) {
          preconditions.add(
            new RegexPrecondition(parent, fixedPosition, minPosition))
        } else {
          val parent2: Operation.OpRepeat =
            new Operation.OpRepeat(child, parent.min, parent.min, true)
          preconditions.add(
            new RegexPrecondition(parent2, fixedPosition, minPosition))
        }
      } else {
        addPrecondition(child, fixedPosition, minPosition)
      }
    } else if (op.isInstanceOf[Operation.OpCapture]) {
      addPrecondition(op.asInstanceOf[Operation.OpCapture].childOp,
                      fixedPosition,
                      minPosition)
    } else if (op.isInstanceOf[Operation.OpSequence]) {
      var fp: Int = fixedPosition
      var mp: Int = minPosition
      op.asInstanceOf[Operation.OpSequence].getOperations.forEach { o=>
        if (o.isInstanceOf[Operation.OpBOL]) {
          fp = 0
        }
        addPrecondition(o, fp, mp)
        if (fp != -1 && o.getMatchLength != -1) {
          fp += o.getMatchLength
        } else {
          fp = -1
        }
        mp += o.getMinimumMatchLength
      }
    }
  }

  def isNullable: Boolean = {
    val m: Int = operation.matchesEmptyString()
    (m & Operation.MATCHES_ZLS_ANYWHERE) != 0
  }

  /**
    * Returns a copy of the prefix of current regular expression program
    * in a character array.  If there is no prefix, or there is no program
    * compiled yet, <code>getPrefix</code> will return null.
    *
    * @return A copy of the prefix of current compiled RE program
    */
  def getPrefix: UnicodeString = prefix

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Originally part of Apache's Jakarta project (downloaded January 2012),
 * this file has been extensively modified for integration into Saxon by
 * Michael Kay, Saxonica.
 */

/**
  * A class that holds compiled regular expressions.
  */
