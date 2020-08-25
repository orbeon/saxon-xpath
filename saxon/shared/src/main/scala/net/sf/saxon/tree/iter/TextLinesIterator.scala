////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.iter

import net.sf.saxon.functions.UnparsedTextFunction

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.s9api.Location

import net.sf.saxon.serialize.charcode.UTF16CharacterSet

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.StringValue

import java.io.IOException

import java.io.LineNumberReader

import java.net.URI

import java.util.function.IntPredicate




/**
  * An iterator that iterates over a file line by line, returning each line as a {@link StringValue}
  */
abstract class TextLinesIterator  () extends SequenceIterator {

   var reader: LineNumberReader = _

   var checker: IntPredicate = _

  var current: StringValue = null

  var position: Int = 0

   var location: Location = _

   var uri: URI = _

  def this(reader: LineNumberReader,
           location: Location,
           uri: URI,
           checker: IntPredicate) = {
    this()
    this.reader = reader
    this.location = location
    this.uri = uri
    this.checker = checker
  }

  /*@Nullable*/

  def next(): StringValue = {
    if (position < 0) {
// input already exhausted
      close()
      return null
    }
    try {
      var s: String = reader.readLine()
      if (s == null) {
        current = null
        position = -1
        close()
        return null
      }
      if (position == 0 && s.startsWith("﻿")) {
// remove any BOM found at start of file
        s = s.substring(1)
      }
      checkLine(checker, s)
      current = new StringValue(s)
      position += 1
      current
    } catch {
      case err: IOException => {
        close()
        val e: XPathException =
          UnparsedTextFunction.handleIOError(uri, err, null)
        if (location != null) {
          e.setLocator(location)
        }
        throw e
      }

    }
  }

  override def close(): Unit = {
    try reader.close()
    catch {
      case err: IOException => {}

    }
  }

  private def checkLine(checker: IntPredicate, buffer: String): Unit = {
    var c: Int = 0
    while (c < buffer.length) {
      var ch32: Int = buffer.charAt({ c += 1; c - 1 })
      if (UTF16CharacterSet.isHighSurrogate(ch32)) {
        val low: Char = buffer.charAt({ c += 1; c - 1 })
        ch32 = UTF16CharacterSet.combinePair(ch32.toChar, low)
      }
      if (!checker.test(ch32)) {
        val err: XPathException = new XPathException(
          "The unparsed-text file contains a character that is illegal in XML (line=" +
            position +
            " column=" +
            (c + 1) +
            " value=hex " +
            java.lang.Integer.toHexString(ch32) +
            ')')
        err.setErrorCode("FOUT1190")
        err.setLocator(location)
        throw err
      }
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
