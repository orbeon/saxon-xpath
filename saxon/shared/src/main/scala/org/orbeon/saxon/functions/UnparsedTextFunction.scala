package org.orbeon.saxon.functions

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.Feature

import org.orbeon.saxon.serialize.charcode.UTF16CharacterSet

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.tiny.CharSlice
import scala.util.control.Breaks._
import org.orbeon.saxon.tree.util.CharSequenceConsumer

import org.orbeon.saxon.tree.util.FastStringBuffer

import java.io.IOException

import java.io.Reader

import java.net.URI

import java.nio.charset.CharacterCodingException

import java.nio.charset.MalformedInputException

import java.nio.charset.UnmappableCharacterException

import java.util.function.IntPredicate

object UnparsedTextFunction {

  def readFile(absoluteURI: URI,
               encoding: String,
               output: CharSequenceConsumer,
               context: XPathContext): Unit = {
    val config: Configuration = context.getConfiguration
    val checker: IntPredicate = config.getValidCharacterChecker
    var reader: Reader = null
    try reader = context.getController.getUnparsedTextURIResolver
      .resolve(absoluteURI, encoding, config)
    catch {
      case err: XPathException => {
        err.maybeSetErrorCode("FOUT1170")
        throw err
      }

    }
    try readFile(checker, reader, output)
    catch {
      case encErr: java.io.UnsupportedEncodingException => {
        val e: XPathException =
          new XPathException("Unknown encoding " + Err.wrap(encoding), encErr)
        e.setErrorCode("FOUT1190")
        throw e
      }

      case ioErr: java.io.IOException =>
        throw handleIOError(absoluteURI, ioErr, context)

    }
  }

  def getAbsoluteURI(href: String,
                     baseURI: String,
                     context: XPathContext): URI = {
    var absoluteURI: URI = null
    try absoluteURI = ResolveURI.makeAbsolute(href, baseURI)
    catch {
      case err: java.net.URISyntaxException => {
        val e: XPathException =
          new XPathException(err.getReason + ": " + err.getInput, err)
        e.setErrorCode("FOUT1170")
        throw e
      }

    }
    if (absoluteURI.getFragment != null) {
      val e: XPathException = new XPathException(
        "URI for unparsed-text() must not contain a fragment identifier")
      e.setErrorCode("FOUT1170")
      throw e
    }
    EncodeForUri.checkPercentEncoding(absoluteURI.toString)
    absoluteURI
  }

  def handleIOError(absoluteURI: URI,
                    ioErr: IOException,
                    context: XPathContext): XPathException = {
    var message: String = "Failed to read input file"
    if (absoluteURI != null && ioErr.getMessage != absoluteURI.toString) {
      message += s" ${absoluteURI.toString}"
    }
    message += " (" + ioErr.getClass.getName + ')'
    val e: XPathException = new XPathException(message, ioErr)
    val errorCode: String = getErrorCode(ioErr)
    e.setErrorCode(errorCode)
    e
  }

  private def getErrorCode(ioErr: IOException): String =
    if (ioErr.isInstanceOf[MalformedInputException]) {
      "FOUT1200"
    } else if (ioErr.isInstanceOf[UnmappableCharacterException]) {
      "FOUT1200"
    } else if (ioErr.isInstanceOf[CharacterCodingException]) {
      "FOUT1200"
    } else {
      "FOUT1170"
    }

  def readFile(checker: IntPredicate, reader: Reader): CharSequence = {
    val buffer: FastStringBuffer = new FastStringBuffer(2048)
    readFile(
      checker,
      reader,
      new CharSequenceConsumer() {
        override def cat(chars: CharSequence): CharSequenceConsumer =
          buffer.cat(chars)

        override def cat(c: Char): CharSequenceConsumer = buffer.cat(c)
      }
    )
    buffer.condense()
  }

  def readFile(checker: IntPredicate,
               reader: Reader,
               output: CharSequenceConsumer): Unit = {
    var buffer: Array[Char] = Array.ofDim[Char](2048)
    var first: Boolean = true
    var actual: Int = 0
    var line: Int = 1
    var column: Int = 1
    var latin: Boolean = true
    breakable {
      while (true) {
        actual = reader.read(buffer, 0, buffer.length)
        if (actual < 0) {
          break()
        }
        var c: Int = 0
        while (c < actual) {
          var ch32: Int = buffer({
            c += 1;
            c - 1
          })
          if (ch32 == '\n') {
            line += 1
            column = 0
          }
          column += 1;
          if (ch32 > 255) {
            latin = false
            if (UTF16CharacterSet.isHighSurrogate(ch32)) {
              if (c == actual) {
                val buffer2: Array[Char] = Array.ofDim[Char](2048)
                val actual2: Int = reader.read(buffer2, 0, 2048)
                val buffer3: Array[Char] = Array.ofDim[Char](actual + actual2)
                System.arraycopy(buffer, 0, buffer3, 0, actual)
                System.arraycopy(buffer2, 0, buffer3, actual, actual2)
                buffer = buffer3
                actual = actual + actual2
              }
              val low: Char = buffer({
                c += 1;
                c - 1
              })
              ch32 = UTF16CharacterSet.combinePair(ch32.toChar, low)
            }
          }
          if (!checker.test(ch32)) {
            val err = new XPathException(
              "The text file contains a character that is illegal in XML (line=" +
                line +
                " column=" +
                column +
                " value=hex " +
                java.lang.Integer.toHexString(ch32) +
                ')')
            err.setErrorCode("FOUT1190")
            throw err
          }
        }
        if (first) {
          first = false
          if (buffer(0) == '﻿') {
            output.cat(new CharSlice(buffer, 1, actual - 1))
          } else {
            output.cat(new CharSlice(buffer, 0, actual))
          }
        } else {
          output.cat(new CharSlice(buffer, 0, actual))
        }
      }
    }
    reader.close()
  }

}

abstract class UnparsedTextFunction extends SystemFunction {

  override def getSpecialProperties(arguments: Array[Expression]): Int = {
    val p: Int = super.getSpecialProperties(arguments)
    if (getRetainedStaticContext.getConfiguration.getBooleanProperty(
      Feature.STABLE_UNPARSED_TEXT)) {
      p
    } else {
      p & ~StaticProperty.NO_NODES_NEWLY_CREATED
    }
  }

}
