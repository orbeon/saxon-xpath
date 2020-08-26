package net.sf.saxon.serialize

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.lib.SaxonOutputKeys

import net.sf.saxon.ma.json.JsonReceiver

import net.sf.saxon.serialize.charcode.CharacterSet

import net.sf.saxon.serialize.codenorm.Normalizer

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.IntegerValue

import net.sf.saxon.value.NumericValue

import javax.xml.transform.OutputKeys

import javax.xml.transform.stream.StreamResult

import java.io.IOException

import java.io.Writer

import java.util.Properties

import java.util.Stack

import scala.beans.{BeanProperty}

class JSONEmitter(pipe: PipelineConfiguration,
                  sResult: StreamResult,
                  var outputProperties: Properties) {

  private var result: ExpandedStreamResult = new ExpandedStreamResult(pipe.getConfiguration, sResult, outputProperties)

  private var writer: Writer = _

  private var normalizer: Normalizer = _

  private var characterMap: CharacterMap = _

  private var characterSet: CharacterSet = _

  private var isIndenting: Boolean = _

  private var indentSpaces: Int = 2

  private var maxLineLength: Int = _

  private var first: Boolean = true

  private var afterKey: Boolean = false

  private var level: Int = _

  private var oneLinerStack: Stack[Boolean] = new Stack()

  private var unfailing: Boolean = false

  def setOutputProperties(details: Properties): Unit = {
    this.outputProperties = details
    if ("yes" == details.getProperty(OutputKeys.INDENT)) {
      isIndenting = true
    }
    if ("yes" == details.getProperty(SaxonOutputKeys.UNFAILING)) {
      unfailing = true
    }
    val max: String = details.getProperty(SaxonOutputKeys.LINE_LENGTH)
    if (max != null) {
      try maxLineLength = java.lang.Integer.parseInt(max)
      catch {
        case err: NumberFormatException => {}

      }
    }
    val spaces: String = details.getProperty(SaxonOutputKeys.INDENT_SPACES)
    if (spaces != null) {
      try indentSpaces = java.lang.Integer.parseInt(spaces)
      catch {
        case err: NumberFormatException => {}

      }
    }
  }

  def getOutputProperties: Properties = outputProperties

  def setNormalizer(normalizer: Normalizer): Unit = {
    this.normalizer = normalizer
  }

  def setCharacterMap(map: CharacterMap): Unit = {
    this.characterMap = map
  }

  def writeKey(key: String): Unit = {
    conditionalComma(false)
    emit('"')
    emit(escape(key))
    emit("\":")
    if (isIndenting) {
      emit(" ")
    }
    afterKey = true
  }

  def writeAtomicValue(item: AtomicValue): Unit = {
    conditionalComma(false)
    if (item == null) {
      emit("null")
    } else if (item.isInstanceOf[NumericValue]) {
      val num: NumericValue = item.asInstanceOf[NumericValue]
      if (num.isNaN) {
        if (unfailing) {
          emit("NaN")
        } else {
          throw new XPathException("JSON has no way of representing NaN",
            "SERE0020")
        }
      } else if (java.lang.Double.isInfinite(num.getDoubleValue)) {
        if (unfailing) {
          emit(if (num.getDoubleValue < 0) "-INF" else "INF")
        } else {
          throw new XPathException("JSON has no way of representing Infinity",
            "SERE0020")
        }
      } else if (item.isInstanceOf[IntegerValue]) {
        emit(num.longValue().toString + "")
      } else if (num.isWholeNumber && !num.isNegativeZero && num
        .abs()
        .compareTo(1000000000000000000L) < 0) {
        emit(num.longValue().toString + "")
      } else {
        emit(num.getStringValue)
      }
    } else if (item.isInstanceOf[BooleanValue]) {
      emit(item.getStringValue)
    } else {
      emit('"')
      emit(escape(item.getStringValue))
      emit('"')
    }
  }

  def startArray(oneLiner: Boolean): Unit = {
    emitOpen('[', oneLiner)
    level += 1
  }

  def endArray(): Unit = {
    emitClose(']', {
      level -= 1;
      level + 1
    })
  }

  def startMap(oneLiner: Boolean): Unit = {
    emitOpen('{', oneLiner)
    level += 1
  }

  def endMap(): Unit = {
    emitClose('}', {
      level -= 1;
      level + 1
    })
  }

  private def emitOpen(bracket: Char, oneLiner: Boolean): Unit = {
    conditionalComma(true)
    oneLinerStack.push(oneLiner)
    emit(bracket)
    first = true
    if (isIndenting) {
      if (oneLiner) {
        emit(' ')
      } else {}
    }
  }

  private def emitClose(bracket: Char, level: Int): Unit = {
    val oneLiner: Boolean = oneLinerStack.pop()
    if (isIndenting) {
      if (oneLiner) {
        emit(' ')
      } else {
        indent(level)
      }
    }
    emit(bracket)
    first = false
  }

  private def conditionalComma(opening: Boolean): Unit = {
    val wasFirst: Boolean = first
    if (first) {
      first = false
    } else if (!afterKey) {
      emit(',')
    }
    if (wasFirst && afterKey) {
      emit(' ')
    } else if (isIndenting && !afterKey && level != 0) {
      emit('\n')
      for (i <- 0 until indentSpaces * (level + 1)) {
        emit(' ')
      }
    }
    afterKey = false
  }

  private def indent(level: Int): Unit = {
    emit('\n')
    for (i <- 0 until indentSpaces * level) {
      emit(' ')
    }
  }

  private def escape(cs: CharSequence): CharSequence =
    if (characterMap != null) {
      val out: FastStringBuffer = new FastStringBuffer(cs.length)
      var chSeq = cs
      chSeq = characterMap.map(chSeq, insertNulls = true)
      val s: String = chSeq.toString
      var prev: Int = 0
      while (true) {
        val start: Int = s.indexOf(0, prev)
        if (start >= 0) {
          out.cat(simpleEscape(s.substring(prev, start)))
          val end: Int = s.indexOf(0, start + 1)
          out.append(s.substring(start + 1, end))
          prev = end + 1
        } else {
          out.cat(simpleEscape(s.substring(prev)))
          out
        }
      }
      ""
    } else {
      simpleEscape(cs)
    }

  private def simpleEscape(cs: CharSequence): CharSequence = {
    var chSeq = cs
    if (normalizer != null) {
      chSeq = normalizer.normalize(chSeq)
    }
    JsonReceiver.escape(
      chSeq,
      forXml = false,
      (c) => c < 31 || (c >= 127 && c <= 159) || !characterSet.inCharset(c))
  }

  private def emit(s: CharSequence): Unit = {
    if (writer == null) {
      writer = result.obtainWriter()
      characterSet = result.getCharacterSet
    }
    writer.append(s)
  }

  private def emit(c: Char): Unit = {
    emit(c.toString + "")
  }

  def close(): Unit = {
    if (first) {
      emit("null")
    }
    if (writer != null) {
      try writer.close()
      catch {
        case e: IOException => {}

      }
    }
  }

}
