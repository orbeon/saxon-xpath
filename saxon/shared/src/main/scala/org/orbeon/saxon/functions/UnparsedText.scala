package org.orbeon.saxon.functions

import java.net.URI
import java.{util => ju}

import org.orbeon.saxon.event.{Outputter, ReceiverOption}
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.functions.UnparsedText._
import org.orbeon.saxon.lib.Feature
import org.orbeon.saxon.om.{Sequence, ZeroOrOne}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.utils.Controller
import org.orbeon.saxon.value.StringValue

object UnparsedText {

  private val errorValue: String = ""

  def evalUnparsedText(hrefVal: StringValue,
                       base: String,
                       encoding: String,
                       context: XPathContext): StringValue = {
    var content: CharSequence = null
    var result: StringValue = null
    val stable: Boolean =
      context.getConfiguration.getBooleanProperty(Feature.STABLE_UNPARSED_TEXT)
    try {
      if (hrefVal == null)
        return null
      val href: String = hrefVal.getStringValue
      val absoluteURI: URI = UnparsedTextFunction.getAbsoluteURI(href, base, context)
      if (stable) {
        val controller: Controller = context.getController
        controller.synchronized {
          var cache: ju.Map[URI, String] = controller
            .getUserData("unparsed-text-cache", "")
            .asInstanceOf[ju.Map[URI, String]]
          if (cache != null) {
            val existing: String = cache.get(absoluteURI)
            if (existing != null) {
              if (existing.startsWith(errorValue))
                throw new XPathException(existing.substring(1), "FOUT1170")
              return new StringValue(existing)
            }
          }
          var error: XPathException = null
          try {
            val consumer: StringValue.Builder = new StringValue.Builder()
            UnparsedTextFunction readFile(absoluteURI, encoding, consumer, context)
            content = consumer.getStringValue.getStringValueCS
          } catch {
            case e: XPathException =>
              error = e
              content = errorValue + e.getMessage
          }
          if (cache == null) {
            cache = new ju.HashMap
            controller.setUserData("unparsed-text-cache", "", cache)
            cache.put(absoluteURI, content.toString)
          }
          if (error != null) {
            throw error
          }
        }
      } else {
        val consumer: StringValue.Builder = new StringValue.Builder()
        UnparsedTextFunction.readFile(absoluteURI, encoding, consumer, context)
        return consumer.getStringValue
      }
      result = new StringValue(content)
    } catch {
      case err: XPathException =>
        err.maybeSetErrorCode("FOUT1170")
        throw err
    }
    result
  }

  // ORBEON: No `File` support.
//  def main(args: Array[String]): Unit = {
//    var sb1: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
//    var sb2: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C256)
//    val file: File = new File(args(0))
//    val is: InputStream = new FileInputStream(file)
//    breakable {
//      while (true) {
//        val b: Int = is.read()
//        if (b < 0) {
//          println(sb1)
//          println(sb2)
//          break()
//        }
//        sb1.append(java.lang.Integer.toHexString(b) + " ")
//        sb2.append(s"${b.toChar} ")
//        if (sb1.length > 80) {
//          println(sb1)
//          println(sb2)
//          sb1 = new FastStringBuffer(FastStringBuffer.C256)
//          sb2 = new FastStringBuffer(FastStringBuffer.C256)
//        }
//      }
//    }
//    is.close()
//  }
}

class UnparsedText extends UnparsedTextFunction with PushableFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[StringValue] = {

    val hrefVal = arguments(0).head.asInstanceOf[StringValue]
    val encoding = if (getArity == 2) arguments(1).head.getStringValue else null

    try
      new ZeroOrOne(evalUnparsedText(hrefVal, getStaticBaseUriString, encoding, context))
    catch {
      case e: XPathException =>
        if (getArity == 2 && e.getErrorCodeLocalPart.==("FOUT1200"))
          e.setErrorCode("FOUT1190")
        throw e
    }
  }

  override def process(destination: Outputter,
                       context: XPathContext,
                       arguments: Array[Sequence]): Unit = {
    val stable = context.getConfiguration.getBooleanProperty(Feature.STABLE_UNPARSED_TEXT)
    if (stable) {
      val result = call(context, arguments)
      val value = result.head
      if (value != null)
        destination.append(value, Loc.NONE, ReceiverOption.NONE)
    } else {
      val href = arguments(0).head.asInstanceOf[StringValue]
      val absoluteURI =
        UnparsedTextFunction.getAbsoluteURI(href.getStringValue, getStaticBaseUriString, context)
      val encoding =
        if (getArity == 2)
          arguments(1).head.getStringValue
        else
          null
      val consumer = destination.getStringReceiver(false)
      consumer.open()
      try {
        UnparsedTextFunction.readFile(absoluteURI, encoding, consumer, context)
        consumer.close()
      } catch {
        case e: XPathException =>
          if (getArity == 2 && e.getErrorCodeLocalPart.==("FOUT1200"))
            e.setErrorCode("FOUT1190")
          throw e
      }
    }
  }
}
