////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trans

import org.orbeon.saxon.expr.Expression
import org.orbeon.saxon.expr.instruct.Instruction
import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om._
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.Whitespace


/**
  * Class containing utility methods for handling error messages
  */
object Err {

  val ELEMENT  : Int = 1
  val ATTRIBUTE: Int = 2
  val FUNCTION : Int = 3
  val VALUE    : Int = 4
  val VARIABLE : Int = 5
  val GENERAL  : Int = 6
  val URI      : Int = 7
  val EQNAME   : Int = 8

  /**
    * Add delimiters to represent variable information within an error message
    *
    * @param cs the variable information to be delimited
    * @return the delimited variable information
    */
  def wrap(cs: CharSequence): String = wrap(cs, GENERAL)

  /**
    * Add delimiters to represent variable information within an error message
    *
    * @param cs        the variable information to be delimited
    * @param valueType the type of value, e.g. element name or attribute name
    * @return the delimited variable information
    */
  def wrap(cs: CharSequence, valueType: Int): String = {
    if (cs == null)
      return "(NULL)"
    val sb = new FastStringBuffer(FastStringBuffer.C64)
    val len = cs.length
    for (i <- 0 until len) {
      val c = cs.charAt(i)
      c match {
        case '\n' => sb.append("\\n")
        case '\t' => sb.append("\\t")
        case '\r' => sb.append("\\r")
        case _ =>
          if (c < 32) {
            sb.append("\\x")
            sb.append(java.lang.Integer.toHexString(c))
          } else {
            sb.cat(c)
          }
      }
//                case '\\':
//                    sb.append("\\\\");
    }
    var s: String = null
    if (valueType == ELEMENT || valueType == ATTRIBUTE) {
      s = sb.toString
      if (s.startsWith("{"))
        s = "Q" + s
      if (s.startsWith("Q{")) {
        try {
          val qn  = StructuredQName.fromEQName(sb.toString)
          val uri = abbreviateURI(qn.getURI)
          s = "Q{" + uri + "}" + qn.getLocalPart
        } catch {
          case _: Exception => s = sb.toString
        }
      }
    } else
      s =
        if (valueType == URI)
          abbreviateURI(sb.toString)
        else if (valueType == EQNAME)
          abbreviateEQName(sb.toString)
        else if (len > 30)
          sb.toString.substring(0, 30) + "..."
        else
          sb.toString
    valueType match {
      case ELEMENT   => "<" + s + ">"
      case ATTRIBUTE => "@" + s
      case FUNCTION  => s + "()"
      case VARIABLE  => "$" + s
      case VALUE     => "\"" + s + "\""
      case EQNAME    => s
      case _         => "{" + s + "}"
    }
  }

  def depict(item: Item): CharSequence =
    item match {
      case node: NodeInfo =>
        node.getNodeKind match {
          case Type.DOCUMENT               => "doc(" + abbreviateURI(node.getSystemId) + ')'
          case Type.ELEMENT                => "<" + node.getDisplayName + ">"
          case Type.ATTRIBUTE              => "@" + node.getDisplayName + "=\"" + node.getStringValueCS + '"'
          case Type.TEXT                   => "text{" + truncate30(node.getStringValueCS) + "}"
          case Type.COMMENT                => "<!--...-->"
          case Type.PROCESSING_INSTRUCTION => "<?" + node.getLocalPart + "...?>"
          case Type.NAMESPACE              => "xmlns:" + node.getLocalPart + "=" + abbreviateURI(node.getStringValue)
          case _                           => ""
        }
      case _              =>
        item.toShortString
    }

  def depictSequence(seq: Sequence): CharSequence = {
    if (seq == null)
      return "(*null*)"
    try {
      val `val` = seq.materialize
      if (`val`.getLength == 0)
        "()"
      else if (`val`.getLength == 1)
        depict(seq.head)
      else
        depictSequenceStart(`val`.iterate(), 3, `val`.getLength)
    } catch {
      case _: Exception => "(*unreadable*)"
    }
  }

  def depictSequenceStart(seq: SequenceIterator,
                          max: Int,
                          actual: Int): String =
    try {
      val sb = new FastStringBuffer(FastStringBuffer.C64)
      var count = 0
      sb.append(" (")
      var next: Item = null
      while ({
        next = seq.next()
        next
      } != null) {
        if ({ count += 1; count - 1 } > 0) {
          sb.append(", ")
        }
        if (count > max) {
          sb.append("... [" + actual + "])")
          sb.toString
        }
        sb.cat(Err.depict(next))
      }
      sb.append(") ")
      sb.toString
    } catch {
      case _: XPathException => ""
    }

  def truncate30(cs: CharSequence): CharSequence =
    if (cs.length <= 30)
      Whitespace.collapseWhitespace(cs)
    else
      Whitespace.collapseWhitespace(cs.subSequence(0, 30)).toString + "..."

  def abbreviateURI(uri: String): String = {
    if (uri == null)
      return ""
    val lastSlash =
      (if (uri.endsWith("/"))
        uri.substring(0, uri.length - 1)
      else
        uri).lastIndexOf('/')
    if (lastSlash < 0) {
      var uriVar = uri
      if (uri.length > 15)
        uriVar = "..." + uri.substring(uri.length - 15)
      uriVar
    } else {
      "..." + uri.substring(lastSlash)
    }
  }

  def abbreviateEQName(eqName: String): String ={
    var eqNameVar = eqName
    try {
      if (eqNameVar.startsWith("{"))
        eqNameVar = "Q" + eqNameVar
      val sq = StructuredQName.fromEQName(eqNameVar)
      "Q{" + abbreviateURI(sq.getURI) + "}" + sq.getLocalPart
    } catch {
      case _: Exception => eqNameVar
    }
  }

  def wrap(exp: Expression): String =
    if (ExpressionTool.expressionSize(exp) < 10 && ! exp.isInstanceOf[Instruction])
      "{" + exp + "}"
    else
      exp.getExpressionName
}
