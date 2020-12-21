package org.orbeon.saxon.serialize

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.om.NodeName

import java.util.Collections

import java.util.HashSet

import java.util.Set

import XHTML1Emitter._

object XHTML1Emitter {

  var emptyTags1: Set[String] = new HashSet[String](31)

  private var emptyTagNames1: Array[String] = Array("area",
    "base",
    "basefont",
    "br",
    "col",
    "embed",
    "frame",
    "hr",
    "img",
    "input",
    "isindex",
    "link",
    "meta",
    "param")

  Collections.addAll(emptyTags1, emptyTagNames1:_*)

}

class XHTML1Emitter extends XMLEmitter {

  private def isRecognizedHtmlElement(name: NodeName): Boolean =
    name.hasURI(NamespaceConstant.XHTML)

  override  def emptyElementTagCloser(displayName: String,
                                               name: NodeName): String =
    if (isRecognizedHtmlElement(name) && emptyTags1.contains(
      name.getLocalPart)) {
      " />"
    } else {
      "></" + displayName + '>'
    }

}
