/*
 * Copyright (c) 2000, 2005, Oracle and/or its affiliates. All rights reserved.
 * ORACLE PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package javax.xml.transform.sax

import javax.xml.transform.Result
import org.xml.sax.ContentHandler
import org.xml.sax.ext.LexicalHandler


object SAXResult {
  val FEATURE = "http://javax.xml.transform.sax.SAXResult/feature"
}

class SAXResult extends Result {

  private var handler: ContentHandler = null
  private var lexhandler: LexicalHandler = null
  private var systemId: String = null

  def this(handler: ContentHandler) = {
    this()
    setHandler(handler)
  }

  def setHandler(handler: ContentHandler): Unit =
    this.handler = handler

  def getHandler: ContentHandler = handler

  def setLexicalHandler(handler: LexicalHandler): Unit =
    this.lexhandler = handler

  def getLexicalHandler: LexicalHandler = lexhandler

  def setSystemId(systemId: String): Unit =
    this.systemId = systemId

  def getSystemId: String = systemId
}
