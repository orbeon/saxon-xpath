package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import java.io.LineNumberReader

import java.io.Reader

import java.net.URI


class UnparsedTextIterator(absoluteURI: URI,
                           var context: XPathContext,
                           var encoding: String,
                            var loc: Location)
  extends TextLinesIterator {


  val config: Configuration = context.getConfiguration

  var lReader: Reader = context.getController.getUnparsedTextURIResolver
    .resolve(absoluteURI, encoding, config)

  this.reader = new LineNumberReader(lReader)

  this.uri = absoluteURI

  this.checker = context.getConfiguration.getValidCharacterChecker

  this.loc = location

  def this(lReader: LineNumberReader,
           absoluteURI: URI,
           context: XPathContext,
           encoding: String,
           location: Location
          ) = {
    this(absoluteURI, context, encoding, location)
    this.reader = lReader
    this.uri = absoluteURI
    this.context = context
    this.checker = context.getConfiguration.getValidCharacterChecker
    this.encoding = encoding
    this.loc = null
  }

}