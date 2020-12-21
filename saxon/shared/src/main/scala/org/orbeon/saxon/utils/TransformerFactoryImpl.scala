package org.orbeon.saxon.utils

class TransformerFactoryImpl() {

  var configuration: Configuration = _

  def this(config: Configuration) = {
    this()
    this.configuration = config
  }
}
