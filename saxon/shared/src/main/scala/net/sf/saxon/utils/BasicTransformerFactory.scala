package net.sf.saxon.utils

import net.sf.saxon.utils.Configuration.ApiProvider

class BasicTransformerFactory extends TransformerFactoryImpl {

  var configure: Configuration = new Configuration
  this.configuration = configure
  configure.setProcessor(new ApiProvider {})

  def this(config: Configuration) {
    this()
    this.configuration = (config)
  }


}