package org.orbeon.saxon.trans

import org.orbeon.saxon.utils.Configuration


class Instantiator[T](private var className: String,
                      private var config: Configuration)
  extends Maker[T] {

  def make(): T = {
    val o: Any = config.getInstance(className)
    o.asInstanceOf[T]
  }
}
