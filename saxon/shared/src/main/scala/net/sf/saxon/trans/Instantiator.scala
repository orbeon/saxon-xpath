package net.sf.saxon.trans

import net.sf.saxon.utils.Configuration

//remove if not needed
// import scala.collection.JavaConversions._

class Instantiator[T](private var className: String,
                      private var config: Configuration)
  extends Maker[T] {

  def make(): T = {
    val o: Any = config.getInstance(className, null)
    o.asInstanceOf[T]
  }

}
