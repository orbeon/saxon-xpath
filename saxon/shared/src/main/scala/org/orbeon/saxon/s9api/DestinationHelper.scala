package org.orbeon.saxon.s9api

import java.util.ArrayList

import java.util.List

import scala.beans.{BeanProperty, BooleanBeanProperty}
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class DestinationHelper(private var helpee: Destination) {

  @BeanProperty
  var listeners: List[Action] = new ArrayList()

  def onClose(listener: Action): Unit = {
    listeners.add(listener)
  }

  def closeAndNotify(): Unit = {
    helpee.close()
    for (action <- listeners.asScala) {
      action.act()
    }
  }

}
