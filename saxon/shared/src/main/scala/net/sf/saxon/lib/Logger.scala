////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import javax.xml.transform.stream.StreamResult

import Logger._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object Logger {

  val INFO: Int = 0

  val WARNING: Int = 1

  val ERROR: Int = 2

  val DISASTER: Int = 3

}

/**
  * Interface to diagnostic event logging mechanism.
  * A Logger can be registered at the level of the Saxon Configuration.
  * The default implementation for the Java platform writes all messages to System.err
  */
abstract class Logger {

  @BooleanBeanProperty
  var unicodeAware: Boolean = false

  /**
    * Log a message with level {@link Logger#INFO}
    * @param message the message to be logged
    */
  def info(message: String): Unit = {
    println(message, INFO)
  }

  /**
    * Log a message with level {@link Logger#WARNING}
    * @param message the message to be logged
    */
  def warning(message: String): Unit = {
    println(message, WARNING)
  }

  /**
    * Log a message with level {@link Logger#ERROR}
    * @param message the message to be logged
    */
  def error(message: String): Unit = {
    println(message, ERROR)
  }

  /**
    * Log a message with level {@link Logger#DISASTER}
    * @param message the message to be logged
    */
  def disaster(message: String): Unit = {
    println(message, DISASTER)
  }

  def println(message: String, severity: Int): Unit

  def close(): Unit = {}

  def asStreamResult(): StreamResult

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
