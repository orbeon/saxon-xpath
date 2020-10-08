package org.orbeon.saxon.trans

import java.io.InputStream
import java.{util => ju}

import org.orbeon.saxon.lib.Logger
import org.orbeon.saxon.serialize.MessageEmitter
import org.orbeon.saxon.utils.Configuration

import scala.beans.BeanProperty


// ORBEON: JVM only
// Used by `Configuration`
class DynamicLoader {

  @BeanProperty
  var classLoader: ClassLoader = _

  var knownClasses: ju.HashMap[String, Class[_]] =
    new ju.HashMap[String, Class[_]](20)

  registerKnownClasses()

  def registerKnownClasses(): Unit = {
    knownClasses.put("org.orbeon.saxon.serialize.MessageEmitter", classOf[MessageEmitter])
    knownClasses.put("org.orbeon.saxon.Configuration", classOf[Configuration])
  }

  def getClass(className: String,
               traceOut: Logger,
               classLoader: ClassLoader): Class[_] = {
    val known = knownClasses.get(className)
    if (known != null)
      return known
    val tracing = traceOut != null
    if (tracing)
      traceOut.info("Loading " + className)
    try {
      var loader = classLoader
      if (loader == null)
        loader = this.classLoader
      if (loader == null)
        loader = Thread.currentThread().getContextClassLoader
      if (loader != null) {
        try loader.loadClass(className)
        catch {
          case _: Throwable =>
            Class.forName(className)
        }
      } else {
        Class.forName(className)
      }
    } catch {
      case e: Throwable =>
        if (tracing)
          traceOut.error("The class " + className + " could not be loaded: " + e.getMessage)
        throw new XPathException("Failed to load " + className + getMissingJarFileMessage(className), e)
    }
  }

  def getInstance(className: String, classLoader: ClassLoader): Any = {
    val theclass = getClass(className, null, classLoader)
    theclass.newInstance()
  }

  def getInstance(className: String,
                  traceOut: Logger,
                  classLoader: ClassLoader): Any = {
    val theclass = getClass(className, traceOut, classLoader)
    theclass.newInstance()
  }

  private def getJarFileForClass(className: String): String =
    if (className.startsWith("org.orbeon.saxon.option.sql.")) {
      "saxon9-sql.jar"
    } else if (className.startsWith("com.saxonica.StatsTransform")) {
      "saxon9-stats.jar"
    } else if (className.startsWith("com.ibm.icu.")) {
      "saxon9-icu.jar"
    } else if (className.startsWith("com.saxonica")) {
      "saxon9ee.jar"
    } else {
      null
    }

  private def getMissingJarFileMessage(className: String): String = {
    val jar = getJarFileForClass(className)
    if (jar == null)
      ""
    else
      ". Check that " + jar + " is on the classpath"
  }

  def getResourceAsStream(name: String): InputStream = {
    var loader = getClassLoader
    if (loader == null)
      loader = Thread.currentThread().getContextClassLoader
    loader.getResourceAsStream(name)
  }
}