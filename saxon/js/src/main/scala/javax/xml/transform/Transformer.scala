package javax.xml.transform

import java.util.Properties


abstract class Transformer protected () {

  def reset(): Unit =
    throw new UnsupportedOperationException

  def transform(xmlSource: Source, outputTarget: Result): Unit
  def setParameter(name: String, value: Any): Unit
  def getParameter(name: String): Any
  def clearParameters(): Unit
  def setURIResolver(resolver: URIResolver): Unit
  def getURIResolver: URIResolver
  def setOutputProperties(oformat: Properties): Unit
  def getOutputProperties: Properties
  def setOutputProperty(name: String, value: String): Unit
  def getOutputProperty(name: String): String
  def setErrorListener(listener: ErrorListener): Unit
  def getErrorListener: ErrorListener
}
