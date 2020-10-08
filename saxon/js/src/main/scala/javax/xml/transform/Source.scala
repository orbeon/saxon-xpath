package javax.xml.transform


trait Source {
  def setSystemId(systemId: String): Unit
  def getSystemId: String
}
