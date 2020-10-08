package javax.xml.transform


trait Result {
  def setSystemId(systemId: String): Unit
  def getSystemId: String
}

object Result {
  val PI_DISABLE_OUTPUT_ESCAPING = "javax.xml.transform.disable-output-escaping"
  val PI_ENABLE_OUTPUT_ESCAPING  = "javax.xml.transform.enable-output-escaping"
}