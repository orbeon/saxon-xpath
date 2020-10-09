package javax.xml.transform

class TransformerFactoryConfigurationError(message: String, cause: Throwable)
  extends Error(message, cause) {

  private var exception: Exception = null

  def this(msg: String) = {
    this(msg, null)
    this.exception = null
  }

  def this(e: Exception) = {
    this(null, e)
    this.exception = e
  }

  def this(e: Exception, msg: String) = {
    this(msg, e)
    this.exception = e
  }

  override def getMessage: String = {
    val message = super.getMessage
    if ((message == null) && (exception != null)) return exception.getMessage
    message
  }

  def getException: Exception = exception

  override def getCause: Throwable = exception
}
