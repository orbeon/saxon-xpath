package javax.xml.transform

import java.io.{PrintStream, PrintWriter}


class TransformerException(message: String) extends Exception(message) {

  private[transform] var containedException: Throwable = null
  private[transform] var locator: SourceLocator = null

  def getLocator: SourceLocator = locator

  def setLocator(location: SourceLocator): Unit =
    locator = location

  def getException: Throwable = containedException

  override def getCause: Throwable =
    if (containedException eq this)
      null
    else
      containedException

  override def initCause(cause: Throwable): Throwable = {
    if (this.containedException != null)
      throw new IllegalStateException("Can't overwrite cause")
    if (cause eq this)
      throw new IllegalArgumentException("Self-causation not permitted")
    this.containedException = cause
    this
  }

  def this(e: Throwable) = {
    this(e.toString)
    this.containedException = e
    this.locator = null
  }

  def this(message: String, e: Throwable) = {
    this(if ((message == null) || (message.length == 0)) e.toString else message)
    this.containedException = e
    this.locator = null
  }

  def this(message: String, locator: SourceLocator) = {
    this(message)
    this.containedException = null
    this.locator = locator
  }

  def this(message: String, locator: SourceLocator, e: Throwable) = {
    this(message)
    this.containedException = e
    this.locator = locator
  }

  def getLocationAsString: String =
    if (null != locator) {
      val sbuffer = new StringBuffer
      val systemID = locator.getSystemId
      val line = locator.getLineNumber
      val column = locator.getColumnNumber
      if (null != systemID) {
        sbuffer.append("; SystemID: ")
        sbuffer.append(systemID)
      }
      if (0 != line) {
        sbuffer.append("; Line#: ")
        sbuffer.append(line)
      }
      if (0 != column) {
        sbuffer.append("; Column#: ")
        sbuffer.append(column)
      }
      sbuffer.toString
    } else
      null

  override def printStackTrace(): Unit =
    printStackTrace(new PrintWriter(System.err, true))

  override def printStackTrace(s: PrintStream): Unit =
    printStackTrace(new PrintWriter(s))

  override def printStackTrace(s: PrintWriter): Unit = {

    super.printStackTrace(s)

//    val _s =
//      if (s eq null)
//        new PrintWriter(System.err, true)
//      else
//        s
//
//    try {
//      val locInfo = getLocationAsString
//      if (null != locInfo) _s.println(locInfo)
//      super.printStackTrace(_s)
//    } catch {
//      case e: Throwable =>
//    }
//    var exception = getException
//    var i = 0
//    while ( {
//      (i < 10) && (null != exception)
//    }) {
//      _s.println("---------")
//      try {
//        exception match {
//          case transformerException: TransformerException =>
//            val locInfo = transformerException.getLocationAsString
//            if (null != locInfo)
//              _s.println(locInfo)
//          case _ =>
//        }
//        exception.printStackTrace(_s)
//      } catch {
//        case _: Throwable =>
//          _s.println("Could not print stack trace...")
//      }
//      try {
//        val meth = exception.asInstanceOf[Any].getClass.getMethod("getException", null.asInstanceOf[Array[Class[_]]])
//        if (null != meth) {
//          val prev = exception
//          exception = meth.invoke(exception, null.asInstanceOf[Array[AnyRef]]).asInstanceOf[Throwable]
//          if (prev eq exception) break //todo: break is not supported
//        }
//        else exception = null
//      } catch {
//        case _: InvocationTargetException =>
//          exception = null
//        case _: IllegalAccessException =>
//          exception = null
//        case _: NoSuchMethodException =>
//          exception = null
//      }
//
//      i += 1
//    }
//    _s.flush()
  }
}
