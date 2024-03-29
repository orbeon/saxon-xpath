package org.orbeon.saxon.trans

import org.orbeon.saxon.lib.ErrorReporter




class XmlProcessingAbort(message: String) extends RuntimeException(message)

/**
  * An unchecked exception, triggered when a user-supplied {@link ErrorReporter} requests
  * that processing should be aborted
  */
