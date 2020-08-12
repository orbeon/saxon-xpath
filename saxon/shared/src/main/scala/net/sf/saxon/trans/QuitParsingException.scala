////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.trans

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * An exception used to signal that no more input is required from the parser, and that
  * parsing can therefore be abandoned early (but without signaling any error to the user)
  */
class QuitParsingException /**
  * Create a QuitParsingException
  *
  * @param notifiedByConsumer should be set to true if the exception was generating in the parsing
  *                           thread in response to an interrupt from the consuming thread; in this case
  *                           there is no need for the consuming thread to be notified of the termination
  *                           (by sending a "stopper" item); and indeed, doing so causes the thread to hang.
  *                           See bug 3080.
  */
(@BooleanBeanProperty var notifiedByConsumer: Boolean)
    extends XPathException("No more input required", "SXQP0001")

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
