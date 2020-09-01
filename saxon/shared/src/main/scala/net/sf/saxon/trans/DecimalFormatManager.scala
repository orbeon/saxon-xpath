////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * DecimalFormatManager manages the collection of named and unnamed decimal formats, for use by the
 * format-number() function.
 * <p>In XSLT 2.0, there is a single set of decimal formats shared by the whole stylesheet. In XQuery 3.0, however,
 * each query module has its own set of decimal formats, and in XSLT 3.0 decimal formats are local to a package.
 * The DecimalFormatManager to use is therefore linked from the format-number() call on the expression tree.</p>
 *
 * @author Michael H. Kay
 */

package net.sf.saxon.trans

import net.sf.saxon.om.StructuredQName
import scala.jdk.CollectionConverters._
import java.util.HashMap

import net.sf.saxon.s9api.HostLanguage.HostLanguage

class DecimalFormatManager(var language: HostLanguage,
                           var languageLevel: Int) {

  private var defaultDFS: DecimalSymbols =
    new DecimalSymbols(language, languageLevel)

  // table for named decimal formats
  private var formatTable: HashMap[StructuredQName, DecimalSymbols] =
    new HashMap(10)

  def getDefaultDecimalFormat: DecimalSymbols = defaultDFS

  /*@Nullable*/

  def getNamedDecimalFormat(qName: StructuredQName): DecimalSymbols = {
    val ds: DecimalSymbols = formatTable.get(qName)
    if (ds == null) {
      return null
    }
    // following two lines had been added to the code since 9.4, but they break XSLT test error089
    //            ds = new DecimalSymbols();
    //            formatTable.put(qName, ds);
    // following two lines had been added to the code since 9.4, but they break XSLT test error089
    //            ds = new DecimalSymbols();
    //            formatTable.put(qName, ds);
    ds
  }

  def obtainNamedDecimalFormat(qName: StructuredQName): DecimalSymbols = {
    var ds: DecimalSymbols = formatTable.get(qName)
    if (ds == null) {
      ds = new DecimalSymbols(language, languageLevel)
      formatTable.put(qName, ds)
    }
    ds
  }

  def getDecimalFormatNames: java.lang.Iterable[StructuredQName] =
    formatTable.keySet

  def checkConsistency(): Unit = {
    defaultDFS.checkConsistency(null)
    for ((key, value) <- formatTable.asScala) {
      value.checkConsistency(key)
    }
  }

}

