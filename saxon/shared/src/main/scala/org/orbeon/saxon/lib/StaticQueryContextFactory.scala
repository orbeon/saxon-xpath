////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.lib

import org.orbeon.saxon.query.StaticQueryContext
import org.orbeon.saxon.utils.Configuration


class StaticQueryContextFactory {

  def newStaticQueryContext(config: Configuration,
                            copyFromDefault: Boolean): StaticQueryContext =
    new StaticQueryContext(config, copyFromDefault)

}
