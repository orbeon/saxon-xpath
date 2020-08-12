////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.query.StaticQueryContext
import net.sf.saxon.utils.Configuration


class StaticQueryContextFactory {

  def newStaticQueryContext(config: Configuration,
                            copyFromDefault: Boolean): StaticQueryContext =
    new StaticQueryContext(config, copyFromDefault)

}
