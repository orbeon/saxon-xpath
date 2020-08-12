
package net.sf.saxon.lib

import java.util.{ArrayList, List}

import javax.xml.transform.{Result, TransformerException}
import net.sf.saxon.event.{CloseNotifier, PipelineConfiguration, Receiver}
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.s9api.Action
import net.sf.saxon.serialize.SerializationProperties
import net.sf.saxon.trans.{UncheckedXPathException, XPathException}

import scala.beans.BeanProperty

class OutputURIResolverWrapper(resolver: OutputURIResolver)
    extends ResultDocumentResolver {

  @BeanProperty
  var outputURIResolver: OutputURIResolver = resolver

  def resolve(context: XPathContext,
              href: String,
              baseUri: String,
              properties: SerializationProperties): Receiver = {
    val r2: OutputURIResolver = outputURIResolver.newInstance()
    val result: Result = r2.resolve(href, baseUri)
    val onClose :Action= new Action {
      def act(): Unit = {
        try r2.close(result)
        catch {
          case te: TransformerException =>
            throw new UncheckedXPathException(XPathException.makeXPathException(te))
        }
      }
      act()
    }
    var out: Receiver = null
    if (result.isInstanceOf[Receiver]) {
      out = result.asInstanceOf[Receiver]
    } else {
      val factory: SerializerFactory =
        context.getConfiguration.getSerializerFactory
      val pipe: PipelineConfiguration =
        context.getController.makePipelineConfiguration
      pipe.setXPathContext(context)
      out = factory.getReceiver(result, properties, pipe)
    }
    val actions: List[Action] = new ArrayList[Action]()
    actions.add(onClose)
    new CloseNotifier(out, actions)
  }

}


