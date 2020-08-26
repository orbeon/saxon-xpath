package net.sf.saxon.s9api

import net.sf.saxon.event._
import net.sf.saxon.expr.instruct.Executable
import net.sf.saxon.expr.instruct.GlobalContextRequirement
import net.sf.saxon.expr.instruct.UserFunction
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.expr.parser.RoleDiagnostic
import net.sf.saxon.lib.Logger
import net.sf.saxon.lib.TraceListener
import net.sf.saxon.model.TypeHierarchy
import net.sf.saxon.om._
import net.sf.saxon.query.DynamicQueryContext
import net.sf.saxon.query.XQueryExpression
import net.sf.saxon.s9api.streams.XdmStream
import net.sf.saxon.serialize.SerializationProperties
import net.sf.saxon.tree.tiny.TinyBuilder
import javax.xml.transform.ErrorListener
import javax.xml.transform.Source
import javax.xml.transform.URIResolver
import javax.xml.transform.dom.DOMSource
import java.util.HashSet
import java.util.Iterator
import java.util.Set

import net.sf.saxon.utils.{Configuration, Controller}
import ValidationMode._

import scala.jdk.CollectionConverters._

class XQueryEvaluator(var processor: Processor,
                      var expression: XQueryExpression)
  extends AbstractDestination
    with java.lang.Iterable[XdmItem] {

  private var context: DynamicQueryContext = new DynamicQueryContext(
    expression.getConfiguration)

  private var controller: Controller = _

  private var destination: Destination = _

  private var updatedDocuments: Set[XdmNode] = _

  private var sourceTreeBuilder: Builder = _

  def setSchemaValidationMode(mode: ValidationMode): Unit = {
    if (mode != null) {
      context.setSchemaValidationMode(mode.getNumber)
    }
  }

  def getSchemaValidationMode(): ValidationMode =
    ValidationMode.get(context.getSchemaValidationMode)

  def setSource(source: Source): Unit = {
    if (source.isInstanceOf[NodeInfo]) {
      this.setContextItem(new XdmNode(source.asInstanceOf[NodeInfo]))
    } else if (source.isInstanceOf[DOMSource]) {
      this.setContextItem(processor.newDocumentBuilder().wrap(source))
    } else {
      this.setContextItem(processor.newDocumentBuilder().build(source))
    }
  }

  def setContextItem(item: XdmItem): Unit = {
    if (item != null) {
      val gcr: GlobalContextRequirement =
        expression.getExecutable.getGlobalContextRequirement
      if (gcr != null && !gcr.isExternal) {
        throw new SaxonApiException(
          "The context item for the query is not defined as external")
      }
      context.setContextItem(item.getUnderlyingValue)
    }
  }

  def getContextItem(): XdmItem = {
    val item: Item = context.getContextItem
    if (item == null) {
      return null
    }
    XdmValue.wrap(item).asInstanceOf[XdmItem]
  }

  def setExternalVariable(name: QName, value: XdmValue): Unit = {
    context.setParameter(
      name.getStructuredQName,
      if (value == null) null
      else value.getUnderlyingValue.asInstanceOf[Sequence].materialize())
  }

  def getExternalVariable(name: QName): XdmValue = {
    val oval: GroundedValue = context.getParameter(name.getStructuredQName)
    if (oval == null) {
      return null
    }
    XdmValue.wrap(oval)
  }

  def setURIResolver(resolver: URIResolver): Unit = {
    context.setURIResolver(resolver)
  }

  def getURIResolver(): URIResolver = context.getURIResolver

  def setErrorListener(listener: ErrorListener): Unit = {
    context.setErrorListener(listener)
  }

  def getErrorListener(): ErrorListener = context.getErrorListener

  def setTraceListener(listener: TraceListener): Unit = {
    context.setTraceListener(listener)
  }

  def getTraceListener(): TraceListener = context.getTraceListener

  def setTraceFunctionDestination(stream: Logger): Unit = {
    context.setTraceFunctionDestination(stream)
  }

  def getTraceFunctionDestination(): Logger =
    context.getTraceFunctionDestination

  def setDestination(destination: Destination): Unit = {
    this.destination = destination
  }

  def run(): Unit = {
    if (expression.isUpdateQuery) {
      val docs: Set[MutableNodeInfo] = expression.runUpdate(context)
      updatedDocuments = new HashSet()
      for (doc <- docs.asScala) {
        updatedDocuments.add(XdmItem.wrapItem(doc))
      }
    } else {
      if (destination == null) {
        throw new IllegalStateException("No destination supplied")
      }
      run(destination)
    }
  }

  def run(destination: Destination): Unit = {
    if (expression.isUpdateQuery) {
      throw new IllegalStateException("Query is updating")
    }
    val out: Receiver = getDestinationReceiver(destination)
    expression.run(context, out, null)
    destination.closeAndNotify()
  }

  def runStreamed(source: Source, destination: Destination): Unit = {
    if (expression.isUpdateQuery) {
      throw new IllegalStateException(
        "Query is updating; cannot run with streaming")
    }
    val config: Configuration = context.getConfiguration
    if (config.isTiming) {
      var systemId: String = source.getSystemId
      if (systemId == null) {
        systemId = ""
      }
      config.getStandardErrorOutput.println(
        "Processing streamed input " + systemId)
    }
    val params: SerializationProperties =
      expression.getExecutable.getPrimarySerializationProperties
    val receiver: Receiver =
      destination.getReceiver(config.makePipelineConfiguration, params)
    expression.runStreamed(context, source, receiver, null)
  }

  def evaluate(): XdmValue = {
    if (expression.isUpdateQuery) {
      throw new IllegalStateException("Query is updating")
    }
    val iter: SequenceIterator = expression.iterator(context)
    XdmValue.wrap(iter.materialize())
  }

  def evaluateSingle(): XdmItem = {
    val iter: SequenceIterator = expression.iterator(context)
    val next: Item = iter.next()
    if (next == null) null else XdmValue.wrap(next).asInstanceOf[XdmItem]
  }

  def iterator(): XdmSequenceIterator[XdmItem] = {
    if (expression.isUpdateQuery) {
      throw new IllegalStateException("Query is updating")
    }
    new XdmSequenceIterator(expression.iterator(context))
  }

  def stream(): XdmStream[_<:XdmItem] = iterator().asInstanceOf[XdmItem].stream()

  private def getDestinationReceiver(destination: Destination): Receiver = {
    val exec: Executable = expression.getExecutable
    val pipe: PipelineConfiguration =
      expression.getConfiguration.makePipelineConfiguration
    val out: Receiver =
      destination.getReceiver(pipe, exec.getPrimarySerializationProperties)
    if (Configuration.isAssertionsEnabled) {
      new RegularSequenceChecker(out, true)
    } else {
      out
    }
  }

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver = {
    if (destination == null) {
      throw new IllegalStateException("No destination has been supplied")
    }
    if (controller == null) {
      controller = expression.newController(context)
    } else {
      context.initializeController(controller)
    }
    sourceTreeBuilder = controller.makeBuilder
    if (sourceTreeBuilder.isInstanceOf[TinyBuilder]) {
      sourceTreeBuilder
        .asInstanceOf[TinyBuilder]
        .setStatistics(
          context.getConfiguration.getTreeStatistics.SOURCE_DOCUMENT_STATISTICS)
    }
    val out: Receiver = controller.makeStripper(sourceTreeBuilder)
    val sn: SequenceNormalizer = params.makeSequenceNormalizer(out)
    sn.onClose(() => {
      val doc: NodeInfo = sourceTreeBuilder.getCurrentRoot
      if (doc == null) {
        throw new SaxonApiException(
          "No source document has been built by the previous pipeline stage")
      }
      doc.getTreeInfo.setSpaceStrippingRule(controller.getSpaceStrippingRule)
      this.setSource(doc)
      sourceTreeBuilder = null
      run(destination)
      destination.closeAndNotify()
    })
    sn
  }

  def close(): Unit = ()

  def getUpdatedDocuments(): Iterator[XdmNode] = updatedDocuments.iterator()

  def callFunction(function: QName, arguments: Array[XdmValue]): XdmValue = {
    val fn: UserFunction = expression.getMainModule.getUserDefinedFunction(
      function.getNamespaceURI,
      function.getLocalName,
      arguments.length)
    if (fn == null) {
      throw new SaxonApiException(
        "No function with name " + function.getClarkName + " and arity " +
          arguments.length +
          " has been declared in the query")
    }
    if (controller == null) {
      controller = expression.newController(context)
    } else {
      context.initializeController(controller)
    }
    val config: Configuration = processor.getUnderlyingConfiguration
    val vr: Array[Sequence] = SequenceTool.makeSequenceArray(arguments.length)
    for (i <- 0 until arguments.length) {
      val `type`: net.sf.saxon.value.SequenceType =
        fn.getParameterDefinitions()(i).getRequiredType
      vr(i) = arguments(i).getUnderlyingValue
      val th: TypeHierarchy = config.getTypeHierarchy
      if (!`type`.matches(vr(i), th)) {
        val role: RoleDiagnostic = new RoleDiagnostic(
          RoleDiagnostic.FUNCTION,
          function.getStructuredQName.getDisplayName,
          i)
        vr(i) = th.applyFunctionConversionRules(vr(i), `type`, role, Loc.NONE)
      }
    }
    val result: Sequence = fn.call(vr, controller)
    XdmValue.wrap(result)
  }

  def getUnderlyingQueryContext(): DynamicQueryContext = context

}
