package net.sf.saxon.trans

import net.sf.saxon.utils.Configuration
import net.sf.saxon.utils.Controller
import net.sf.saxon.event._
import net.sf.saxon.expr._
import net.sf.saxon.expr.accum.AccumulatorManager
import net.sf.saxon.expr.instruct._
import net.sf.saxon.expr.parser.Loc
import net.sf.saxon.lib._
import net.sf.saxon.om._
import net.sf.saxon.s9api.Destination
import net.sf.saxon.serialize.Emitter
import net.sf.saxon.serialize.MessageEmitter
import net.sf.saxon.serialize.PrincipalOutputGatekeeper
import net.sf.saxon.tree.iter.EmptyIterator
import net.sf.saxon.tree.iter.ManualIterator
import net.sf.saxon.tree.iter.SingletonIterator
import net.sf.saxon.tree.wrapper.SpaceStrippedDocument
import net.sf.saxon.tree.wrapper.SpaceStrippedNode
import net.sf.saxon.tree.wrapper.TypeStrippedDocument
import javax.xml.transform.OutputKeys
import javax.xml.transform.Source
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamSource
import java.util._
import java.util.function.Supplier
import net.sf.saxon.style.StylesheetPackage

import scala.beans.{BeanProperty, BooleanBeanProperty}

class XsltController(config: Configuration, pss: Executable) extends Controller(config, pss) { //  PreparedStylesheet class not exist

  private val messageCounters: Map[StructuredQName, Integer] = new HashMap()

  private var explicitMessageReceiver: Receiver = null

  private var messageFactory: Supplier[Receiver] = () => new NamespaceDifferencer(new MessageEmitter(), new Properties())

  @BooleanBeanProperty
  var assertionsEnabled: Boolean = true

  @BeanProperty
  var resultDocumentResolver: ResultDocumentResolver = _

  private var allOutputDestinations: HashSet[DocumentURI] = _

  private var initialMode: Component.M = _

  private var initialFunction: Function = _

  private var initialTemplateParams: Map[StructuredQName, Sequence] = _

  private var initialTemplateTunnelParams: Map[StructuredQName, Sequence] = _

  private var attributeSetEvaluationStacks: Map[Long, Stack[AttributeSet]] =
    new HashMap()

  @BeanProperty
  var accumulatorManager: AccumulatorManager = new AccumulatorManager()

  @BeanProperty
  var gatekeeper: PrincipalOutputGatekeeper = null

  @BeanProperty
  var principalDestination: Destination = _

  override def reset(): Unit = {
    super.reset()
    val config: Configuration = getConfiguration
    validationMode = config.getSchemaValidationMode
    accumulatorManager = new AccumulatorManager()
    traceListener = null
    var tracer: TraceListener = null
    tracer = config.makeTraceListener
    if (tracer != null) {
      addTraceListener(tracer)
    }
    this.setModel(config.getParseOptions.getModel)
    globalContextItem = null
    initialMode = null
    clearPerTransformationData()
  }

  override  def clearPerTransformationData(): Unit = {
    synchronized {
      super.clearPerTransformationData()
      principalResult = null
      allOutputDestinations = null
      if (messageCounters != null) {
        messageCounters.clear()
      }
    }
  }

  def setInitialMode(expandedModeName: StructuredQName): Unit = {
    /* if (expandedModeName == null || expandedModeName == Mode.UNNAMED_MODE_NAME) {
       val initial: Mode = executable
         .asInstanceOf[PreparedStylesheet]
         .getRuleManager
         .obtainMode(Mode.UNNAMED_MODE_NAME, true)
       initialMode = initial.getDeclaringComponent
     } */
    if (expandedModeName != null || expandedModeName != Mode.UNNAMED_MODE_NAME) {
      val topLevelPackage: StylesheetPackage =
        executable.getTopLevelPackage.asInstanceOf[StylesheetPackage]
      if (expandedModeName == Mode.DEFAULT_MODE_NAME) {
        val defaultModeName: StructuredQName = topLevelPackage.getDefaultMode
        if (expandedModeName != defaultModeName) {
          setInitialMode(defaultModeName)
        }
      } else {
        val declaredModes: Boolean = topLevelPackage.isDeclaredModes
        val sn: SymbolicName =
          new SymbolicName(StandardNames.XSL_MODE, expandedModeName)
        val c: Component.M =
          topLevelPackage.getComponent(sn).asInstanceOf[Component.M]
        if (c == null) {
          throw new XPathException(
            "Requested initial mode " + expandedModeName + " is not defined in the stylesheet",
            "XTDE0045")
        }
        //PreparedStylesheet not exist
        /*  if (!executable
            .asInstanceOf[PreparedStylesheet]
            .isEligibleInitialMode(c)) {
            throw new XPathException(
              "Requested initial mode " + expandedModeName + " is private in the top-level package",
              "XTDE0045")
          }*/
        initialMode = c
        if (!declaredModes && initialMode.getActor.isEmpty() && expandedModeName != topLevelPackage.getDefaultMode) {
          throw new XPathException(
            "Requested initial mode " + expandedModeName + " contains no template rules",
            "XTDE0045")
        }
      }
    }
  }

  def getInitialModeName(): StructuredQName =
    if (initialMode == null) null else initialMode.getActor.getModeName

  def getInitialMode(): Mode =
    if (initialMode == null) {
      val top: StylesheetPackage =
        executable.getTopLevelPackage.asInstanceOf[StylesheetPackage]
      var defaultMode: StructuredQName = top.getDefaultMode
      if (defaultMode == null) {
        defaultMode = Mode.UNNAMED_MODE_NAME
      }
      val c: Component.M = top
        .getComponent(new SymbolicName(StandardNames.XSL_MODE, defaultMode))
        .asInstanceOf[Component.M]
      initialMode = c
      c.getActor
    } else {
      initialMode.getActor
    }

  def checkUniqueOutputDestination(uri: DocumentURI): Boolean = synchronized {
    if (uri == null) {
      return true
    }
    if (allOutputDestinations == null) {
      allOutputDestinations = new HashSet(20)
    }
    !allOutputDestinations.contains(uri)
  }

  def addUnavailableOutputDestination(uri: DocumentURI): Unit = {
    if (allOutputDestinations == null) {
      allOutputDestinations = new HashSet(20)
    }
    allOutputDestinations.add(uri)
  }

  def removeUnavailableOutputDestination(uri: DocumentURI): Unit = {
    if (allOutputDestinations != null) {
      allOutputDestinations.remove(uri)
    }
  }

  def isUnusedOutputDestination(uri: DocumentURI): Boolean =
    allOutputDestinations == null || !allOutputDestinations.contains(uri)

  def setInitialTemplateParameters(params: Map[StructuredQName, Sequence],
                                   tunnel: Boolean): Unit = {
    if (tunnel) {
      this.initialTemplateTunnelParams = params
    } else {
      this.initialTemplateParams = params
    }
  }

  def getInitialTemplateParameters(
                                    tunnel: Boolean): Map[StructuredQName, Sequence] =
    if (tunnel) initialTemplateTunnelParams else initialTemplateParams

  def setMessageFactory(messageReceiverFactory: Supplier[Receiver]): Unit = {
    this.messageFactory = messageReceiverFactory
  }

  def setMessageReceiverClassName(name: String): Unit = {
    if (name != classOf[MessageEmitter].getName) {
      this.messageFactory = () => {
        var messageReceiver: Any = getConfiguration.getInstance(name, null)
        if (!(messageReceiver.isInstanceOf[Receiver])) {
          throw new XPathException(name + " is not a Receiver")
        }
        messageReceiver.asInstanceOf[Receiver]
      }
    }
  }

  def makeMessageReceiver(): Receiver = messageFactory.get

  def setMessageEmitter(receiver: Receiver): Unit = {
    if (getConfiguration.getBooleanProperty(Feature.ALLOW_MULTITHREADING)) {
      throw new IllegalStateException(
        "XsltController#setMessageEmitter() is not supported for a configuration that allows multi-threading. Use setMessageFactory() instead")
    }
    explicitMessageReceiver = receiver
    val messageReceiver: Receiver = explicitMessageReceiver
    receiver.setPipelineConfiguration(makePipelineConfiguration)
    if (receiver.isInstanceOf[Emitter] &&
      receiver.asInstanceOf[Emitter].getOutputProperties == null) {
      try {
        val props: Properties = new Properties()
        props.setProperty(OutputKeys.METHOD, "xml")
        props.setProperty(OutputKeys.INDENT, "yes")
        props.setProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
        receiver.asInstanceOf[Emitter].setOutputProperties(props)
      } catch {
        case e: XPathException => {}

      }
    }
    this.messageFactory = () =>
      new ProxyReceiver(messageReceiver) {
        override def close(): Unit = {}
      }
  }

  def getMessageEmitter(): Receiver = explicitMessageReceiver

  def incrementMessageCounter(code: StructuredQName): Unit = {
    messageCounters.synchronized {
      val c: java.lang.Integer = messageCounters.get(code)
      val n: Int = if (c == null) 1 else c + 1
      messageCounters.put(code, n)
    }
  }

  def getMessageCounters(): Map[StructuredQName, Integer] =
    messageCounters.synchronized {
      new HashMap(messageCounters)
    }

  def setOutputURIResolver(resolver: OutputURIResolver): Unit = {
    val our: OutputURIResolver =
      if (resolver == null) getConfiguration.getOutputURIResolver else resolver
    this.resultDocumentResolver = new OutputURIResolverWrapper(our)
  }

  def getOutputURIResolver(): OutputURIResolver =
    if (resultDocumentResolver.isInstanceOf[OutputURIResolverWrapper]) {
      resultDocumentResolver
        .asInstanceOf[OutputURIResolverWrapper]
        .getOutputURIResolver
    } else {
      getConfiguration.getOutputURIResolver
    }

  override def preEvaluateGlobals(context: XPathContext): Unit = {
    openMessageEmitter()
    super.preEvaluateGlobals(context)
    closeMessageEmitter()
  }

  def applyTemplates(source: Sequence, out: Receiver): Unit = {
    checkReadiness()
    openMessageEmitter()
    try {
      val dest: ComplexContentOutputter = prepareOutputReceiver(out)
      val initialContext: XPathContextMajor = newXPathContext
      initialContext.createThreadManager()
      initialContext.setOrigin(this)
      val close: Boolean = false
      val mode: Mode = getInitialMode
      if (mode == null) {
        throw new XPathException(
          "Requested initial mode " +
            (if (initialMode == null) "#unnamed"
            else initialMode.getActor.getModeName.getDisplayName) +
            " does not exist",
          "XTDE0045")
      }
      // PreparedStylesheet not exist
      /*  if (!executable
          .asInstanceOf[PreparedStylesheet]
          .isEligibleInitialMode(initialMode)) {
          throw new XPathException(
            "Requested initial mode " + (mode.getModeName.getDisplayName) +
              " is not public or final",
            "XTDE0045")
        }*/
      warningIfStreamable(mode)
      val mustClose: Boolean = false
      var ordinaryParams: ParameterSet = null
      if (initialTemplateParams != null) {
        ordinaryParams = new ParameterSet(initialTemplateParams)
      }
      var tunnelParams: ParameterSet = null
      if (initialTemplateTunnelParams != null) {
        tunnelParams = new ParameterSet(initialTemplateTunnelParams)
      }
      var iter: SequenceIterator = source.iterate()
      val preprocessor: MappingFunction = getInputPreprocessor(mode)
      iter = new MappingIterator(iter, preprocessor)
      initialContext.trackFocus(iter)
      initialContext.setCurrentMode(initialMode)
      initialContext.setCurrentComponent(initialMode)
      var tc: TailCall = mode.applyTemplates(ordinaryParams,
        tunnelParams,
        dest,
        initialContext,
        Loc.NONE)
      while (tc != null) tc = tc.processLeavingTail()
      initialContext.waitForChildThreads()
      dest.close()
    } catch {
      case err: TerminationException => {
        if (!err.hasBeenReported) {
          reportFatalError(err)
        }
        throw err
      }

      case err: UncheckedXPathException =>
        handleXPathException(err.getXPathException)

      case err: XPathException => handleXPathException(err)

    } finally {
      inUse = false
      closeMessageEmitter()
      if (traceListener != null) {
        traceListener.close()
      }
      principalResultURI = null
    }
  }

  private def prepareOutputReceiver(out: Receiver): ComplexContentOutputter = {
    var outRec = out
    principalResult = outRec
    if (principalResultURI == null) {
      principalResultURI = outRec.getSystemId
    }
    if (getExecutable.createsSecondaryResult) {
      this.gatekeeper = new PrincipalOutputGatekeeper(this, outRec)
      outRec = this.gatekeeper
    }
    val cco: ComplexContentOutputter = new ComplexContentOutputter(outRec)
    cco.setSystemId(out.getSystemId)
    cco.open()
    cco
  }

  private def getInputPreprocessor(finalMode: Mode): MappingFunction =
    (item) =>
      if (item.isInstanceOf[NodeInfo]) {
        var node: NodeInfo = item.asInstanceOf[NodeInfo]
        if (node.getConfiguration == null) {
          throw new XPathException(
            "The supplied source document must be associated with a Configuration")
        }
        if (!node.getConfiguration.isCompatible(executable.getConfiguration)) {
          throw new XPathException(
            "Source document and stylesheet must use the same or compatible Configurations",
            SaxonErrorCode.SXXP0004)
        }
        if (node.getTreeInfo.isTyped && !executable.isSchemaAware) {
          throw new XPathException(
            "Cannot use a schema-validated source document unless the stylesheet is schema-aware")
        }
        if (/*isStylesheetStrippingTypeAnnotations &&*/ node != globalContextItem) {
          val docInfo: TreeInfo = node.getTreeInfo
          if (docInfo.isTyped) {
            val strippedDoc: TypeStrippedDocument = new TypeStrippedDocument(
              docInfo)
            node = strippedDoc.wrap(node)
          }
        }
        val spaceStrippingRule: SpaceStrippingRule = getSpaceStrippingRule
        if (isStylesheetContainingStripSpace && isStripSourceTree &&
          !(node.isInstanceOf[SpaceStrippedNode]) &&
          node != globalContextItem &&
          node.getTreeInfo.getSpaceStrippingRule != spaceStrippingRule) {
          val strippedDoc: SpaceStrippedDocument =
            new SpaceStrippedDocument(node.getTreeInfo, spaceStrippingRule)
          if (!SpaceStrippedNode.isPreservedNode(node,
            strippedDoc,
            node.getParent)) {
            EmptyIterator.emptyIterator()
          }
          node = strippedDoc.wrap(node)
        }
        if (getAccumulatorManager != null) {
          getAccumulatorManager.setApplicableAccumulators(
            node.getTreeInfo,
            finalMode.getAccumulators)
        }
        SingletonIterator.makeIterator(node)
      } else {
        SingletonIterator.makeIterator(item)
      }

  private def warningIfStreamable(mode: Mode): Unit = {
    if (mode.isDeclaredStreamable) {
      warning((if (initialMode == null) "" else getInitialMode.getModeTitle) +
        " is streamable, but the input is not supplied as a stream",
        SaxonErrorCode.SXWN9000,
        Loc.NONE)
    }
  }

  def callTemplate(initialTemplateName: StructuredQName, out: Receiver): Unit = {
    checkReadiness()
    openMessageEmitter()
    try {
      val dest: ComplexContentOutputter = prepareOutputReceiver(out)
      val initialContext: XPathContextMajor = newXPathContext
      initialContext.createThreadManager()
      initialContext.setOrigin(this)
      if (globalContextItem != null) {
        initialContext.setCurrentIterator(
          new ManualIterator(globalContextItem))
      }
      var ordinaryParams: ParameterSet = null
      if (initialTemplateParams != null) {
        ordinaryParams = new ParameterSet(initialTemplateParams)
      }
      var tunnelParams: ParameterSet = null
      if (initialTemplateTunnelParams != null) {
        tunnelParams = new ParameterSet(initialTemplateTunnelParams)
      }
      val pack: StylesheetPackage =
        executable.getTopLevelPackage.asInstanceOf[StylesheetPackage]
      val initialComponent: Component = pack.getComponent(
        new SymbolicName(StandardNames.XSL_TEMPLATE, initialTemplateName))
      if (initialComponent == null) {
        throw new XPathException(
          "Template " + initialTemplateName.getDisplayName + " does not exist",
          "XTDE0040")
      }
      if (!pack.isImplicitPackage &&
        !(initialComponent.getVisibility == Visibility.PUBLIC ||
          initialComponent.getVisibility == Visibility.FINAL)) {
        throw new XPathException(
          "Template " + initialTemplateName.getDisplayName + " is " +
            initialComponent.getVisibility.show(),
          "XTDE0040")
      }
      val t: NamedTemplate =
        initialComponent.getActor.asInstanceOf[NamedTemplate]
      val c2: XPathContextMajor = initialContext.newContext()
      initialContext.setOrigin(this)
      c2.setCurrentComponent(initialComponent)
      c2.openStackFrame(t.getStackFrameMap)
      c2.setLocalParameters(ordinaryParams)
      c2.setTunnelParameters(tunnelParams)
      var tc: TailCall = t.expand(dest, c2)
      while (tc != null) tc = tc.processLeavingTail()
      initialContext.waitForChildThreads()
      dest.close()
    } catch {
      case err: UncheckedXPathException =>
        handleXPathException(err.getXPathException)

      case err: XPathException => handleXPathException(err)

    } finally {
      if (traceListener != null) {
        traceListener.close()
      }
      closeMessageEmitter()
      inUse = false
    }
  }

  def applyStreamingTemplates(source: Source, out: Receiver): Unit = {
    checkReadiness()
    openMessageEmitter()
    val dest: ComplexContentOutputter = prepareOutputReceiver(out)
    var close: Boolean = false
    try {
      var validationMode: Int = getSchemaValidationMode
      var underSource: Source = source
      if (source.isInstanceOf[AugmentedSource]) {
        close = source.asInstanceOf[AugmentedSource].isPleaseCloseAfterUse
        val localValidate: Int =
          source.asInstanceOf[AugmentedSource].getSchemaValidation
        if (localValidate != Validation.DEFAULT) {
          validationMode = localValidate
        }
        underSource = source.asInstanceOf[AugmentedSource].getContainedSource
      }
      val config: Configuration = getConfiguration
      val s2: Source =
        config.getSourceResolver.resolveSource(underSource, config)
      if (s2 != null) {
        underSource = s2
      }
      if (!(source.isInstanceOf[SAXSource] || source
        .isInstanceOf[StreamSource] ||
        source.isInstanceOf[EventSource])) {
        throw new IllegalArgumentException(
          "Streaming requires a SAXSource, StreamSource, or EventSource")
      }
      if (!initialMode.getActor.isDeclaredStreamable) {
        throw new IllegalArgumentException("Initial mode is not streamable")
      }
      if (source.isInstanceOf[SAXSource] &&
        config.getBooleanProperty(Feature.IGNORE_SAX_SOURCE_PARSER)) {
        source.asInstanceOf[SAXSource].setXMLReader(null)
      }
      val initialContext: XPathContextMajor = newXPathContext
      initialContext.createThreadManager()
      initialContext.setOrigin(this)
      var ordinaryParams: ParameterSet = null
      if (initialTemplateParams != null) {
        ordinaryParams = new ParameterSet(initialTemplateParams)
      }
      var tunnelParams: ParameterSet = null
      if (initialTemplateTunnelParams != null) {
        tunnelParams = new ParameterSet(initialTemplateTunnelParams)
      }
      var despatcher: Receiver = config.makeStreamingTransformer(
        initialMode.getActor,
        ordinaryParams,
        tunnelParams,
        dest,
        initialContext)
      if (config.isStripsAllWhiteSpace || isStylesheetContainingStripSpace) {
        despatcher = makeStripper(despatcher)
      }
      val pipe: PipelineConfiguration = despatcher.getPipelineConfiguration
      pipe.getParseOptions.setSchemaValidationMode(this.validationMode)
      val verbose: Boolean = getConfiguration.isTiming
      if (verbose) {
        getConfiguration.getLogger.info("Streaming " + source.getSystemId)
      }
      try Sender.send(source, despatcher, null)
      catch {
        case e: QuitParsingException =>
          if (verbose) {
            getConfiguration.getLogger.info(
              "Streaming " + source.getSystemId + " : early exit")
          }

      }
      initialContext.waitForChildThreads()
      dest.close()
    } catch {
      case err: TerminationException => {
        if (!err.hasBeenReported) {
          reportFatalError(err)
        }
        throw err
      }

      case err: UncheckedXPathException =>
        handleXPathException(err.getXPathException)

      case err: XPathException => handleXPathException(err)

    } finally {
      inUse = false
      if (close && source.isInstanceOf[AugmentedSource]) {
        source.asInstanceOf[AugmentedSource].close()
      }
      if (traceListener != null) {
        traceListener.close()
      }
    }
  }

  def getStreamingReceiver(mode: Mode, result: Receiver): Receiver = {
    checkReadiness()
    openMessageEmitter()
    val dest: ComplexContentOutputter = prepareOutputReceiver(result)
    val initialContext: XPathContextMajor = newXPathContext
    initialContext.setOrigin(this)
    globalContextItem = null
    if (!mode.isDeclaredStreamable) {
      throw new XPathException(
        "mode supplied to getStreamingReceiver() must be streamable")
    }
    val config: Configuration = getConfiguration
    var despatcher: Receiver =
      config.makeStreamingTransformer(mode, null, null, dest, initialContext)
    if (despatcher == null) {
      throw new XPathException("Streaming requires Saxon-EE")
    }
    if (config.isStripsAllWhiteSpace || isStylesheetContainingStripSpace) {
      despatcher = makeStripper(despatcher)
    }
    despatcher.setPipelineConfiguration(makePipelineConfiguration)
    val finalResult: Outputter = dest
    new ProxyReceiver(despatcher) {
      override def close(): Unit = {
        if (traceListener != null) {
          traceListener.close()
        }
        closeMessageEmitter()
        finalResult.close()
        inUse = false
      }
    }
  }

  private def openMessageEmitter(): Unit = {
    if (explicitMessageReceiver != null) {
      explicitMessageReceiver.open()
      if (explicitMessageReceiver.isInstanceOf[Emitter] &&
        explicitMessageReceiver.asInstanceOf[Emitter].getWriter ==
          null) {
        explicitMessageReceiver
          .asInstanceOf[Emitter]
          .setStreamResult(getConfiguration.getLogger.asStreamResult())
      }
    }
  }

  private def closeMessageEmitter(): Unit = {
    if (explicitMessageReceiver != null) {
      explicitMessageReceiver.close()
    }
  }

  def getAttributeSetEvaluationStack(): Stack[AttributeSet] = synchronized {
    val thread: Long = Thread.currentThread().getId
    attributeSetEvaluationStacks.computeIfAbsent(thread, (k) => new Stack())
  }

  def releaseAttributeSetEvaluationStack(): Unit = {
    synchronized {
      val thread: Long = Thread.currentThread().getId
      attributeSetEvaluationStacks.remove(thread)
    }
  }

}
