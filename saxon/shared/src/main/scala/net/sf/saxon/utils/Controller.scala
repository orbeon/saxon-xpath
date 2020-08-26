package net.sf.saxon.utils

import java.net.{URI, URISyntaxException}
import java.util

import javax.xml.transform.{Source, URIResolver}
import javax.xml.transform.sax.SAXSource
import net.sf.saxon.event._
import net.sf.saxon.expr.instruct.{Bindery, Executable, GlobalParameterSet, GlobalVariable}
import net.sf.saxon.expr.parser.PathMap
import net.sf.saxon.expr.sort.GroupIterator
import net.sf.saxon.expr._
import net.sf.saxon.functions.AccessorFn
import net.sf.saxon.lib._
import net.sf.saxon.model.{Type, Untyped}
import net.sf.saxon.om._
import net.sf.saxon.regex.RegexIterator
import net.sf.saxon.s9api.{HostLanguage, Location}
import net.sf.saxon.trace.TraceEventMulticaster
import net.sf.saxon.trans._
import net.sf.saxon.tree.tiny.TinyBuilder
import net.sf.saxon.tree.wrapper.{SpaceStrippedDocument, SpaceStrippedNode}
import net.sf.saxon.value.{DateTimeValue, SequenceType}
import net.sf.saxon.z.IntHashMap
import org.xml.sax.SAXParseException

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * The Controller underpins Saxon's implementation of the JAXP Transformer class, and represents
 * an executing instance of a transformation or query. Multiple concurrent executions of
 * the same transformation or query will use different Controller instances. This class is
 * therefore not thread-safe.
 * <p>The Controller is serially reusable: when one transformation or query
 * is finished, it can be used to run another. However, there is no advantage in doing this
 * rather than allocating a new Controller each time.</p>
 * <p> A dummy Controller is created when running free-standing XPath expressions.</p>
 * <p>The Controller holds those parts of the dynamic context that do not vary during the course
 * of a transformation or query, or that do not change once their value has been computed.
 * This also includes those parts of the static context that are required at run-time.</p>
 * <p>Many methods on the Controller are designed for internal use and should not be
 * considered stable. From release 8.4 onwards, those methods that are considered sufficiently
 * stable to constitute path of the Saxon public API are labelled with the JavaDoc tag "since":
 * the value indicates the release at which the method was added to the public API.</p>
 * <p>Prior to Saxon 9.6 the Controller implemented (extended) the JAXP {@link Transformer}
 * interface, and advanced applications were able to down-cast the Transformer to a Controller.
 * This is no longer the case. Instead, the JAXP factory delivers an instance of {@link net.sf.saxon.jaxp.TransformerImpl},
 * from which the Controller is accessible if required. Because the Controller is no longer required
 * to implement the JAXP interface, it has been possible to make it less monolithic, so some of the
 * things it did are now done elsewhere: for example, it no longer handles global parameters</p>
 *
 * @since 8.4. From 9.6 this class should no longer be considered a public API. In 9.9, the class
 *        was split into two: XSLT-specific functionality has been moved into the subclass { @link XsltController}.
 */
object Controller {
  val ANONYMOUS_PRINCIPAL_OUTPUT_URI = "dummy:/anonymous/principal/result"
}

class Controller extends ContextOriginator {
  private var config: Configuration = _

   var executable: Executable = _

   var globalContextItem: Item = _

  private var globalContextItemPreset: Boolean = _

  private var binderies: Map[PackageData, Bindery] = _

  private var globalParameters: GlobalParameterSet = _

  private var convertParameters: Boolean = true

  private var globalVariableDependencies: Map[GlobalVariable, Set[GlobalVariable]] = new HashMap()

   var traceListener: TraceListener = _

  private var tracingPaused: Boolean = _

  private var traceFunctionDestination: Logger = _

  private var standardURIResolver: URIResolver = _

  private var userURIResolver: URIResolver = _

   var principalResult: Receiver = _

   var principalResultURI: String = _

  private var unparsedTextResolver: UnparsedTextURIResolver = _

  private var defaultCollectionURI: String = _

  private var errorReporter: ErrorReporter = new StandardErrorReporter()

  // UnfailingErrorListener errorListener;
  private var treeModel: TreeModel = TreeModel.TINY_TREE

  private var sourceDocumentPool: DocumentPool = _

  private var localIndexes: IntHashMap[Map[Long, KeyIndex]] = _

  private var userDataTable: HashMap[String, Any] = _

  private var lastRememberedNode: NodeInfo = null

  private var lastRememberedNumber: Int = -1

  private var currentDateTime: DateTimeValue = _

  private var dateTimePreset: Boolean = false

  private var pathMap: PathMap = null

   var validationMode: Int = Validation.DEFAULT

   var inUse: Boolean = false

  private var stripSourceTrees: Boolean = true

  // boolean buildTree = true;
  private var collectionFinder: CollectionFinder = null

  private var stylesheetCache: StylesheetCache = null

  private var multiThreadedFocusTrackerFactory: Function1[SequenceIterator, FocusTrackingIterator] = _

  private var focusTrackerFactory: Function1[SequenceIterator, FocusTrackingIterator] = (seqItr: SequenceIterator) => new FocusTrackingIterator()

  /**
   * Create a Controller and initialise variables. Note: XSLT applications should
   * create the Controller by using the JAXP newTransformer() method, or in S9API
   * by using XsltExecutable.load()
   *
   * @param config The Configuration used by this Controller
   */
  def this(config: Configuration) {
    this()
    this.config = config
    // create a dummy executable
    executable = new Executable(config)
    sourceDocumentPool = new DocumentPool
    reset()
  }

  /**
   * Create a Controller and initialise variables.
   *
   * @param config     The Configuration used by this Controller
   * @param executable The executable used by this Controller
   */
  def this(config: Configuration, executable: Executable) {
    this()
    this.config = config
    this.executable = executable
    sourceDocumentPool = new DocumentPool
    reset()
  }

  /**
   * <p>Reset this <code>Transformer</code> to its original configuration.</p>
   * <p><code>Transformer</code> is reset to the same state as when it was created with
   * {@link javax.xml.transform.TransformerFactory#newTransformer()},
   * {@link javax.xml.transform.TransformerFactory#newTransformer(javax.xml.transform.Source source)} or
   * {@link javax.xml.transform.Templates#newTransformer()}.
   * <code>reset()</code> is designed to allow the reuse of existing <code>Transformer</code>s
   * thus saving resources associated with the creation of new <code>Transformer</code>s.</p>
   * <p><i>The above is from the JAXP specification. With Saxon, it's unlikely that reusing a Transformer will
   * give any performance benefits over creating a new one. The one case where it might be beneficial is
   * to reuse the document pool (the set of documents that have been loaded using the doc() or document()
   * functions). Therefore, this method does not clear the document pool. If you want to clear the document
   * pool, call the method {@link #clearDocumentPool} as well.</i></p>
   * <p>The reset <code>Transformer</code> is not guaranteed to have the same {@link javax.xml.transform.URIResolver}
   * or {@link javax.xml.transform.ErrorListener} <code>Object</code>s, e.g. {@link Object#equals(Object obj)}.
   * It is guaranteed to have a functionally equal <code>URIResolver</code>
   * and <code>ErrorListener</code>.</p>
   *
   * @since 1.5
   */
  def reset(): Unit = {
    globalParameters = new GlobalParameterSet
    focusTrackerFactory = config.getFocusTrackerFactory(executable, false)
    multiThreadedFocusTrackerFactory = config.getFocusTrackerFactory(executable, true)
    standardURIResolver = config.getSystemURIResolver
    userURIResolver = config.getURIResolver
    unparsedTextResolver = config.getUnparsedTextURIResolver
    validationMode = config.getSchemaValidationMode
    errorReporter = new StandardErrorReporter
    traceListener = null
    traceFunctionDestination = config.getLogger
    var tracer: TraceListener = null
    try tracer = config.makeTraceListener
    catch {
      case err: XPathException =>
        throw new IllegalStateException(err.getMessage)
    }
    if (tracer != null) addTraceListener(tracer)
    setModel(config.getParseOptions.getModel)
    globalContextItem = null
    currentDateTime = null
    dateTimePreset = false
    clearPerTransformationData()
  }

  /**
   * Reset variables that need to be reset for each transformation if the controller
   * is serially reused
   */
   def clearPerTransformationData() = {
    userDataTable = new HashMap[String, AnyRef]()
    principalResult = null
    tracingPaused = false
    lastRememberedNode = null
    lastRememberedNumber = -1
    stylesheetCache = null
    localIndexes = null
    if (!globalContextItemPreset) globalContextItem = null
  }

  /**
   * Get the Configuration associated with this Controller. The Configuration holds
   * settings that potentially apply globally to many different queries and transformations.
   *
   * @return the Configuration object
   * @since 8.4
   */
  def getConfiguration: Configuration = config

  /**
   * Get the value of a supplied parameter (XSLT) or external variable (XQuery)
   *
   * @param name the QName of the parameter
   * @return the supplied value of the parameter, if such a parameter exists, and if a value
   *         was supplied. Returns null if the parameter is not declared or if no value was supplied,
   *         even if there is a default defined in the stylesheet or query.
   */
  def getParameter(name: StructuredQName) = globalParameters.get(name)

  /**
   * Get the value of a parameter, converted and/or type-checked
   *
   * @param name         the name of the stylesheet parameter (XSLT) or external variable (XQuery)
   * @param requiredType the declared type of the parameter
   * @param context      the dynamic evaluation context
   * @return the parameter value if defined, or null otherwise. If the option
   *         { @link #setApplyFunctionConversionRulesToExternalVariables(boolean)}} is set, the supplied
   *         value is converted to the required type. Otherwise, the supplied value is checked
   *         against the required type.
   * @throws XPathException if a problem is found with the supplied parameter value
   */
  @throws[XPathException]
  def getConvertedParameter(name: StructuredQName, requiredType: SequenceType, context: XPathContext): GroundedValue = {
    var `val` = globalParameters.convertParameterValue(name, requiredType, convertParameters, context)
    if (`val` != null) { // Check that any nodes belong to the right configuration
      val config = getConfiguration
      val iter = `val`.iterate()
      var next: Item = null
      while ( {
        (next = iter.next()) != null
      }) if (next.isInstanceOf[NodeInfo] && !config.isCompatible((next.asInstanceOf[NodeInfo]).getConfiguration)) throw new XPathException("A node supplied in a global parameter must be built using the same Configuration " + "that was used to compile the stylesheet or query", SaxonErrorCode.SXXP0004)
      // If the supplied value is a document node, and the document node has a systemID that is an absolute
      // URI, and the absolute URI does not already exist in the document pool, then register it in the document
      // pool, so that the document-uri() function will find it there, and so that a call on doc() will not
      // reload it.
      if (`val`.isInstanceOf[NodeInfo] && `val`.asInstanceOf[NodeInfo].getNodeKind() == Type.DOCUMENT) {
        val systemId = `val`.asInstanceOf[NodeInfo].getRoot().getSystemId
        try if (systemId != null && new URI(systemId).isAbsolute) {
          val pool = getDocumentPool
          if (pool.find(systemId) == null) pool.add(`val`.asInstanceOf[NodeInfo].getTreeInfo(), systemId)
        }
        catch {
          case err: URISyntaxException =>

          // ignore it
        }
      }
      `val` = `val`.materialize
    }
    `val`
  }

  /**
   * Set the base output URI.
   * <p>This defaults to the system ID of the Result object for the principal output
   * of the transformation if this is known; if it is not known, it defaults
   * to the current directory.</p>
   * <p> The base output URI is used for resolving relative URIs in the <code>href</code> attribute
   * of the <code>xsl:result-document</code> instruction.</p>
   *
   * @param uri the base output URI
   * @since 8.4
   */
  def setBaseOutputURI(uri: String) = principalResultURI = uri

  /**
   * Get the base output URI.
   * <p>This returns the value set using the {@link #setBaseOutputURI} method. If no value has been set
   * explicitly, then the method returns null if called before the transformation, or the computed
   * default base output URI if called after the transformation.</p>
   * <p> The base output URI is used for resolving relative URIs in the <code>href</code> attribute
   * of the <code>xsl:result-document</code> instruction.</p>
   *
   * @return the base output URI
   * @since 8.4
   */
  /*@Nullable*/ def getBaseOutputURI = principalResultURI

  /**
   * Get the principal result destination.
   * <p>This method is intended for internal use only. It is typically called by Saxon during the course
   * of a transformation, to discover the result that was supplied in the transform() call.</p>
   *
   * @return the Result object supplied as the principal result destination.
   */
  def getPrincipalResult = principalResult

  /**
   * Allocate a SequenceOutputter for a new output destination.
   *
   * @return the allocated SequenceOutputter
   */
  def allocateSequenceOutputter = {
    val pipe = makePipelineConfiguration
    new SequenceCollector(pipe, 20)
  }

  /**
   * Allocate a SequenceCollector for a new output destination.
   *
   * @param size the estimated size of the output sequence
   * @return SequenceOutputter the allocated SequenceOutputter
   */
  def allocateSequenceOutputter(size: Int) = {
    val pipe = makePipelineConfiguration
    new SequenceCollector(pipe, size)
  }

  /**
   * Make a PipelineConfiguration based on the properties of this Controller.
   * <p>This interface is intended primarily for internal use, although it may be necessary
   * for applications to call it directly if they construct pull or push pipelines.</p>
   *
   * @return a newly constructed PipelineConfiguration holding a reference to this
   *         Controller as well as other configuration information.
   */
  /*@NotNull*/
  def makePipelineConfiguration: PipelineConfiguration = {
    val pipe = config.makePipelineConfiguration
    pipe.setURIResolver(if (userURIResolver == null) standardURIResolver
    else userURIResolver)
    pipe.getParseOptions.setSchemaValidationMode(validationMode) // added in 9.7
    pipe.getParseOptions.setErrorReporter(errorReporter) // added in 9.7.0.4
    pipe.setController(this)
    val executable = getExecutable
    if (executable != null) { // can be null for an IdentityTransformer
      pipe.setHostLanguage(executable.getHostLanguage)
    }
    pipe
  }

  /**
   * Set a callback that will be used when reporting a dynamic error or warning
   *
   * @param reporter the error reporter to be notified
   */
  def setErrorReporter(reporter: ErrorReporter) = errorReporter = reporter

  def getErrorReporter = errorReporter

  /**
   * Report a fatal error
   *
   * @param err the error to be reported
   */
  def reportFatalError(err: XPathException) = if (!err.hasBeenReported) {
    if (err.getHostLanguage == null) if (executable.getHostLanguage eq HostLanguage.XSLT) err.setHostLanguage("XSLT")
    else if (executable.getHostLanguage eq HostLanguage.XQUERY) err.setHostLanguage("XQuery")
    getErrorReporter.report(new XmlProcessingException(err))
    err.setHasBeenReported(true)
  }

  /**
   * Report a run-time warning
   *
   * @param message   the warning message
   * @param errorCode the local part of the error code (in the ERR namespace). May be null.
   * @param locator   the location in the source code. May be null.
   */
  def warning(message: String, errorCode: String, locator: Location) = {
    val warning = new XmlProcessingIncident(message, errorCode, locator).asWarning
    errorReporter.report(warning)
  }

  @throws[XPathException]
   def handleXPathException(err: XPathException) = {
    var cause = err.getException
    if (cause.isInstanceOf[SAXParseException]) { // This generally means the error was already reported.
      // But if a RuntimeException occurs in Saxon during a callback from
      // the Crimson parser, Crimson wraps this in a SAXParseException without
      // reporting it further.
      val spe = cause.asInstanceOf[SAXParseException]
      cause = spe.getException
      if (cause.isInstanceOf[RuntimeException]) reportFatalError(err)
    }
    else reportFatalError(err)
    throw err
  }

  /**
   * Get the Executable object.
   * <p>This method is intended for internal use only.</p>
   *
   * @return the Executable (which represents the compiled stylesheet)
   */
  def getExecutable: Executable = executable

  /**
   * Get the document pool. This is used only for source documents, not for stylesheet modules.
   * <p>This method is intended for internal use only.</p>
   *
   * @return the source document pool
   */
  def getDocumentPool: DocumentPool = sourceDocumentPool

  /**
   * Clear the document pool.
   * This is sometimes useful when re-using the same Transformer
   * for a sequence of transformations, but it isn't done automatically, because when
   * the transformations use common look-up documents, the caching is beneficial.
   */
  def clearDocumentPool(): Unit = {
   /* for (pack <- getExecutable.getPackages.asScala) {
     // sourceDocumentPool.discardIndexes(pack.getKeyManager) // required KeyManager type but this class not exist
    }*/
    sourceDocumentPool = new DocumentPool
  }

  /**
   * Get the bindery for the global variables in a particular package.
   * <p>This method is intended for internal use only.</p>
   *
   * @param packageData the package for which the variables are required
   * @return the Bindery (in which values of all variables for the requested package are held)
   */
  def getBindery(packageData: PackageData) = {
    var b: Bindery = binderies.get(packageData).getOrElse(null)
    if (b == null) {
      b = new Bindery(packageData)
      binderies += (packageData -> b)
    }
    b
  }

  /**
   * Set the item used as the context for evaluating global variables. This value is used
   * as the global context item by XQuery, XSLT and XPath.
   *
   * @param contextItem the context item for evaluating global variables, or null if there is none
   * @throws XPathException if the supplied context item is a node, and if it (a) does not belong
   *                        to the right Configuration, or (b) is schema-validated, when the stylesheet or query is
   *                        not compiled with schema-awareness enabled
   * @since 9.7. Changed in 9.9 to raise an exception if the context item is inappropriate.
   */
  @throws[XPathException]
  def setGlobalContextItem(contextItem: Item): Unit = setGlobalContextItem(contextItem, false)

  /**
   * Set the item used as the context for evaluating global variables. This value is used
   * as the global context item by XQuery, XSLT, and XPath.
   *
   * @param contextItem     the context item for evaluating global variables, or null if there is none
   * @param alreadyStripped true if any stripping of type annotations or whitespace text node specified
   *                        in the stylesheet has already been carried out
   * @throws XPathException if the supplied context item is a node, and if it (a) does not belong
   *                        to the right Configuration, or (b) is schema-validated, when the stylesheet or query is
   *                        not compiled with schema-awareness enabled
   * @since 9.7. Changed in 9.9 to raise an exception if the context item is inappropriate.
   */
  @throws[XPathException]
  def setGlobalContextItem(contextItem: Item, alreadyStripped: Boolean): Unit = {
    var conItem = contextItem
    if (!alreadyStripped) { // Bug 2929 - don't do space-stripping twice
      if (globalContextItem.isInstanceOf[SpaceStrippedNode] && (globalContextItem.asInstanceOf[SpaceStrippedNode].getUnderlyingNode eq conItem)) return
      if (conItem.isInstanceOf[NodeInfo]) { // In XSLT, apply strip-space and strip-type-annotations options
        conItem = prepareInputTree(conItem.asInstanceOf[NodeInfo])
      }
    }
    if (globalContextItem.isInstanceOf[NodeInfo]) {
      val startNode = globalContextItem.asInstanceOf[NodeInfo]
      if (startNode.getConfiguration == null) { // must be a non-standard document implementation
        throw new XPathException("The supplied source document must be associated with a Configuration")
      }
      if (!startNode.getConfiguration.isCompatible(executable.getConfiguration))
        throw new XPathException("Source document and stylesheet must use the same or compatible Configurations", SaxonErrorCode.SXXP0004)
      if (startNode.getTreeInfo().isTyped && !executable.isSchemaAware)
        throw new XPathException("Cannot use a schema-validated source document unless the stylesheet is schema-aware")
    }
    this.globalContextItem = conItem
    this.globalContextItemPreset = true
  }

  /**
   * Reset the global context item to null. This clears any previous setting of the global context
   * item.
   */
  def clearGlobalContextItem() = {
    this.globalContextItem = null
    this.globalContextItemPreset = false
  }

  /**
   * Get the item used as the context for evaluating global variables. In XQuery this
   * is the same as the initial context item; in XSLT 1.0 and 2.0 it is the root of the tree containing
   * the initial context node; in XSLT 3.0 it can be set independently of the initial match selection.
   *
   * @return the context item for evaluating global variables, or null if there is none
   * @since 9.7
   */
  def getGlobalContextItem = {
    globalContextItem
    // See W3C bug 5224, which points out that the rules for XQuery 1.0 weren't clearly defined
  }

  /**
   * Set an object that will be used to resolve URIs used in
   * document(), etc.
   *
   * @param resolver An object that implements the URIResolver interface, or
   *                 null.
   */
  def setURIResolver(resolver: URIResolver) = {
    userURIResolver = resolver
    if (resolver.isInstanceOf[StandardURIResolver]) resolver.asInstanceOf[StandardURIResolver].setConfiguration(getConfiguration)
  }

  /**
   * Get the URI resolver.
   * <p><i>This method changed in Saxon 8.5, to conform to the JAXP specification. If there
   * is no user-specified URIResolver, it now returns null; previously it returned the system
   * default URIResolver.</i></p>
   *
   * @return the user-supplied URI resolver if there is one, or null otherwise.
   */
  def getURIResolver = userURIResolver

  /**
   * Get the fallback URI resolver. This is the URIResolver that Saxon uses when
   * the user-supplied URI resolver returns null.
   * <p>This method is intended for internal use only.</p>
   *
   * @return the the system-defined URIResolver
   */
  def getStandardURIResolver = standardURIResolver

  /**
   * Set an UnparsedTextURIResolver to be used to resolve URIs passed to the XSLT
   * unparsed-text() function.
   *
   * @param resolver the unparsed text URI resolver to be used. This replaces any unparsed text
   *                 URI resolver previously registered.
   * @since 8.9
   */
  def setUnparsedTextURIResolver(resolver: UnparsedTextURIResolver) = unparsedTextResolver = resolver

  /**
   * Get the URI resolver for the unparsed-text() function. This will
   * return the UnparsedTextURIResolver previously set using the {@link #setUnparsedTextURIResolver}
   * method.
   *
   * @return the registered UnparsedTextURIResolver
   * @since 8.9
   */
  def getUnparsedTextURIResolver: UnparsedTextURIResolver = unparsedTextResolver

  /**
   * Get the collection finder associated with this configuration. This is used to dereference
   * collection URIs used in the fn:collection and fn:uri-collection functions
   *
   * @return the CollectionFinder to be used
   * @since 9.7
   */
  def getCollectionFinder = {
    if (collectionFinder == null) collectionFinder = config.getCollectionFinder
    collectionFinder
  }

  /**
   * Set the collection finder associated with this configuration. This is used to dereference
   * collection URIs used in the fn:collection and fn:uri-collection functions
   *
   * @param cf the CollectionFinder to be used
   * @since 9.7
   */
  def setCollectionFinder(cf: CollectionFinder) = collectionFinder = cf

  /**
   * Set the name of the default collection. Defaults to the default collection
   * name registered with the Configuration.
   *
   * @param uri the collection URI of the default collection. May be null, to cause
   *            fallback to the collection name registered with the Configuration. The name will be passed
   *            to the collection URI resolver to identify the documents in the collection, unless
   *            the name is <code>http://saxon.sf.net/collection/empty</code> which always refers
   *            to the empty collection.
   * @since 9.4
   */
  def setDefaultCollection(uri: String) = defaultCollectionURI = uri

  /**
   * Get the name of the default collection. Defaults to the default collection
   * name registered with the Configuration.
   *
   * @return the collection URI of the default collection. If no value has been
   *         set explicitly, the collection URI registered with the Configuration is returned
   * @since 9.4
   */
  def getDefaultCollection = if (defaultCollectionURI == null) getConfiguration.getDefaultCollection
  else defaultCollectionURI

  /**
   * Ask whether source documents loaded using the doc(), document(), and collection()
   * functions, or supplied as a StreamSource or SAXSource to the transform() or addParameter() method
   * should be subjected to schema validation
   *
   * @return the schema validation mode previously set using setSchemaValidationMode(),
   *         or the default mode (derived from the global Configuration) otherwise.
   */
  def getSchemaValidationMode = validationMode

  /**
   * Say whether source documents loaded using the doc(), document(), and collection()
   * functions, or supplied as a StreamSource or SAXSource to the transform() or addParameter() method,
   * should be subjected to schema validation. The default value is taken
   * from the corresponding property of the Configuration.
   *
   * @param validationMode the validation (or construction) mode to be used for source documents.
   *                       One of { @link Validation#STRIP}, { @link Validation#PRESERVE}, { @link Validation#STRICT},
   *                       { @link Validation#LAX}
   * @since 9.2
   */
  def setSchemaValidationMode(validationMode: Int) = this.validationMode = validationMode

  /**
   * Set the tree model to use. Default is the tiny tree
   *
   * @param model typically one of the constants { @link net.sf.saxon.om.TreeModel#TINY_TREE},
   *              { @link net.sf.saxon.om.TreeModel#TINY_TREE_CONDENSED}, or { @link net.sf.saxon.om.TreeModel#LINKED_TREE}.
   *              It is also possible to use a user-defined tree model.
   * @since 9.2
   */
  def setModel(model: TreeModel) = treeModel = model

  /**
   * Get the tree model that will be used.
   *
   * @return typically one of the constants { @link net.sf.saxon.om.TreeModel#TINY_TREE},
   *         { @link TreeModel#TINY_TREE_CONDENSED}, or { @link TreeModel#LINKED_TREE}.
   *         It is also possible to use a user-defined tree model.
   * @since 9.2
   */
  def getModel = treeModel

  /**
   * Make a builder for the selected tree model.
   *
   * @return an instance of the Builder for the chosen tree model
   * @since 8.4
   */
  def makeBuilder = {
    val b = treeModel.makeBuilder(makePipelineConfiguration)
    b.setTiming(config.isTiming)
    b.setLineNumbering(config.isLineNumbering)
    b
  }

  /**
   * Say whether the transformation should perform whitespace stripping as defined
   * by the xsl:strip-space and xsl:preserve-space declarations in the stylesheet
   * in the case where a source tree is supplied to the transformation as a tree
   * (typically a DOMSource, or a Saxon NodeInfo).
   * The default is true. It is legitimate to suppress whitespace
   * stripping if the client knows that all unnecessary whitespace has already been removed
   * from the tree before it is processed. Note that this option applies to all source
   * documents for which whitespace-stripping is normally applied, that is, both the
   * principal source documents, and documents read using the doc(), document(), and
   * collection() functions. It does not apply to source documents that are supplied
   * in the form of a SAXSource or StreamSource, for which whitespace is stripped
   * during the process of tree construction.
   * <p>Generally, stripping whitespace speeds up the transformation if it is done
   * while building the source tree, but slows it down if it is applied to a tree that
   * has already been built. So if the same source tree is used as input to a number
   * of transformations, it is better to strip the whitespace once at the time of
   * tree construction, rather than doing it on-the-fly during each transformation.</p>
   *
   * @param strip true if whitespace is to be stripped from supplied source trees
   *              as defined by xsl:strip-space; false to suppress whitespace stripping
   * @since 9.3
   */
  def setStripSourceTrees(strip: Boolean) = stripSourceTrees = strip

  /**
   * Ask whether the transformation will perform whitespace stripping for supplied source trees as defined
   * by the xsl:strip-space and xsl:preserve-space declarations in the stylesheet.
   *
   * @return true unless whitespace stripping has been suppressed using
   *         { @link #setStripSourceTrees(boolean)}.
   * @since 9.3
   */
  def isStripSourceTree = stripSourceTrees

  /**
   * Ask whether the executable is a stylesheet whose top-level package
   * contains an xsl:strip-space declaration requesting stripping of whitespace
   * from the principal source document to the transformation
   *
   * @return true if whitespace stripping has been requested
   */
   def isStylesheetContainingStripSpace = {
    var rule = null
   /* executable.isInstanceOf[PreparedStylesheet] && (rule = executable.asInstanceOf[PreparedStylesheet].getTopLevelPackage.getSpaceStrippingRule) != null &&*/
      rule != NoElementsSpaceStrippingRule.getInstance // PreparedStylesheet class not exist
  }

  /**
   * Ask whether the executable is a stylesheet whose top-level package
   * contains requests stripping of type annotations
   *
   * @return true if stripping of type annotations has been requested
   */
  /*def isStylesheetStrippingTypeAnnotations = executable.isInstanceOf[PreparedStylesheet] &&
    executable.asInstanceOf[PreparedStylesheet].getTopLevelPackage.isStripsTypeAnnotations // PreparedStylesheet class not exist*/

  /**
   * Make a Stripper configured to implement the whitespace stripping rules.
   * In the case of XSLT the whitespace stripping rules are normally defined
   * by <code>xsl:strip-space</code> and <code>xsl:preserve-space</code> elements
   * in the stylesheet. Alternatively, stripping of all whitespace text nodes
   * may be defined at the level of the Configuration, using the code
   * {@code Configuration.getParseOptions().setSpaceStrippingRules(AllElementsSpaceStrippingRule.getInstance()}.
   *
   * @param next the Receiver to which the events filtered by this stripper are
   *             to be sent (often a Builder). May be null if the stripper is not being used for filtering
   *             into a Builder or other Receiver.
   * @return the required Stripper. A Stripper may be used in two ways. It acts as
   *         a filter applied to an event stream, that can be used to remove the events
   *         representing whitespace text nodes before they reach a Builder. Alternatively,
   *         it can be used to define a view of an existing tree in which the whitespace
   *         text nodes are dynamically skipped while navigating the XPath axes.
   * @since 8.4 - Generalized in 8.5 to accept any Receiver as an argument
   */
  def makeStripper(next: Receiver) = {
    var rec = next
    if (rec == null)
      rec = new Sink(makePipelineConfiguration)
    new Stripper(getSpaceStrippingRule, rec)
  }

  /**
   * Return the default whitespace-stripping rules that apply to this transformation or query.
   *
   * @return If the configuration-level whitespace-stripping rule is to strip whitespace for
   *         all elements, then AllElementsSpaceStrippingRule.getInstance(). Otherwise,
   */
  def getSpaceStrippingRule: SpaceStrippingRule = {
    if (config.getParseOptions.getSpaceStrippingRule eq AllElementsSpaceStrippingRule.getInstance) return AllElementsSpaceStrippingRule.getInstance
   /* else if (executable.isInstanceOf[PreparedStylesheet]) {
      val rule = executable.asInstanceOf[PreparedStylesheet].getTopLevelPackage.getSpaceStrippingRule // // PreparedStylesheet class not exist
      if (rule != null) return rule
    }*/
    NoElementsSpaceStrippingRule.getInstance
  }

  /**
   * Add a document to the document pool, and check that it is suitable for use in this query or
   * transformation. This check rejects the document if document has been validated (and thus carries
   * type annotations) but the query or transformation is not schema-aware.
   * <p>This method is intended for internal use only.</p>
   *
   * @param doc the root node of the document to be added. Must not be null.
   * @param uri the document-URI property of this document. If non-null, the document is registered
   *            in the document pool with this as its document URI.
   * @throws XPathException if an error occurs
   */
  @throws[XPathException]
  def registerDocument(doc: TreeInfo, uri: DocumentURI): Unit = {
    if (!getExecutable.isSchemaAware && !Untyped.getInstance.equals(doc.getRootNode().getSchemaType)) {
      val isXSLT = getExecutable.getHostLanguage eq HostLanguage.XSLT
      var message: String = null
      if (isXSLT) message = "The source document has been schema-validated, but" + " the stylesheet is not schema-aware. A stylesheet is schema-aware if" + " either (a) it contains an xsl:import-schema declaration, or (b) the stylesheet compiler" + " was configured to be schema-aware."
      else message = "The source document has been schema-validated, but" + " the query is not schema-aware. A query is schema-aware if" + " either (a) it contains an 'import schema' declaration, or (b) the query compiler" + " was configured to be schema-aware."
      throw new XPathException(message)
    }
    if (uri != null) sourceDocumentPool.add(doc, uri)
  }

  /**
   * Get the Rule Manager.
   * <p>This method is intended for internal use only.</p>
   *
   * @return the Rule Manager, used to hold details of template rules for
   *         all modes; or null in the case of a non-XSLT controller
   */
  /*def getRuleManager = {
    val exec = getExecutable
    if (exec.isInstanceOf[PreparedStylesheet]) getExecutable.asInstanceOf[PreparedStylesheet].getRuleManager // PreparedStylesheet class does not exist
    else null
  }*/

  /**
   * Set a TraceListener, replacing any existing TraceListener
   * <p>This method has no effect unless the stylesheet or query was compiled
   * with tracing enabled.</p>
   *
   * @param listener the TraceListener to be set. May be null, in which case
   *                 trace events will not be reported
   * @since 9.2
   */
  def setTraceListener(listener: TraceListener): Unit = this.traceListener = listener

  /**
   * Get the TraceListener. By default, there is no TraceListener, and this
   * method returns null. A TraceListener may be added using the method
   * {@link #addTraceListener}. If more than one TraceListener has been added,
   * this method will return a composite TraceListener. Because the form
   * this takes is implementation-dependent, this method is not part of the
   * stable Saxon public API.
   *
   * @return the TraceListener used for XSLT or XQuery instruction tracing, or null if absent.
   */
  def getTraceListener: TraceListener = traceListener

  /**
   * Test whether instruction execution is being traced. This will be true
   * if (a) at least one TraceListener has been registered using the
   * {@link #addTraceListener} method, and (b) tracing has not been temporarily
   * paused using the {@link #pauseTracing} method.
   *
   * @return true if tracing is active, false otherwise
   * @since 8.4
   */
  final def isTracing: Boolean = traceListener != null && !tracingPaused

  /**
   * Pause or resume tracing. While tracing is paused, trace events are not sent to any
   * of the registered TraceListeners.
   *
   * @param pause true if tracing is to pause; false if it is to resume
   * @since 8.4
   */
  final def pauseTracing(pause: Boolean): Unit = tracingPaused = pause

  /**
   * Adds the specified trace listener to receive trace events from
   * this instance. Note that although TraceListeners can be added
   * or removed dynamically, this has no effect unless the stylesheet
   * or query has been compiled with tracing enabled. This is achieved
   * by calling {@link Configuration#setTraceListener} or by setting
   * the attribute {@link net.sf.saxon.lib.FeatureKeys#TRACE_LISTENER} on the
   * TransformerFactory. Conversely, if this property has been set in the
   * Configuration or TransformerFactory, the TraceListener will automatically
   * be added to every Controller that uses that Configuration.
   *
   * @param trace the trace listener. If null is supplied, the call has no effect.
   * @since 8.4
   */
  def addTraceListener(trace: TraceListener): Unit = if (trace != null) traceListener = TraceEventMulticaster.add(traceListener, trace)

  /**
   * Removes the specified trace listener so that the listener will no longer
   * receive trace events.
   *
   * @param trace the trace listener.
   * @since 8.4
   */
  def removeTraceListener(trace: TraceListener): Unit = traceListener = TraceEventMulticaster.remove(traceListener, trace)

  /**
   * Set the destination for output from the fn:trace() function.
   * By default, the destination is System.err. If a TraceListener is in use,
   * this is ignored, and the trace() output is sent to the TraceListener.
   *
   * @param stream the PrintStream to which trace output will be sent. If set to
   *               null, trace output is suppressed entirely. It is the caller's responsibility
   *               to close the stream after use.
   * @since 9.1. Changed in 9.6 to use a Logger
   */
  def setTraceFunctionDestination(stream: Logger): Unit = traceFunctionDestination = stream

  /**
   * Get the destination for output from the fn:trace() function.
   *
   * @return the PrintStream to which trace output will be sent. If no explicitly
   *         destination has been set, returns System.err. If the destination has been set
   *         to null to suppress trace output, returns null.
   * @since 9.1. Changed in 9.6 to use a Logger
   */
  def getTraceFunctionDestination: Logger = traceFunctionDestination

  /**
   * Initialize the controller ready for a new transformation. This method should not normally be called by
   * users (it is done automatically when transform() is invoked). However, it is available as a low-level API
   * especially for use with XQuery.
   *
   * @param params the values of stylesheet parameters. Changed in 9.9.1.1 so this no longer includes
   *               static parameters (which are already available in the { @link PreparedStylesheet}).
   * @throws XPathException if an error occurs, for example if a required parameter is not supplied.
   */
  @throws[XPathException]
  def initializeController(params: GlobalParameterSet): Unit = { // get a new bindery, to clear out any variables from previous runs
    binderies = new HashMap[PackageData, Bindery]()
    // if parameters were supplied, set them up
    try executable.checkSuppliedParameters(params)
    catch {
      case e: XPathException =>
        if (!e.hasBeenReported) {
          getErrorReporter.report(new XmlProcessingException(e))
          throw e
        }
    }
    globalParameters = params
    // Check the global context item
    globalContextItem = executable.checkInitialContextItem(globalContextItem, newXPathContext)
    if (traceListener != null) {
      traceListener.open(this)
      preEvaluateGlobals(newXPathContext)
    }
  }

  def setApplyFunctionConversionRulesToExternalVariables(applyConversionRules: Boolean): Unit = {
    convertParameters = applyConversionRules
  }

  /**
   * Get user data associated with a key. To retrieve user data, two objects are required:
   * an arbitrary object that may be regarded as the container of the data (originally, and
   * typically still, a node in a tree), and a name. The name serves to distingush data objects
   * associated with the same node by different client applications.
   * <p>This method is intended primarily for internal use, though it may also be
   * used by advanced applications.</p>
   *
   * @param key  an object acting as a key for this user data value. This must be equal
   *             (in the sense of the equals() method) to the key supplied when the data value was
   *             registered using { @link #setUserData}.
   * @param name the name of the required property
   * @return the value of the required property
   */
  def getUserData(key: Any, name: String): Any = {
    val keyValue = key.hashCode + " " + name
    // System.err.println("getUserData " + name + " on object returning " + userDataTable.get(key));
    userDataTable.get(keyValue).getOrElse(null)
  }

  /**
   * Set user data associated with a key. To store user data, two objects are required:
   * an arbitrary object that may be regarded as the container of the data (originally, and
   * typically still, a node in a tree), and a name. The name serves to distingush data objects
   * associated with the same node by different client applications.
   * <p>This method is intended primarily for internal use, though it may also be
   * used by advanced applications.</p>
   *
   * @param key  an object acting as a key for this user data value. This can be any object, for example
   *             a node or a string. If data for the given object and name already exists, it is overwritten.
   * @param name the name of the required property
   * @param data the value of the required property. If null is supplied, any existing entry
   *             for the key is removed.
   */
  def setUserData(key: Any, name: String, data: Any): Unit = { // System.err.println("setUserData " + name + " on object to " + data);
    val keyVal: String = key.hashCode + " " + name
    if (data == null)
      userDataTable -= keyVal
    else
      userDataTable += (keyVal -> data)
  }

  /**
   * Get the table of local indexes supporting xsl:key (or implicit keys created
   * by the optimizer). Indexes are held at Controller level (rather than being
   * shared across transformations) if the key definition is dependent on local
   * information, for example stylesheet parameters.
   *
   * @return the index of indexes. The master index is created if it does not
   *         already exist. The master index is a two-level index: the first level is indexed
   *         by the integer fingerprint of the key name; the second level is indexed by
   *         the document number (a long) for the specific document or temporary tree.
   */
  def getLocalIndexes: IntHashMap[Map[Long, KeyIndex]] = {
    if (localIndexes == null) localIndexes = new IntHashMap[Map[Long, KeyIndex]]
    localIndexes
  }

  /**
   * Set the last remembered node, for node numbering purposes.
   * <p>This method is strictly for internal use only.</p>
   *
   * @param node   the node in question
   * @param number the number of this node
   */
  def setRememberedNumber(node: NodeInfo, number: Int): Unit = {
    lastRememberedNode = node
    lastRememberedNumber = number
  }

  /**
   * Get the number of a node if it is the last remembered one.
   * <p>This method is strictly for internal use only.</p>
   *
   * @param node the node for which remembered information is required
   * @return the number of this node if known, else -1.
   */
  def getRememberedNumber(node: NodeInfo): Int = {
    if (lastRememberedNode eq node) return lastRememberedNumber
    -1
  }

  @throws[XPathException]
   def checkReadiness(): Unit = {
    if (inUse) throw new IllegalStateException("The Controller is being used recursively or concurrently. This is not permitted.")
    if (binderies == null) throw new IllegalStateException("The Controller has not been initialized")
    inUse = true
    clearPerTransformationData()
    if (executable == null) throw new XPathException("Stylesheet has not been prepared")
    if (!dateTimePreset) currentDateTime = null // reset at start of each transformation
  }

  /**
   * Make a source tree from a source supplied as a StreamSource or SAXSource
   *
   * @param source         the source
   * @param validationMode indicates whether the source should be schema-validated
   * @return the root of the constructed tree
   * @throws XPathException if tree construction fails
   */
  @throws[XPathException]
  def makeSourceTree(source: Source, validationMode: Int): NodeInfo = {
    if (source.isInstanceOf[SAXSource] && config.getBooleanProperty(Feature.IGNORE_SAX_SOURCE_PARSER)) { // This option is provided to allow the parser set by applications such as Ant to be overridden by
      // the parser requested using FeatureKeys.SOURCE_PARSER
      source.asInstanceOf[SAXSource].setXMLReader(null)
    }
    val sourceBuilder = makeBuilder
    sourceBuilder.setUseEventLocation(true)
    if (sourceBuilder.isInstanceOf[TinyBuilder]) sourceBuilder.asInstanceOf[TinyBuilder].setStatistics(config.getTreeStatistics.SOURCE_DOCUMENT_STATISTICS)
    var r: Receiver = sourceBuilder
    var spaceStrippingRule: SpaceStrippingRule = NoElementsSpaceStrippingRule.getInstance
    if (config.isStripsAllWhiteSpace || isStylesheetContainingStripSpace || validationMode == Validation.STRICT || validationMode == Validation.LAX) {
      r = makeStripper(sourceBuilder)
      spaceStrippingRule = getSpaceStrippingRule
    }
   /* if (isStylesheetStrippingTypeAnnotations) r = config.getAnnotationStripper(r)*/
    val pipe = sourceBuilder.getPipelineConfiguration
    pipe.getParseOptions.setSchemaValidationMode(validationMode)
    r.setPipelineConfiguration(pipe)
    Sender.send(source, r, null)
    if (source.isInstanceOf[AugmentedSource] && source.asInstanceOf[AugmentedSource].isPleaseCloseAfterUse) source.asInstanceOf[AugmentedSource].close()
    val doc = sourceBuilder.getCurrentRoot
    //globalContextItem = doc;
    sourceBuilder.reset()
    if (source.getSystemId != null) registerDocument(doc.getTreeInfo(), new DocumentURI(source.getSystemId))
    doc.getTreeInfo().setSpaceStrippingRule(spaceStrippingRule)
    doc
  }

  /**
   * Prepare an input tree for processing. This is used when either the initial
   * input, or a Source returned by the document() function, is a NodeInfo or a
   * DOMSource. The preparation consists of wrapping a DOM document inside a wrapper
   * that implements the NodeInfo interface, and/or adding a space-stripping wrapper
   * if the stylesheet strips whitespace nodes, and/or adding a type-stripping wrapper
   * if the stylesheet strips input type annotations.
   * <p>This method is intended for internal use.</p>
   *
   * @param source the input tree. Must be either a DOMSource or a NodeInfo
   * @return the NodeInfo representing the input node, suitably wrapped. Exceptionally,
   *         the the source is a whitespace text node that is itself stripped, return null.
   */
  def prepareInputTree(source: Source): NodeInfo = {
    var start = getConfiguration.unravel(source)
    // Stripping type annotations happens before stripping of whitespace
   /* if (false) {
      val docInfo = start.getTreeInfo()
      if (docInfo.isTyped) {
        val strippedDoc = new TypeStrippedDocument(docInfo)
        start = strippedDoc.wrap(start)
      }
    }*/
    if (stripSourceTrees && isStylesheetContainingStripSpace) {
      val docInfo = start.getTreeInfo()
      val spaceStrippingRule = getSpaceStrippingRule
      if (docInfo.getSpaceStrippingRule() != spaceStrippingRule) { // if not already space-stripped
        val strippedDoc = new SpaceStrippedDocument(docInfo, spaceStrippingRule)
        // Edge case: the global context item might itself be a whitespace text node that is stripped
        if (!SpaceStrippedNode.isPreservedNode(start, strippedDoc, start.getParent())) return null
        start = strippedDoc.wrap(start)
      }
    }
    start
  }

  /**
   * Pre-evaluate global variables (when debugging/tracing).
   * <p>This method is intended for internal use.</p>
   *
   * @param context the dynamic context for evaluating the global variables
   */
  @throws[XPathException]
  def preEvaluateGlobals(context: XPathContext): Unit = {
    for (pack <- getExecutable.getPackages.asScala) {
      for (glVar <- pack.getGlobalVariableList.asScala) {
        if (!glVar.isUnused) try glVar.evaluateVariable(context, glVar.getDeclaringComponent)
        catch {
          case err: XPathException =>
            // Don't report an exception unless the variable is actually evaluated
            getBindery(glVar.getPackageData).setGlobalVariable(glVar, new Bindery.FailureValue(err))
        }
      }
    }
  }

  /**
   * Register the dependency of one variable ("one") upon another ("two"), throwing an exception if this would establish
   * a cycle of dependencies.
   *
   * @param one the first (dependent) variable
   * @param two the second (dependee) variable
   * @throws XPathException if adding this dependency creates a cycle of dependencies among global variables.
   */
  @throws[XPathException]
  def registerGlobalVariableDependency(one: GlobalVariable, two: GlobalVariable): Unit = {
    if (one eq two) throw new XPathException.Circularity("Circular dependency among global variables: " + one.getVariableQName.getDisplayName + " depends on its own value")
    val transitiveDependencies: Set[GlobalVariable] = globalVariableDependencies.get(two).get
    if (transitiveDependencies != null) {
      if (transitiveDependencies.contains(one)) throw new XPathException.Circularity("Circular dependency among variables: " + one.getVariableQName.getDisplayName + " depends on the value of " + two.getVariableQName.getDisplayName + ", which depends directly or indirectly on the value of " + one.getVariableQName.getDisplayName)
      for (glVar <- transitiveDependencies) { // register the transitive dependencies
        registerGlobalVariableDependency(one, glVar)
      }
    }
    val existingDependencies: mutable.Set[GlobalVariable] = globalVariableDependencies.asInstanceOf[util.Map[instruct.GlobalVariable, util.Set[instruct.GlobalVariable]]]
      .computeIfAbsent(one, (k: GlobalVariable) => new util.HashSet[GlobalVariable]).asScala
    existingDependencies(two)
  }

  @throws[XPathException]
  def setCurrentDateTime(dateTime: DateTimeValue): Unit = if (currentDateTime == null) {
    if (dateTime.getComponent(AccessorFn.Component.TIMEZONE) == null) throw new XPathException("No timezone is present in supplied value of current date/time")
    currentDateTime = dateTime
    dateTimePreset = true
  }
  else throw new IllegalStateException("Current date and time can only be set once, and cannot subsequently be changed")

  /**
   * Get the current date and time for this query or transformation.
   * All calls during one transformation return the same answer.
   *
   * @return Get the current date and time. This will deliver the same value
   *         for repeated calls within the same transformation. The returned dateTime
   *         value will have a timezone, which will be the default/local timezone
   *         determined by the platform on which the application is running.
   */
  def getCurrentDateTime: DateTimeValue = {
    if (currentDateTime == null) currentDateTime = DateTimeValue.now
    currentDateTime
  }

  /**
   * Get the implicit timezone offset for this query or transformation
   *
   * @return the implicit timezone as an offset in minutes. This will be the default/local timezone
   *         determined by the platform on which the application is running. The value will be unchanged for
   *         repeated calls within the same transformation.
   */
  def getImplicitTimezone: Int = getCurrentDateTime.getTimezoneInMinutes

  /**
   * Make an XPathContext object for expression evaluation.
   * <p>This method is intended for internal use.</p>
   *
   * @return the new XPathContext
   */
  def newXPathContext: XPathContextMajor = {
    val c = new XPathContextMajor(this)
    c.setCurrentOutputUri(principalResultURI)
    c
  }

  /**
   * Indicate whether document projection should be used, and supply the PathMap used to control it.
   * Note: this is available only under Saxon-EE.
   *
   * @param pathMap a path map to be used for projecting source documents
   */
  def setUseDocumentProjection(pathMap: PathMap): Unit = this.pathMap = pathMap

  /**
   * Get the path map used for document projection, if any.
   *
   * @return the path map to be used for document projection, if one has been supplied; otherwise null
   */
  def getPathMapForDocumentProjection: PathMap = pathMap

  /**
   * Get the cache of stylesheets (cached during calls on fn:transform()) for this query or transformation.
   *
   * @return the stylesheet cache
   */
  def getStylesheetCache: StylesheetCache = {
    if (stylesheetCache == null) this.stylesheetCache = new StylesheetCache
    stylesheetCache
  }

  /**
   * Get the factory function that is used to create new instances of FocusTrackingIterator.
   * The standard function for instantiating a FocusTrackingIterator can be overridden to deliver
   * one with extra diagnostic capability for use in debuggers
   *
   * @param multithreaded true if the focus tracker must be suitable for executing a multi-threaded
   *                      xsl:for-each iteration
   * @return a factory function that is used to create FocusTrackingIterator instances
   */
  def getFocusTrackerFactory(multithreaded: Boolean): Function1[SequenceIterator, FocusTrackingIterator] =
    if (multithreaded && multiThreadedFocusTrackerFactory != null) multiThreadedFocusTrackerFactory
    else focusTrackerFactory

  /**
   * Set a factory function that will be used to create new instances of FocusTrackingIterator.
   * The standard function for instantiating a FocusTrackingIterator can be overridden to deliver
   * one with extra diagnostic capability for use in debuggers.
   *
   * @param focusTrackerFactory a factory function that is used to create FocusTrackingIterator instances
   */
  def setFocusTrackerFactory(focusTrackerFactory: Function1[SequenceIterator, FocusTrackingIterator]): Unit =
    this.focusTrackerFactory = focusTrackerFactory

  /**
   * Set a factory function that will be used to create new instances of FocusTrackingIterator for
   * multithreaded operation.
   * The standard function for instantiating a FocusTrackingIterator can be overridden to deliver
   * one with extra diagnostic capability for use in debuggers.
   *
   * @param focusTrackerFactory a factory function that is used to create FocusTrackingIterator instances
   */
  def setMultithreadedFocusTrackerFactory(focusTrackerFactory: Function1[SequenceIterator, FocusTrackingIterator]): Unit =
    this.multiThreadedFocusTrackerFactory = focusTrackerFactory

  /**
   * Set the focus tracker factory function to a function that creates a memoizing focus tracker, which
   * has the effect that all items read by the focus iterator are accessible to a debugger at any stage
   * while iterating over the sequence
   */
  def setMemoizingFocusTrackerFactory(): Unit = setFocusTrackerFactory((base: SequenceIterator) => {
    def foo(base: SequenceIterator) = {
      var fti: FocusTrackingIterator = null
      if (!base.getProperties.contains(SequenceIterator.Property.GROUNDED) && !base.isInstanceOf[GroupIterator] && !base.isInstanceOf[RegexIterator]) try {
        val ms = new MemoSequence(base)
        fti = FocusTrackingIterator.track(ms.iterate)
      } catch {
        case e: XPathException =>
          fti = FocusTrackingIterator.track(base)
      }
      else fti = FocusTrackingIterator.track(base)
      fti
    }

    foo(base)
  })
}
