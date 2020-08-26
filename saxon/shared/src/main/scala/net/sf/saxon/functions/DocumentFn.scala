package net.sf.saxon.functions

import java.net.{URI, URISyntaxException}

import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.{Source, SourceLocator, TransformerException, URIResolver}
import net.sf.saxon.event.{Builder, PipelineConfiguration, Receiver, Sender}
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser.PathMap
import net.sf.saxon.expr.sort.{DocumentOrderIterator, GlobalOrderComparer}
import net.sf.saxon.functions.DocumentFn._
import net.sf.saxon.lib.{Feature, ParseOptions, RelativeURIResolver, StandardErrorHandler}
import net.sf.saxon.om._
import net.sf.saxon.s9api.Location
import net.sf.saxon.style.StylesheetPackage
import net.sf.saxon.trans.{Err, NonDelegatingURIResolver, XPathException}
import net.sf.saxon.tree.tiny.TinyBuilder
import net.sf.saxon.utils.{Configuration, Controller}
import net.sf.saxon.value.Cardinality

object DocumentFn {

  private class DocumentMappingFunction(var context: XPathContext)
    extends ItemMappingFunction {

    var baseURI: String = _

    var stylesheetURI: String = _

    var locator: Location = _

    var packageData: PackageData = _

    def mapItem(item: Item): Item = {
      var b: String = baseURI
      if (b == null) {
        b =
          if (item.isInstanceOf[NodeInfo])
            item.asInstanceOf[NodeInfo].getBaseURI
          else stylesheetURI
      }
      makeDoc(item.getStringValue,
        b,
        packageData,
        null,
        context,
        locator,
        silent = false)
    }

  }

  def makeDoc(href: String,
              baseURI: String,
              packageData: PackageData,
              options: ParseOptions,
              c: XPathContext,
              locator: Location,
              silent: Boolean): NodeInfo = {
    var hrefStr = href
    var parseOpt = options
    val config: Configuration = c.getConfiguration
    val hash: Int = hrefStr.indexOf('#')
    var fragmentId: String = null
    if (hash >= 0) {
      if (hash == hrefStr.length - 1) {
        hrefStr = hrefStr.substring(0, hash)
      } else {
        fragmentId = hrefStr.substring(hash + 1)
        hrefStr = hrefStr.substring(0, hash)
        if (!NameChecker.isValidNCName(fragmentId)) {
          val de: XPathException = new XPathException(
            "The fragment identifier " + Err
              .wrap(fragmentId) + " is not a valid NCName")
          de.setErrorCode("XTDE1160")
          de.setXPathContext(c)
          de.setLocator(locator)
          throw de
        }
      }
    }
    val controller: Controller = c.getController
    if (controller == null) {
      throw new XPathException(
        "doc() function is not available in this environment")
    }
    val documentKey: DocumentURI =
      computeDocumentKey(hrefStr, baseURI, packageData, c)
    var doc: TreeInfo = config.getGlobalDocumentPool.find(documentKey)
    if (doc != null) {
      doc.getRootNode
    }
    val pool: DocumentPool = controller.getDocumentPool
    controller.synchronized {
      doc = pool.find(documentKey)
      if (doc != null) {
        getFragment(doc, fragmentId, c, locator)
      }
      //      if (controller.isInstanceOf[XsltController] &&
      //        !controller
      //          .asInstanceOf[XsltController]
      //          .checkUniqueOutputDestination(documentKey)) {
      //        pool.markUnavailable(documentKey)
      //        val err: XPathException = new XPathException(
      //          "Cannot read a document that was written during the same transformation: " +
      //            documentKey)
      //        err.setXPathContext(c)
      //        err.setErrorCode("XTRE1500")
      //        err.setLocator(locator)
      //        throw err
      //      }
      if (pool.isMarkedUnavailable(documentKey)) {
        val err: XPathException = new XPathException(
          "Document has been marked not available: " + documentKey)
        err.setXPathContext(c)
        err.setErrorCode("FODC0002")
        err.setLocator(locator)
        throw err
      }
    }
    try {
      var source: Source = resolveURI(hrefStr, baseURI, documentKey.toString, c)
      source = config.getSourceResolver.resolveSource(source, config)
      var newdoc: TreeInfo = null
      if (source.isInstanceOf[NodeInfo] || source.isInstanceOf[DOMSource]) {
        val startNode: NodeInfo = controller.prepareInputTree(source)
        newdoc = startNode.getTreeInfo
      } else {
        val b: Builder = controller.makeBuilder
        b.setUseEventLocation(true)
        if (b.isInstanceOf[TinyBuilder]) {
          b.asInstanceOf[TinyBuilder]
            .setStatistics(config.getTreeStatistics.SOURCE_DOCUMENT_STATISTICS)
        }
        var s: Receiver = b
        if (parseOpt == null) {
          parseOpt = new ParseOptions(
            b.getPipelineConfiguration.getParseOptions)
          if (packageData.isInstanceOf[StylesheetPackage]) {
            val rule: SpaceStrippingRule =
              packageData.asInstanceOf[StylesheetPackage].getSpaceStrippingRule
            if (rule != NoElementsSpaceStrippingRule.getInstance) {
              parseOpt.setSpaceStrippingRule(rule)
            }
          }
          parseOpt.setSchemaValidationMode(controller.getSchemaValidationMode)
        }
        b.getPipelineConfiguration.setParseOptions(parseOpt)
        if (parseOpt.isLineNumbering) {
          b.setLineNumbering(true)
        }
        if (silent) {
          val eh: StandardErrorHandler = new StandardErrorHandler(
            controller.getErrorReporter)
          eh.setSilent(true)
          parseOpt.setErrorHandler(eh)
        }
        if (packageData.isInstanceOf[StylesheetPackage] &&
          packageData
            .asInstanceOf[StylesheetPackage]
            .isStripsTypeAnnotations) {
          s = config.getAnnotationStripper(s)
        }
        val map: PathMap = controller.getPathMapForDocumentProjection
        if (map != null) {
          val pathRoot: PathMap.PathMapRoot =
            map.getRootForDocument(documentKey.toString)
          if (pathRoot != null && !pathRoot.isReturnable && !pathRoot
            .hasUnknownDependencies) {
            parseOpt.addFilter(config.makeDocumentProjector(pathRoot))
          }
        }
        s.setPipelineConfiguration(b.getPipelineConfiguration)
        try {
          Sender.send(source, s, parseOpt)
          newdoc = b.getCurrentRoot.getTreeInfo
          b.reset()
        } catch {
          case err: XPathException => {
            if (err.getErrorCodeLocalPart == null || err.getErrorCodeLocalPart
              .==("SXXP0003")) {
              err.setErrorCode("FODC0002")
            }
            throw err
          }

        } finally if (parseOpt.isPleaseCloseAfterUse) {
          ParseOptions.close(source)
        }
      }
      controller.synchronized {
        doc = pool.find(documentKey)
        if (doc != null) {
          getFragment(doc, fragmentId, c, locator)
        }
        controller.registerDocument(newdoc, documentKey)
        //        if (controller.isInstanceOf[XsltController]) {
        //          controller
        //            .asInstanceOf[XsltController]
        //            .addUnavailableOutputDestination(documentKey)
        //        }
      }
      getFragment(newdoc, fragmentId, c, locator)
    } catch {
      case err: TransformerException => {
        pool.markUnavailable(documentKey)
        val xerr: XPathException = XPathException.makeXPathException(err)
        xerr.maybeSetLocation(locator)
        val code: String =
          if ((err.getException.isInstanceOf[URISyntaxException])) "FODC0005"
          else "FODC0002"
        xerr.maybeSetErrorCode(code)
        throw xerr
      }

    }
  }

  def resolveURI(href: String,
                 baseURI: String,
                 documentKey: String,
                 context: XPathContext): Source = {
    var resolver: URIResolver = context.getURIResolver
    var source: Source = null
    if (baseURI == null) {
      val uri: URI = new URI(href)
      if (!uri.isAbsolute) {
        throw new XPathException(
          "Relative URI passed to document() function (" + href +
            "); but no base URI is available",
          "XTDE1162")
      }
    }
    try source =
      if (resolver.isInstanceOf[RelativeURIResolver] && documentKey != null)
        resolver.asInstanceOf[RelativeURIResolver].dereference(documentKey)
      else resolver.resolve(href, baseURI)
    catch {
      case ex: Exception => {
        val de: XPathException = new XPathException(
          "Exception thrown by URIResolver resolving `" + href +
            "` against `" +
            baseURI +
            "'",
          ex)
        if (context.getConfiguration.getBooleanProperty(
          Feature.TRACE_EXTERNAL_FUNCTIONS)) {
          ex.printStackTrace()
        }
        throw de
      }

    }
    if (source.isInstanceOf[StreamSource] &&
      source.asInstanceOf[StreamSource].getInputStream == null &&
      source.asInstanceOf[StreamSource].getReader == null) {
      val uri: String = source.getSystemId
      if (uri != null) {
        resolver = context.getController.getStandardURIResolver
        source = resolver.resolve(uri, "")
      } else {
        source = null
      }
    }
    if (source == null && !(resolver.isInstanceOf[NonDelegatingURIResolver])) {
      resolver = context.getController.getStandardURIResolver
      source =
        if (resolver.isInstanceOf[RelativeURIResolver] && documentKey != null)
          resolver.asInstanceOf[RelativeURIResolver].dereference(documentKey)
        else resolver.resolve(href, baseURI)
    }
    source
  }

  def computeDocumentKey(href: String,
                         baseURI: String,
                         packageData: PackageData,
                         c: XPathContext): DocumentURI = {
    val controller: Controller = c.getController
    var resolver: URIResolver = controller.getURIResolver
    if (resolver == null) {
      resolver = controller.getStandardURIResolver
    }
    computeDocumentKey(href, baseURI, packageData, resolver, strip = true)
  }

  def computeDocumentKey(href: String,
                         baseURI: String,
                         packageData: PackageData,
                         resolver: URIResolver,
                         strip: Boolean): DocumentURI = {
    var documentKey: String = null
    var hrefStr = href
    if (resolver.isInstanceOf[RelativeURIResolver]) {
      try documentKey =
        resolver.asInstanceOf[RelativeURIResolver].makeAbsolute(hrefStr, baseURI)
      catch {
        case e: TransformerException => documentKey = s"/$hrefStr"

      }
    } else {
      hrefStr = ResolveURI.escapeSpaces(hrefStr)
      if (baseURI == null) {
        try documentKey = new URI(hrefStr).toString
        catch {
          case err: URISyntaxException => documentKey = s"/$hrefStr"

        }
      } else if (hrefStr.isEmpty) {
        documentKey = baseURI
      } else {
        try {
          val uri: URI = new URI(baseURI).resolve(hrefStr)
          documentKey = uri.toString
        } catch {
          case err@(_: URISyntaxException | _: IllegalArgumentException) =>
            documentKey = baseURI + "/../" + hrefStr

        }
      }
    }
    if (strip && packageData.isInstanceOf[StylesheetPackage] &&
      packageData.asInstanceOf[StylesheetPackage].getSpaceStrippingRule !=
        NoElementsSpaceStrippingRule.getInstance) {
      val name: String =
        packageData.asInstanceOf[StylesheetPackage].getPackageName
      if (name != null) {
        documentKey = name + " " +
          " " +
          documentKey //  packageData.asInstanceOf[StylesheetPackage].getPackageVersion
      }
    }
    new DocumentURI(documentKey)
  }

  def preLoadDoc(href: String,
                 baseURI: String,
                 config: Configuration,
                 locator: SourceLocator): NodeInfo = {
    var baseUri = baseURI
    val hash: Int = href.indexOf('#')
    if (hash >= 0) {
      throw new XPathException(
        "Fragment identifier not supported for preloaded documents")
    }
    var documentKey: String = null
    val resolver: URIResolver = config.getURIResolver
    if (resolver.isInstanceOf[RelativeURIResolver]) {
      try documentKey =
        resolver.asInstanceOf[RelativeURIResolver].makeAbsolute(href, baseUri)
      catch {
        case e: TransformerException => {
          documentKey = s"/$href"
          baseUri = ""
        }

      }
    } else {
      if (baseUri == null) {
        try documentKey = new URI(href).toString
        catch {
          case err: URISyntaxException => {
            documentKey =  s"/$href"
            baseUri = ""
          }

        }
      } else if (href.isEmpty) {
        documentKey = baseUri
      } else {
        try {
          val uri: URI = new URI(baseUri).resolve(href)
          documentKey = uri.toString
        } catch {
          case err: URISyntaxException => documentKey = baseUri + "/../" + href

          case err: IllegalArgumentException =>
            documentKey = baseUri + "/../" + href

        }
      }
    }
    val doc: TreeInfo = config.getGlobalDocumentPool.find(documentKey)
    if (doc != null) {
      doc.getRootNode
    }
    try {
      var r: URIResolver = resolver
      var source: Source = null
      if (r != null) {
        try source = r.resolve(href, baseUri)
        catch {
          case ex: Exception => {
            val de: XPathException =
              new XPathException("Exception thrown by URIResolver", ex)
            if (config.getBooleanProperty(Feature.TRACE_EXTERNAL_FUNCTIONS)) {
              ex.printStackTrace()
            }
            de.setLocator(locator)
            throw de
          }

        }
      }
      if (source == null && !(r.isInstanceOf[NonDelegatingURIResolver])) {
        r = config.getSystemURIResolver
        source = r.resolve(href, baseUri)
      }
      source = config.getSourceResolver.resolveSource(source, config)
      val newdoc: TreeInfo = config.buildDocumentTree(source)
      config.getGlobalDocumentPool.add(newdoc, documentKey)
      newdoc.getRootNode
    } catch {
      case err: TransformerException => {
        val xerr: XPathException = XPathException.makeXPathException(err)
        xerr.setLocator(locator)
        xerr.setErrorCode("FODC0002")
        throw new XPathException(err)
      }

    }
  }

  def sendDoc(href: String,
              baseURL: String,
              c: XPathContext,
              locator: Location,
              out: Receiver,
              parseOptions: ParseOptions): Unit = {
    var baseUrl = baseURL
    var pipe: PipelineConfiguration = out.getPipelineConfiguration
    if (pipe == null) {
      pipe = c.getController.makePipelineConfiguration
      pipe.setXPathContext(c)
      out.setPipelineConfiguration(pipe)
    }
    var documentKey: String = null
    if (baseUrl == null) {
      try documentKey = new URI(href).toString
      catch {
        case err: URISyntaxException => {
          documentKey =  s"/$href"
          baseUrl = ""
        }

      }
    } else if (href.isEmpty) {
      documentKey = baseUrl
    } else {
      try {
        val url: URI = new URI(baseUrl).resolve(href)
        documentKey = url.toString
      } catch {
        case err: URISyntaxException => documentKey = baseUrl + "/../" + href

        case err: IllegalArgumentException =>
          documentKey = baseUrl + "/../" + href

      }
    }
    val controller: Controller = c.getController
    val doc: TreeInfo = controller.getDocumentPool.asInstanceOf[DocumentPool].find(documentKey)
    var source: Source = null
    if (doc != null) {
      source = doc.getRootNode
    } else {
      try {
        var r: URIResolver = controller.getURIResolver
        if (r != null) {
          source = r.resolve(href, baseUrl)
        }
        if (source == null) {
          r = controller.getStandardURIResolver
          source = r.resolve(href, baseUrl)
        }
        if (source.isInstanceOf[NodeInfo] || source.isInstanceOf[DOMSource]) {
          val startNode: NodeInfo = controller.prepareInputTree(source)
          source = startNode.getRoot
        }
      } catch {
        case err: TransformerException => {
          val xerr: XPathException = XPathException.makeXPathException(err)
          xerr.setLocator(locator)
          xerr.maybeSetErrorCode("FODC0005")
          throw xerr
        }

      }
    }
    if (controller.getConfiguration.isTiming) {
      controller.getConfiguration.getLogger
        .info("Streaming input document " + source.getSystemId)
    }
    out.setPipelineConfiguration(pipe)
    try Sender.send(source, out, parseOptions)
    catch {
      case e: XPathException => {
        e.maybeSetLocation(locator)
        e.maybeSetErrorCode("FODC0002")
        throw e
      }

    }
  }

  private def getFragment(doc: TreeInfo,
                          fragmentId: String,
                          context: XPathContext,
                          locator: Location): NodeInfo = {
    if (fragmentId == null) {
      doc.getRootNode
    }
    if (!NameChecker.isValidNCName(fragmentId)) {
      context.getController.warning("Invalid fragment identifier in URI",
        "XTDE1160",
        locator)
      doc.getRootNode
    }
    doc.selectID(fragmentId, getParent = false)
  }

}

class DocumentFn extends SystemFunction with Callable {

  private var location: Location = _

  override def getCardinality(arguments: Array[Expression]): Int = {
    val expression: Expression = arguments(0)
    if (Cardinality.allowsMany(expression.getCardinality)) {
      StaticProperty.ALLOWS_ZERO_OR_MORE
    } else {
      StaticProperty.ALLOWS_ZERO_OR_ONE
    }
  }

  override def getSpecialProperties(arguments: Array[Expression]): Int =
    StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET |
      StaticProperty.NO_NODES_NEWLY_CREATED

  override def makeFunctionCall(arguments: Expression*): Expression = {
    location = arguments(0).getLocation
    val expr: Expression = Doc.maybePreEvaluate(this, arguments.toArray)
    if (expr == null) super.makeFunctionCall(arguments: _*) else expr
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val numArgs: Int = getArity
    val hrefSequence: SequenceIterator = arguments(0).iterate()
    var baseURI: String = null
    if (numArgs == 2) {
      val base: NodeInfo = arguments(1).head().asInstanceOf[NodeInfo]
      baseURI = base.getBaseURI
      if (baseURI == null) {
        throw new XPathException(
          "The second argument to document() is a node with no base URI",
          "XTDE1162")
      }
    }
    val map: DocumentMappingFunction = new DocumentMappingFunction(context)
    map.baseURI = baseURI
    map.stylesheetURI = getStaticBaseUriString
    map.packageData = getRetainedStaticContext.getPackageData
    map.locator = location
    val iter: ItemMappingIterator = new ItemMappingIterator(hrefSequence, map)
    SequenceTool.toLazySequence(
      new DocumentOrderIterator(iter, GlobalOrderComparer.getInstance))
  }

}
