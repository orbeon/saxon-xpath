package net.sf.saxon.expr.instruct

import java.util.{ArrayList, HashMap, Map, Properties}

import javax.xml.transform.dom.DOMResult
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{OutputKeys, Result}
import net.sf.saxon.event.Outputter
import net.sf.saxon.expr._
import net.sf.saxon.expr.instruct.ResultDocument._
import net.sf.saxon.expr.parser._
import net.sf.saxon.lib._
import net.sf.saxon.model.{ErrorType, ItemType, SchemaType}
import net.sf.saxon.om._
import net.sf.saxon.serialize.CharacterMapIndex
import net.sf.saxon.style.StylesheetPackage
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.{Err, XPathException, XmlProcessingException}
import net.sf.saxon.utils.{Configuration, Controller}
import net.sf.saxon.value.Whitespace

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.jdk.CollectionConverters._

object ResultDocument {

  //  def makeReceiver(hrefValue: String,
  //                   baseURI: String,
  //                   context: XPathContext,
  //                   resolver: ResultDocumentResolver,
  //                   params: SerializationProperties,
  //                   resolveAgainstStaticBase: Boolean): Receiver = {
  //    val resultURI: String = null
  //    val controller: Controller = context.getController
  //    var base: String = null
  //    base =
  //      if (resolveAgainstStaticBase) baseURI else controller.getBaseOutputURI
  //    try {
  //      val out: Receiver = resolver.resolve(context, hrefValue, base, params)
  //      var systemId: String = out.getSystemId
  //      if (systemId == null) {
  //        systemId = ResolveURI.makeAbsolute(hrefValue, base).toASCIIString()
  //        out.setSystemId(systemId)
  //      }
  //      checkAcceptableUri(context, systemId)
  //      out
  //    } catch {
  //      case e: XPathException => throw e
  //
  //      case err: Exception => {
  //        err.printStackTrace()
  //        throw new XPathException("Exception thrown by output resolver", err)
  //      }
  //
  //    }
  //  }

  def traceDestination(context: XPathContext, result: Result): Unit = {
    val config: Configuration = context.getConfiguration
    val timing: Boolean = config.isTiming
    if (timing) {
      var dest: String = result.getSystemId
      if (dest == null) {
        dest =
          if (result.isInstanceOf[StreamResult]) "anonymous output stream"
          else if (result.isInstanceOf[SAXResult]) "SAX2 ContentHandler"
          else if (result.isInstanceOf[DOMResult]) "DOM tree"
          else result.getClass.getName
      }
      config.getStandardErrorOutput.println("Writing to " + dest)
    }
  }

  //  def checkAcceptableUri(context: XPathContext, uri: String): Unit = {
  //    val controller: XsltController =
  //      context.getController.asInstanceOf[XsltController]
  //    assert(controller != null)
  //    if (uri != null) {
  //      if (controller.getDocumentPool.find(uri) != null) {
  //        val err: XPathException = new XPathException(
  //          "Cannot write to a URI that has already been read: " +
  //            (if (uri == Controller.ANONYMOUS_PRINCIPAL_OUTPUT_URI)
  //              "(implicit output URI)"
  //            else uri))
  //        err.setXPathContext(context)
  //        err.setErrorCode("XTDE1500")
  //        throw err
  //      }
  //      val documentKey: DocumentURI = new DocumentURI(uri)
  //      controller.synchronized {
  //        if (!controller.checkUniqueOutputDestination(documentKey)) {
  //          val err: XPathException = new XPathException(
  //            "Cannot write more than one result document to the same URI: " +
  //              (if (uri == Controller.ANONYMOUS_PRINCIPAL_OUTPUT_URI)
  //                "(implicit output URI)"
  //              else uri))
  //          err.setXPathContext(context)
  //          err.setErrorCode("XTDE1490")
  //          throw err
  //        } else {
  //          controller.addUnavailableOutputDestination(documentKey)
  //        }
  //      }
  //    }
  //  }

  def setSerializationProperty(details: Properties,
                               uri: String,
                               lname: String,
                               value: String,
                               nsResolver: NamespaceResolver,
                               prevalidated: Boolean,
                               config: Configuration): Unit = {
    var valStr = value
    val sf: SerializerFactory = config.getSerializerFactory
    var clarkName: String = lname
    if (!uri.isEmpty) {
      clarkName = "{" + uri + "}" + lname
    }
    if (uri.isEmpty || NamespaceConstant.SAXON == uri) {
      clarkName match {
        case "method" =>
          valStr = Whitespace.trim(valStr)
          if (valStr.startsWith("Q{}") && valStr.length > 3) {
            valStr = valStr.substring(3)
          }
          if (valStr.==("xml") || valStr.==("html") || valStr.==("text") ||
            valStr.==("xhtml") ||
            valStr.==("json") ||
            valStr.==("adaptive") ||
            prevalidated ||
            valStr.startsWith("{")) {
            details.setProperty(OutputKeys.METHOD, valStr)
          } else if (valStr.startsWith("Q{")) {
            details.setProperty(OutputKeys.METHOD, valStr.substring(1))
          } else {
            var parts: Array[String] = null
            try {
              parts = NameChecker.getQNameParts(valStr)
              val prefix: String = parts(0)
              if (prefix.isEmpty) {
                val err: XPathException = new XPathException(
                  "method must be xml, html, xhtml, text, json, adaptive, or a prefixed name")
                err.setErrorCode("SEPM0016")
                err.setIsStaticError(true)
                throw err
              } else if (nsResolver != null) {
                val muri: String = nsResolver.getURIForPrefix(prefix, false)
                if (muri == null) {
                  val err: XPathException = new XPathException(
                    "Namespace prefix '" + prefix + "' has not been declared")
                  err.setErrorCode("SEPM0016")
                  err.setIsStaticError(true)
                  throw err
                }
                details.setProperty(OutputKeys.METHOD,
                  "{" + muri + "}" + parts(1))
              } else {
                details.setProperty(OutputKeys.METHOD, valStr)
              }
            } catch {
              case e: QNameException => {
                val err: XPathException = new XPathException(
                  "Invalid method name. " + e.getMessage)
                err.setErrorCode("SEPM0016")
                err.setIsStaticError(true)
                throw err
              }

            }
          }
        case "use-character-maps" =>
          var existing: String =
            details.getProperty(SaxonOutputKeys.USE_CHARACTER_MAPS)
          if (existing == null) {
            existing = ""
          }
          details.setProperty(SaxonOutputKeys.USE_CHARACTER_MAPS,
            existing + valStr)
        case "cdata-section-elements" =>
          processListOfNodeNames(details,
            clarkName,
            valStr,
            nsResolver,
            true,
            prevalidated,
            false)
        case "suppress-indentation" =>
          processListOfNodeNames(details,
            clarkName,
            valStr,
            nsResolver,
            true,
            prevalidated,
            false)
        case SaxonOutputKeys.DOUBLE_SPACE =>
          processListOfNodeNames(details,
            clarkName,
            valStr,
            nsResolver,
            true,
            prevalidated,
            false)
        case SaxonOutputKeys.ATTRIBUTE_ORDER =>
          processListOfNodeNames(details,
            clarkName,
            valStr,
            nsResolver,
            false,
            prevalidated,
            true)
        case SaxonOutputKeys.NEXT_IN_CHAIN =>
        case _ =>
          if (clarkName.==("output-version")) {
            clarkName = "version"
          }
          if (!prevalidated) {
            try {
              if (!SaxonOutputKeys.isUnstrippedProperty(clarkName)) {
                valStr = Whitespace.trim(valStr)
              }
              valStr = sf.checkOutputProperty(clarkName, valStr)
            } catch {
              case err: XPathException => {
                err.maybeSetErrorCode("SEPM0016")
                throw err
              }

            }
          }
          details.setProperty(clarkName, valStr)

      }
    } else {
      details.setProperty("{" + uri + "}" + lname, valStr)
    }
  }

  private def processListOfNodeNames(details: Properties,
                                     key: String,
                                     value: String,
                                     nsResolver: NamespaceResolver,
                                     useDefaultNS: Boolean,
                                     prevalidated: Boolean,
                                     allowStar: Boolean): Unit = {
    var existing: String = details.getProperty(key)
    if (existing == null) {
      existing = ""
    }
    val s: String = SaxonOutputKeys.parseListOfNodeNames(value,
      nsResolver,
      useDefaultNS,
      prevalidated,
      allowStar,
      "SEPM0016")
    details.setProperty(key, existing + s)
  }

  def processXslOutputElement(element: NodeInfo,
                              props: Properties,
                              c: XPathContext): Unit = {
    val resolver: NamespaceResolver = element.getAllNamespaces
    for (att <- element.attributes()) {
      val uri: String = att.getNodeName.getURI
      val local: String = att.getNodeName.getLocalPart
      val `val`: String = Whitespace.trim(att.getValue)
      setSerializationProperty(props,
        uri,
        local,
        `val`,
        resolver,
        false,
        c.getConfiguration)
    }
  }

}

class ResultDocument(private val globalProperties: Properties,
                     private val localProperties: Properties,
                     href: Expression,
                     formatExpression: Expression,
                     validationAction: Int,
                     schemaType: SchemaType,
                     serializationAttris: Map[StructuredQName, Expression],
                     @BeanProperty val characterMapIndex: CharacterMapIndex)
  extends Instruction
    with ValidatingInstruction
    with InstructionWithComplexContent
    with ContextOriginator {

  private var hrefOp: Operand = _

  private var formatOp: Operand = _

  private var contentOp: Operand = _

  private var async: Boolean = false

  @BeanProperty
  var validationOptions: ParseOptions = _

  private val serializationAttributes: Map[StructuredQName, Operand] = new HashMap(serializationAttris.size)

  @BooleanBeanProperty
  var resolveAgainstStaticBase: Boolean = false

  if (href != null) {
    hrefOp = new Operand(this, href, OperandRole.SINGLE_ATOMIC)
  }

  if (formatExpression != null) {
    formatOp = new Operand(this, formatExpression, OperandRole.SINGLE_ATOMIC)
  }

  setValidationAction(validationAction, schemaType)

  for ((key, value) <- serializationAttris.asScala) {
    this.serializationAttributes.put(key, new Operand(this, value, OperandRole.SINGLE_ATOMIC))
  }

  for (e <- serializationAttris.values.asScala) {
    adoptChildExpression(e)
  }

  def setContentExpression(content: Expression): Unit = {
    contentOp = new Operand(this, content, OperandRole.SINGLE_ATOMIC)
  }

  def setSchemaType(`type`: SchemaType): Unit = {
    if (validationOptions == null) {
      validationOptions = new ParseOptions()
    }
    validationOptions.setSchemaValidationMode(Validation.BY_TYPE)
    validationOptions.setTopLevelType(`type`)
  }

  def getSchemaType(): SchemaType =
    if (validationOptions == null) null else validationOptions.getTopLevelType

  def setValidationAction(mode: Int, schemaType: SchemaType): Unit = {
    val preservingTypes: Boolean = mode == Validation.PRESERVE && schemaType == null
    if (!preservingTypes) {
      if (validationOptions == null) {
        validationOptions = new ParseOptions()
        validationOptions.setSchemaValidationMode(mode)
        validationOptions.setTopLevelType(schemaType)
      }
    }
  }

  def getValidationAction(): Int =
    if (validationOptions == null) Validation.PRESERVE
    else validationOptions.getSchemaValidationMode

  def getFormatExpression(): Expression =
    if (formatOp == null) null else formatOp.getChildExpression

  def setUseStaticBaseUri(staticBase: Boolean): Unit = {
    resolveAgainstStaticBase = staticBase
  }

  def setAsynchronous(async: Boolean): Unit = {
    this.async = async
  }

  def isAsynchronous(): Boolean = async

  override def isMultiThreaded(config: Configuration): Boolean =
    isAsynchronous &&
      config.isLicensedFeature(Configuration.LicenseFeature.SCHEMA_VALIDATION) &&
      config.getBooleanProperty(Feature.ALLOW_MULTITHREADING)

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    val METHOD = new StructuredQName("", "", "method") // added this from XSLResultDocument class
    val BUILD_TREE = new StructuredQName("", "", "build-tree") // added this from XSLResultDocument class

    val method: String = getStaticSerializationProperty(METHOD) // XSLResultDocument not found
    val contentDependentMethod: Boolean = method == null && formatOp == null && !serializationAttributes.containsKey(METHOD) // XSLResultDocument not found
    val buildTree: Boolean = "yes" == getStaticSerializationProperty(BUILD_TREE) // XSLResultDocument not found
    if (buildTree || contentDependentMethod || "xml" == method ||
      "html" == method ||
      "xhtml" == method ||
      "text" == method) {
      try DocumentInstr.checkContentSequence(visitor.getStaticContext,
        contentOp,
        validationOptions)
      catch {
        case err: XPathException => {
          err.maybeSetLocation(getLocation)
          throw err
        }

      }
    }
    this
  }

  override def getIntrinsicDependencies(): Int = StaticProperty.HAS_SIDE_EFFECTS

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextInfo)
    if (isAsynchronous) {
      var e: Expression = getParentExpression
      while (e != null) {
        if (e.isInstanceOf[LetExpression] &&
          ExpressionTool.dependsOnVariable(
            getContentExpression,
            Array(e.asInstanceOf[LetExpression]))) {
          e.asInstanceOf[LetExpression].setNeedsEagerEvaluation(true)
        }
        e = e.getParentExpression
      }
    }
    this
  }

  def copy(rebindings: RebindingMap): Expression = {
    val map: Map[StructuredQName, Expression] =
      new HashMap[StructuredQName, Expression]()
    for ((key, value) <- serializationAttributes.asScala) {
      map.put(key, value.getChildExpression.copy(rebindings))
    }
    val r: ResultDocument = new ResultDocument(
      globalProperties,
      localProperties,
      if (getHref == null) null else getHref.copy(rebindings),
      if (getFormatExpression == null) null
      else getFormatExpression.copy(rebindings),
      getValidationAction,
      getSchemaType,
      map,
      characterMapIndex
    )
    ExpressionTool.copyLocationInfo(this, r)
    r.setContentExpression(getContentExpression.copy(rebindings))
    r.resolveAgainstStaticBase = resolveAgainstStaticBase
    r.async = async
    r
  }

  override def getInstructionNameCode(): Int = StandardNames.XSL_RESULT_DOCUMENT

  override def getItemType(): ItemType = ErrorType.getInstance

  override def operands(): java.lang.Iterable[Operand] = {
    val list: ArrayList[Operand] = new ArrayList[Operand](6)
    list.add(contentOp)
    if (hrefOp != null) {
      list.add(hrefOp)
    }
    if (formatOp != null) {
      list.add(formatOp)
    }
    list.addAll(serializationAttributes.values)
    list
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val result: PathMap.PathMapNodeSet =
      super.addToPathMap(pathMap, pathMapNodeSet)
    result.setReturnable(false)
    new PathMap.PathMapNodeSet(pathMap.makeNewRoot(this))
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    process(getContentExpression, context)
    null
  }

  def process(content: Expression, context: XPathContext): Unit = {
    checkNotTemporaryOutputState(context)
    //context.getConfiguration.processResultDocument(this, content, context) // processResultDocument method commented
  }

  //  def processInstruction(content: Expression, context: XPathContext): Unit = {
  //    val controller: XsltController =
  //      context.getController.asInstanceOf[XsltController]
  //    assert(controller != null)
  //    val savedOutputUri: String = context.getCurrentOutputUri
  //    val out: ComplexContentOutputter = processLeft(context)
  //    var failed: Boolean = false
  //    try content.process(out, context)
  //    catch {
  //      case err: XPathException => {
  //        failed = true
  //        err.maybeSetContext(context)
  //        err.maybeSetLocation(getLocation)
  //        throw err
  //      }
  //
  //    } finally try out.close()
  //    catch {
  //      case e: XPathException =>
  //        if (!failed) {
  //          throw e
  //        }
  //
  //    }
  //    context.setCurrentOutputUri(savedOutputUri)
  //  }

  //  def processLeft(context: XPathContext): ComplexContentOutputter = {
  //    val controller: XsltController =
  //      context.getController.asInstanceOf[XsltController]
  //    val config: Configuration = controller.getConfiguration
  //    checkNotTemporaryOutputState(context)
  //    val computedLocalProps: Properties = gatherOutputProperties(context)
  //    if (getStaticBaseURIString != null) {
  //      computedLocalProps.setProperty(
  //        SaxonOutputKeys.PARAMETER_DOCUMENT_BASE_URI,
  //        getStaticBaseURIString)
  //    }
  //    val serParams: SerializationProperties =
  //      new SerializationProperties(computedLocalProps, characterMapIndex)
  //    if (validationOptions != null &&
  //      validationOptions.getSchemaValidationMode != Validation.PRESERVE) {
  //      serParams.setValidationFactory((output) => {
  //        val nr: NamespaceReducer = new NamespaceReducer(output)
  //        config.getDocumentValidator(nr,
  //          output.getSystemId,
  //          validationOptions,
  //          getLocation)
  //      })
  //    }
  //    var out: Receiver = null
  //    var resolver: ResultDocumentResolver = null
  //    var hrefValue: String = ""
  //    if (getHref != null) {
  //      hrefValue = IriToUri.iriToUri(getHref.evaluateAsString(context)).toString
  //    }
  //    if (hrefValue.isEmpty || hrefValue == controller.getBaseOutputURI) {
  //      val gateKeeper: PrincipalOutputGatekeeper = controller.getGatekeeper
  //      if (gateKeeper != null) {
  //        gateKeeper.useAsSecondary()
  //        out = gateKeeper.makeReceiver(serParams)
  //      }
  //    }
  //    if (out == null) {
  //      try {
  //        resolver = controller.getResultDocumentResolver
  //        out = makeReceiver(hrefValue,
  //          getStaticBaseURIString,
  //          context,
  //          resolver,
  //          serParams,
  //          resolveAgainstStaticBase)
  //        traceDestination(context, out)
  //      } catch {
  //        case e: XPathException => {
  //          e.maybeSetLocation(getLocation)
  //          e.maybeSetContext(context)
  //          throw e
  //        }
  //
  //      }
  //    }
  //    out.getPipelineConfiguration.setController(controller)
  //    val systemId: String = out.getSystemId
  //    val nr: NamespaceReducer = new NamespaceReducer(out)
  //    val cco: ComplexContentOutputter = new ComplexContentOutputter(nr)
  //    cco.setSystemId(systemId)
  //    context.setCurrentOutputUri(systemId)
  //    cco.open()
  //    cco
  //  }

  private def checkNotTemporaryOutputState(context: XPathContext): Unit = {
    if (context.getTemporaryOutputState != 0) {
      val err: XPathException = new XPathException(
        "Cannot execute xsl:result-document while evaluating xsl:" +
          context.getNamePool.getLocalName(context.getTemporaryOutputState))
      err.setErrorCode("XTDE1480")
      err.setLocation(getLocation)
      throw err
    }
  }

  def gatherOutputProperties(context: XPathContext): Properties = {
    val controller: Controller = context.getController
    assert(controller != null)
    val config: Configuration = context.getConfiguration
    var computedGlobalProps: Properties = globalProperties
    val nsResolver: NamespaceResolver = getRetainedStaticContext
    assert(nsResolver != null)
    if (getFormatExpression != null) {
      var qName: StructuredQName = null
      val format: String =
        getFormatExpression.evaluateAsString(context).toString
      if (format.startsWith("Q{")) {
        qName = StructuredQName.fromEQName(format)
      } else {
        var parts: Array[String] = null
        try parts = NameChecker.getQNameParts(format)
        catch {
          case e: QNameException => {
            val err: XPathException = new XPathException(
              "The requested output format " + Err
                .wrap(format) + " is not a valid QName")
            err.maybeSetLocation(getFormatExpression.getLocation)
            err.setErrorCode("XTDE1460")
            err.setXPathContext(context)
            throw err
          }

        }
        val uri: String = nsResolver.getURIForPrefix(parts(0), false)
        if (uri == null) {
          val err: XPathException = new XPathException(
            "The namespace prefix in the format name " + format +
              " is undeclared")
          err.maybeSetLocation(getFormatExpression.getLocation)
          err.setErrorCode("XTDE1460")
          err.setXPathContext(context)
          throw err
        }
        qName = new StructuredQName(parts(0), uri, parts(1))
      }
      computedGlobalProps = getRetainedStaticContext.getPackageData
        .asInstanceOf[StylesheetPackage]
        .getNamedOutputProperties(qName)
      if (computedGlobalProps == null) {
        val err: XPathException = new XPathException(
          "There is no xsl:output format named " + format)
        err.setErrorCode("XTDE1460")
        err.setXPathContext(context)
        throw err
      }
    }
    val computedLocalProps: Properties = new Properties(computedGlobalProps)
    for (keyo <- localProperties.keySet.asScala) {
      val key: String = keyo.asInstanceOf[String]
      val qName: StructuredQName = StructuredQName.fromClarkName(key)
      try setSerializationProperty(computedLocalProps,
        qName.getURI,
        qName.getLocalPart,
        localProperties.getProperty(key),
        nsResolver,
        true,
        config)
      catch {
        case e: XPathException => {
          e.setErrorCode("XTDE0030")
          e.maybeSetLocation(getLocation)
          throw e
        }

      }
    }
    if (!serializationAttributes.isEmpty) {
      for ((key, value) <- serializationAttributes.asScala) {
        val valStr: String =
          value.getChildExpression.evaluateAsString(context).toString
        val lname: String = key.getLocalPart
        val uri: String = key.getURI
        try setSerializationProperty(computedLocalProps,
          uri,
          lname,
          valStr,
          nsResolver,
          false,
          config)
        catch {
          case e: XPathException => {
            e.setErrorCode("XTDE0030")
            e.maybeSetLocation(getLocation)
            e.maybeSetContext(context)
            if (NamespaceConstant.SAXON == e.getErrorCodeNamespace && "SXWN" == e.getErrorCodeLocalPart
              .substring(0, 4)) {
              val ee: XmlProcessingException = new XmlProcessingException(e)
              ee.setWarning(true)
              controller.getErrorReporter.report(ee)
            } else {
              throw e
            }
          }

        }
      }
    }
    computedLocalProps
  }

  def getStaticSerializationProperty(name: StructuredQName): String = {
    val clarkName: String = name.getClarkName
    val local: String = localProperties.getProperty(clarkName)
    if (local != null) {
      return local
    }
    if (serializationAttributes.containsKey(name)) {
      return null
    }
    globalProperties.getProperty(clarkName)
  }

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("resultDoc", this)
    out.emitAttribute("global", exportProperties(globalProperties))
    out.emitAttribute("local", exportProperties(localProperties))
    if (getValidationAction != Validation.SKIP && getValidationAction != Validation.BY_TYPE) {
      out.emitAttribute("validation", Validation.toString(getValidationAction))
    }
    val schemaType: SchemaType = getSchemaType
    if (schemaType != null) {
      out.emitAttribute("type", schemaType.getStructuredQName)
    }
    if (getHref != null) {
      out.setChildRole("href")
      getHref.export(out)
    }
    if (getFormatExpression != null) {
      out.setChildRole("format")
      getFormatExpression.export(out)
    }
    for ((key, value) <- serializationAttributes.asScala) {
      val name: StructuredQName = key
      val valStr: Expression = value.getChildExpression
      out.setChildRole(name.getEQName)
      valStr.export(out)
    }
    out.setChildRole("content")
    getContentExpression.export(out)
    out.endElement()
  }

  private def exportProperties(props: Properties): String = {
    val writer: StringBuilder = new StringBuilder()
    for (key <- props.stringPropertyNames().asScala) {
      var mapKey = key
      var `val`: String = props.getProperty(mapKey)
      if (mapKey == SaxonOutputKeys.ITEM_SEPARATOR || mapKey == SaxonOutputKeys.NEWLINE) {
        `val` = ExpressionPresenter.jsEscape(`val`)
      }
      if (mapKey == SaxonOutputKeys.USE_CHARACTER_MAPS || mapKey == OutputKeys.METHOD) {
        `val` = `val`.replace("{", "Q{")
      }
      if (mapKey.startsWith("{")) {
        mapKey = "Q" + mapKey
      }
      writer.append(mapKey).append("=").append(`val`).append("\n")
    }
    writer.toString
  }

  override def getStreamerName(): String = "ResultDocument"

  def getHref(): Expression =
    if (hrefOp == null) null else hrefOp.getChildExpression

  def setHref(href: Expression): Unit = {
    hrefOp.setChildExpression(href)
  }

  def setFormatExpression(formatExpression: Expression): Unit = {
    formatOp.setChildExpression(formatExpression)
  }

  def getContentExpression(): Expression = contentOp.getChildExpression

}
