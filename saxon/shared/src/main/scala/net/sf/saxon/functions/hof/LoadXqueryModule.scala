////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.hof

import net.sf.saxon.utils.Configuration
import net.sf.saxon.utils.Controller
import net.sf.saxon.expr._
import net.sf.saxon.expr.instruct.GlobalContextRequirement
import net.sf.saxon.expr.instruct.GlobalVariable
import net.sf.saxon.expr.instruct.UserFunction
import net.sf.saxon.functions.OptionsParameter
import net.sf.saxon.functions.SystemFunction
import net.sf.saxon.lib.ModuleURIResolver
import net.sf.saxon.lib.StandardModuleURIResolver
import net.sf.saxon.ma.map._
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.ItemType
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.Item
import net.sf.saxon.om.Sequence
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.trans.SaxonErrorCode
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AtomicIterator
import net.sf.saxon.value._
import javax.xml.transform.stream.StreamSource
import java.math.BigDecimal
import java.util.ArrayList
import java.util.Iterator
import java.util.List
import java.util.Map

import scala.jdk.CollectionConverters._
import net.sf.saxon.query.{DynamicQueryContext, QueryLibrary, QueryModule, QueryReader, StaticQueryContext, XQueryExpression, XQueryFunction, XQueryFunctionLibrary}
import net.sf.saxon.trace.ExpressionPresenter


object LoadXqueryModule {

  def makeOptionsParameter(): OptionsParameter = {
    val op: OptionsParameter = new OptionsParameter()
    op.addAllowedOption("xquery-version", SequenceType.SINGLE_DECIMAL)
    op.addAllowedOption("location-hints", SequenceType.STRING_SEQUENCE)
    op.addAllowedOption("context-item", SequenceType.OPTIONAL_ITEM)
    // standard type?
    op.addAllowedOption(
      "variables",
      SequenceType.makeSequenceType(
        new MapType(BuiltInAtomicType.QNAME, SequenceType.ANY_SEQUENCE),
        StaticProperty.EXACTLY_ONE))
    op.addAllowedOption(
      "vendor-options",
      SequenceType.makeSequenceType(
        new MapType(BuiltInAtomicType.QNAME, SequenceType.ANY_SEQUENCE),
        StaticProperty.EXACTLY_ONE))
    op
  }

}

/**
 * This class implements the function load-xquery-module(), which is a standard function in XPath 3.1.
 * It is classified as a higher-order function and therefore requires Saxon-PE or higher.
 */
class LoadXqueryModule extends SystemFunction with Callable {

  /**
   * Prepare an XPathContext object for evaluating the function
   *
   * @param callingContext the XPathContext of the function calling expression
   * @param originator
   * @return a suitable context for evaluating the function (which may or may
   *         not be the same as the caller's context)
   */
  override def makeNewContext(callingContext: XPathContext,
                              originator: ContextOriginator): XPathContext =
    callingContext

  def call(context: XPathContext, args: Array[Sequence]): MapItem = {
    var xqueryVersionOption: Sequence = null
    var locationHintsOption: Sequence = null
    var variablesOption: Sequence = null
    var contextItemOption: Sequence = null
    var vendorOptionsOption: Sequence = null
    if (args.length == 2) {
      val suppliedOptions: MapItem = args(1).head().asInstanceOf[MapItem]
      val checkedOptions: Map[String, Sequence] = getDetails.optionDetails
        .processSuppliedOptions(suppliedOptions, context)
      xqueryVersionOption = checkedOptions.get("xquery-version")
      if (xqueryVersionOption != null &&
        xqueryVersionOption
          .head()
          .asInstanceOf[DecimalValue]
          .getDoubleValue *
          10 >
          31) {
        throw new XPathException(
          "No XQuery version " + xqueryVersionOption + " processor is available",
          "FOQM0006")
      }
      locationHintsOption = checkedOptions.get("location-hints")
      variablesOption = checkedOptions.get("variables")
      contextItemOption = checkedOptions.get("context-item")
      vendorOptionsOption = checkedOptions.get("vendor-options")
    }
    var qv: Int = 31
    if (xqueryVersionOption != null) {
      val decimalVn: BigDecimal =
        xqueryVersionOption.head().asInstanceOf[DecimalValue].getDecimalValue
      if (decimalVn == new BigDecimal("1.0") || decimalVn == new BigDecimal(
        "3.0") ||
        decimalVn == new BigDecimal("3.1")) {
        qv = decimalVn.multiply(BigDecimal.TEN).intValue()
      } else {
        throw new XPathException("Unsupported XQuery version " + decimalVn,
          "FOQM0006")
      }
    }
    val moduleUri: String = args(0).head().getStringValue
    if (moduleUri.isEmpty) {
      throw new XPathException(
        "First argument of fn:load-xquery-module() must not be a zero length string",
        "FOQM0001")
    }
    // location hints are currently ignored by QT3TestDriver?
    val locationHints: List[String] = new ArrayList[String]()
    if (locationHintsOption != null) {
      val iterator: SequenceIterator = locationHintsOption.iterate()
      var hint: Item = null
      while (({
        hint = iterator.next()
        hint
      }) != null) locationHints.add(
        hint.getStringValue)
    }
    val config: Configuration = context.getConfiguration
    val staticQueryContext: StaticQueryContext = config.newStaticQueryContext
    var moduleURIResolver: ModuleURIResolver = config.getModuleURIResolver
    if (moduleURIResolver == null) {
      moduleURIResolver = new StandardModuleURIResolver(config)
    }
    staticQueryContext.setModuleURIResolver(moduleURIResolver)
    val baseURI: String = getRetainedStaticContext.getStaticBaseUriString
    staticQueryContext.setBaseURI(baseURI)
    var streamSources: Array[StreamSource] = null
    try {
      val hints: Array[String] = locationHints.toArray(Array.ofDim[String](0))
      streamSources = staticQueryContext.getModuleURIResolver.resolve(
        moduleUri,
        baseURI,
        hints)
      if (streamSources == null) {
        streamSources = new StandardModuleURIResolver(config)
          .resolve(moduleUri, baseURI, hints)
      }
    } catch {
      case e: XPathException => {
        e.maybeSetErrorCode("FOQM0002")
        throw e
      }

    }
    if (streamSources.length == 0) {
      throw new XPathException(
        "No library module found with specified target namespace " +
          moduleUri,
        "FOQM0002")
    }
    // Note: Location hints other than the first are ignored
    val sourceQuery: String = QueryReader.readSourceQuery(
      streamSources(0),
      config.getValidCharacterChecker)
    staticQueryContext.compileLibrary(sourceQuery)
    val lib: QueryLibrary = staticQueryContext.getCompiledLibrary(moduleUri)
    if (lib == null) {
      throw new XPathException(
        "The library module located does not have the expected namespace " +
          moduleUri,
        "FOQM0002")
    }
    // module to be loaded is a library module not a main module
    val main: QueryModule = new QueryModule(staticQueryContext)
    // so use alternative constructor?
    main.setPackageData(lib.getPackageData)
    main.setExecutable(lib.getExecutable)
    lib.link(main)
    val xqe: XQueryExpression =
      new XQueryExpression(new ContextItemExpression(), main, false)
    val dqc: DynamicQueryContext = new DynamicQueryContext(
      context.getConfiguration)
    // Get the external variables and set parameters on DynamicQueryContext dqc
    if (variablesOption != null) {
      val extVariables: MapItem = variablesOption.head().asInstanceOf[MapItem]
      val iterator: AtomicIterator[_ <: AtomicValue] = extVariables.keys
      var key: AtomicValue = null
      while (({
        key = iterator.next()
        key
      }) != null) dqc.setParameter(
        key.asInstanceOf[QNameValue].getStructuredQName,
        extVariables.get(key).asInstanceOf[Sequence].materialize())
    }
    // Get the context item supplied, and set it on the new Controller
    if (contextItemOption != null) {
      val contextItem: Item = contextItemOption.head()
      val gcr: GlobalContextRequirement =
        main.getExecutable.getGlobalContextRequirement
      if (gcr != null) {
        val req: ItemType = gcr.getRequiredItemType
        if (req != null && !req.matches(contextItem, config.getTypeHierarchy)) {
          throw new XPathException("Required context item type is " + req,
            "FOQM0005")
        }
      }
      dqc.setContextItem(contextItemOption.head())
    }
    val newController: Controller = xqe.newController(dqc)
    val newContext: XPathContext = newController.newXPathContext
    var variablesMap: HashTrieMap = new HashTrieMap()
    for (variable <- lib.getGlobalVariables.asScala) {
      var value: GroundedValue = null
      val qNameValue: QNameValue =
        new QNameValue(variable.getVariableQName, BuiltInAtomicType.QNAME)
      if (qNameValue.getNamespaceURI == moduleUri) {
        try value = variable.evaluateVariable(newContext)
        catch {
          case e: XPathException => {
            e.setIsGlobalError(false)
            if (e.getErrorCodeLocalPart.==("XPTY0004")) {
              throw new XPathException(e.getMessage, "FOQM0005")
            } else {
              throw e
            }
          }

        }
        variablesMap = variablesMap.addEntry(qNameValue, value)
      }
    }
    // Add functions to the result.
    var functionsMap: HashTrieMap = new HashTrieMap()
    val functionLib: XQueryFunctionLibrary = lib.getGlobalFunctionLibrary
    val functionIterator: Iterator[XQueryFunction] =
      functionLib.getFunctionDefinitions
    val agent: ExportAgent = new ExportAgent {
      override def export(out: ExpressionPresenter): Unit = {
        var err: XPathException = new XPathException(
          "Cannot export a stylesheet that statically incorporates XQuery functions",
          SaxonErrorCode.SXST0069)
        err.setIsStaticError(true)
        throw err
      }
    }
    if (functionIterator.hasNext) {
      var function: XQueryFunction = null
      var newMap: MapItem = null
      var functionQName: QNameValue = null
      while (functionIterator.hasNext) {
        function = functionIterator.next()
        functionQName =
          new QNameValue(function.getFunctionName, BuiltInAtomicType.QNAME)
        if (functionQName.getNamespaceURI == moduleUri) {
          val userFunction: UserFunction = function.getUserFunction
          val buf: UserFunctionReference.BoundUserFunction =
            new UserFunctionReference.BoundUserFunction(agent,
              userFunction,
              null,
              newController)
          newMap =
            if (functionsMap.get(functionQName) != null)
              functionsMap
                .get(functionQName)
                .asInstanceOf[MapItem]
                .addEntry(new Int64Value(function.getNumberOfArguments), buf)
            else
              new SingleEntryMap(
                Int64Value.makeIntegerValue(function.getNumberOfArguments),
                buf)
          functionsMap = functionsMap.addEntry(functionQName, newMap)
        }
      }
    }
    val map: DictionaryMap = new DictionaryMap()
    map.initialPut("variables", variablesMap)
    map.initialPut("functions", functionsMap)
    map
  }

  // Set the vendor options (configuration features) -- at the moment none supported
  /*if (vendorOptionsOption != null) {
            MapItem vendorOptions = (MapItem) options.get(new StringValue("vendor-options")).head();
        }*/

  // Evaluate the global variables, and add values to the result.
  // Set the vendor options (configuration features) -- at the moment none supported
  /*if (vendorOptionsOption != null) {
            MapItem vendorOptions = (MapItem) options.get(new StringValue("vendor-options")).head();
        }*/

  // Evaluate the global variables, and add values to the result.

}

// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
