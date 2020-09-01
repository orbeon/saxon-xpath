package net.sf.saxon.functions

import java.net.URI
import java.util.{HashSet, Map, Set}

import javax.xml.transform.Source
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamSource
import net.sf.saxon.event.{Builder, Sender}
import net.sf.saxon.expr._
import net.sf.saxon.expr.accum.{Accumulator, AccumulatorRegistry}
import net.sf.saxon.lib.{ParseOptions, Validation}
import net.sf.saxon.ma.map.MapItem
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.om._
import net.sf.saxon.style.StylesheetPackage
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.tiny.TinyBuilder
import net.sf.saxon.utils.{Configuration, Controller}
import net.sf.saxon.value.{AtomicValue, BooleanValue, QNameValue, SequenceType}

object Doc_2 {

  def makeOptionsParameter(): OptionsParameter = {
    val listOfQNames: SequenceType = SequenceType.makeSequenceType(
      BuiltInAtomicType.QNAME,
      StaticProperty.ALLOWS_ZERO_OR_MORE)
    val op: OptionsParameter = new OptionsParameter()
    op.addAllowedOption("validation", SequenceType.SINGLE_STRING)
    op.setAllowedValues("validation",
      "SXZZ0001",
      "strict",
      "lax",
      "preserve",
      "skip")
    op.addAllowedOption("type", SequenceType.SINGLE_QNAME)
    op.addAllowedOption("strip-space", SequenceType.SINGLE_STRING)
    op.setAllowedValues("strip-space",
      "SXZZ0001",
      "none",
      "all",
      "package-defined")
    op.addAllowedOption("stable", SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption("dtd-validation", SequenceType.SINGLE_BOOLEAN)
    op.addAllowedOption("accumulators", listOfQNames)
    op.addAllowedOption("use-xsi-schema-location", SequenceType.SINGLE_BOOLEAN)
    op
  }

}

class Doc_2 extends SystemFunction with Callable {

  private def setParseOptions(checkedOptions: Map[String, Sequence],
                              context: XPathContext): ParseOptions = {
    val result: ParseOptions = new ParseOptions(
      context.getConfiguration.getParseOptions)
    var value: Sequence = checkedOptions.get("validation")
    if (value != null) {
      var valStr: String = value.head.getStringValue
      if ("skip" == valStr) {
        valStr = "strip"
      }
      val v: Int = Validation.getCode(valStr)
      if (v == Validation.INVALID) {
        throw new XPathException("Invalid validation value " + valStr,
          "SXZZ0002")
      }
      result.setSchemaValidationMode(v)
    }
    value = checkedOptions.get("type")
    if (value != null) {
      val qval: QNameValue = value.head.asInstanceOf[QNameValue]
      result.setTopLevelType(
        context.getConfiguration.getSchemaType(qval.getStructuredQName))
      result.setSchemaValidationMode(Validation.BY_TYPE)
    }
    value = checkedOptions.get("strip-space")
    if (value != null) {
      val s: String = value.head.getStringValue
      s match {
        case "all" =>
          result.setSpaceStrippingRule(
            AllElementsSpaceStrippingRule.getInstance)
        case "none" =>
          result.setSpaceStrippingRule(
            NoElementsSpaceStrippingRule.getInstance)
        case "package-defined" =>
          var data: PackageData = getRetainedStaticContext.getPackageData
          if (data.isInstanceOf[StylesheetPackage]) {
            result.setSpaceStrippingRule(
              data.asInstanceOf[StylesheetPackage].getSpaceStrippingRule)
          }

      }
    }
    value = checkedOptions.get("dtd-validation")
    if (value != null) {
      result.setDTDValidationMode(
        if (value.head.asInstanceOf[BooleanValue].getBooleanValue)
          Validation.STRICT
        else Validation.SKIP)
    }
    value = checkedOptions.get("accumulators")
    if (value != null) {
      val reg: AccumulatorRegistry =
        getRetainedStaticContext.getPackageData.getAccumulatorRegistry
      val accumulators: Set[Accumulator] = new HashSet[Accumulator]()
      val iter: SequenceIterator = value.iterate()
      var it: Item = null
      while (({
        it = iter.next()
        it
      }) != null) {
        val name: QNameValue = it.asInstanceOf[QNameValue]
        val acc: Accumulator = reg.getAccumulator(name.getStructuredQName)
        accumulators.add(acc)
      }
      result.setApplicableAccumulators(accumulators)
    }
    value = checkedOptions.get("use-xsi-schema-location")
    if (value != null) {
      result.setUseXsiSchemaLocation(
        value.head.asInstanceOf[BooleanValue].getBooleanValue)
    }
    result
  }

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[NodeInfo] = {
    val hrefVal: AtomicValue = arguments(0).head.asInstanceOf[AtomicValue]
    if (hrefVal == null) {
      ZeroOrOne.empty()
    }
    val href: String = hrefVal.getStringValue
    val param: Item = arguments(1).head
    val checkedOptions: Map[String, Sequence] = getDetails.optionDetails
      .processSuppliedOptions(param.asInstanceOf[MapItem], context)
    val parseOptions: ParseOptions = setParseOptions(checkedOptions, context)
    val item: NodeInfo = fetch(href, parseOptions, context).getRootNode
    if (item == null) {
      throw new XPathException("Failed to load document " + href,
        "FODC0002",
        context)
    }
    val controller: Controller = context.getController
//    if (parseOptions != null && controller.isInstanceOf[XsltController]) {
//      controller
//        .asInstanceOf[XsltController]
//        .getAccumulatorManager
//        .setApplicableAccumulators(item.getTreeInfo,
//          parseOptions.getApplicableAccumulators)
//    }
    new ZeroOrOne(item)
  }

  private def fetch(href: String,
                    options: ParseOptions,
                    context: XPathContext): TreeInfo = {
    val config: Configuration = context.getConfiguration
    val controller: Controller = context.getController
    var abs: URI = null
    abs = ResolveURI.makeAbsolute(href, getStaticBaseUriString)
    val source: Source = config.getSourceResolver
      .resolveSource(new StreamSource(abs.toASCIIString()), config)
    var newdoc: TreeInfo = null
    if (source.isInstanceOf[NodeInfo] || source.isInstanceOf[DOMSource]) {
      val startNode: NodeInfo = controller.prepareInputTree(source)
      newdoc = startNode.getTreeInfo
    } else {
      val b: Builder = controller.makeBuilder
      if (b.isInstanceOf[TinyBuilder]) {
        b.asInstanceOf[TinyBuilder]
          .setStatistics(config.getTreeStatistics.SOURCE_DOCUMENT_STATISTICS)
      }
      b.setPipelineConfiguration(b.getPipelineConfiguration)
      try {
        Sender.send(source, b, options)
        newdoc = b.getCurrentRoot.getTreeInfo
        b.reset()
      } finally if (options.isPleaseCloseAfterUse) {
        ParseOptions.close(source)
      }
    }
    newdoc
  }

 override def getSpecialProperties(arguments: Array[Expression]): Int =
    StaticProperty.ORDERED_NODESET | StaticProperty.PEER_NODESET |
      StaticProperty.NO_NODES_NEWLY_CREATED |
      StaticProperty.SINGLE_DOCUMENT_NODESET

}
