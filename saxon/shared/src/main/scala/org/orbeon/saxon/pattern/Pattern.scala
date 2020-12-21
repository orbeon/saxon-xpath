package org.orbeon.saxon.pattern

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.SlotManager
import org.orbeon.saxon.expr.parser.ContextItemStaticInfo
import org.orbeon.saxon.expr.parser.ExpressionVisitor
import org.orbeon.saxon.expr.parser.RebindingMap
import org.orbeon.saxon.expr.parser.XPathParser
import org.orbeon.saxon.functions.Current
import org.orbeon.saxon.lib.Feature
import org.orbeon.saxon.model.ItemType
import org.orbeon.saxon.model.UType
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.HostLanguage._
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter._
import org.orbeon.saxon.utils.Configuration

//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.beans.{BeanProperty, BooleanBeanProperty}

object Pattern {

  def make(pattern: String,
           env: StaticContext,
           packageData: PackageData): Pattern = {
    var languageLevel: Int = env.getConfiguration.getConfigurationProperty(
      Feature.XPATH_VERSION_FOR_XSLT)
    if (languageLevel == 30) {
      languageLevel = 305
    }
   /* val lineNumber: Int =
      if (env.isInstanceOf[ExpressionContext]) // class not exist
        env.asInstanceOf[ExpressionContext].getStyleElement.getLineNumber // class not exist
      else -1*/
    val parser: PatternParser = env.getConfiguration
      .newExpressionParser("PATTERN", updating = false, languageLevel)
      .asInstanceOf[PatternParser]
    parser
      .asInstanceOf[XPathParser]
      .setLanguage(XPathParser.ParsedLanguage.XSLT_PATTERN, 30)
    var pat: Pattern = parser.parsePattern(pattern, env)
    pat.setRetainedStaticContext(env.makeRetainedStaticContext())
    pat = pat.simplify()
    pat
  }

   def replaceCurrent(exp: Expression, binding: LocalBinding): Unit = {
    for (o <- exp.operands.asScala) {
      val child = o.getChildExpression
      if (child.isCallOn(classOf[Current])) {
        val ref: LocalVariableReference = new LocalVariableReference(binding)
        o.setChildExpression(ref)
      } else {
        replaceCurrent(child, binding)
      }
    }
  }

  def patternContainsVariable(pattern: Pattern): Boolean =
    pattern != null &&
      (pattern.getDependencies & StaticProperty.DEPENDS_ON_LOCAL_VARIABLES) !=
        0

}

abstract class Pattern extends PseudoExpression {

  var priority: Double = 0.5

  @BooleanBeanProperty
  var recoverable: Boolean = true

  override def isLiftable(forStreaming: Boolean): Boolean = false

  def bindCurrent(binding: LocalBinding): Unit = ()

  def matchesCurrentGroup(): Boolean = false

  def setOriginalText(text: String): Unit = ()

   def handleDynamicError(ex: XPathException,
                                   context: XPathContext): Unit = {
    if ("XTDE0640" == ex.getErrorCodeLocalPart) {
      throw ex
    }
    if (!isRecoverable) {
      throw ex
    }
    context.getController.warning(
      "An error occurred matching pattern {" + this + "}: ",
      ex.getErrorCodeQName.getEQName,
      getLocation)
  }

  override def simplify(): Pattern = this

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Pattern = {
    typeCheckChildren(visitor, contextInfo)
    this
  }

  override def getDependencies(): Int = 0

  def allocateSlots(slotManager: SlotManager, nextFree: Int): Int = nextFree

  def isMotionless: Boolean = true

  override def effectiveBooleanValue(context: XPathContext): Boolean =
    matches(context.getContextItem, context)

  def matches(item: Item, context: XPathContext): Boolean

  def matchesBeneathAnchor(node: NodeInfo,
                           anchor: NodeInfo,
                           context: XPathContext): Boolean =
    matches(node, context)

  def selectNodes(document: TreeInfo,
                  context: XPathContext): SequenceIterator = {
    val doc: NodeInfo = document.getRootNode
    val uType: UType = getUType
    if (UType.DOCUMENT.subsumes(uType)) {
      if (matches(doc, context)) {
        SingletonIterator.makeIterator(doc)
      } else {
        EmptyIterator.ofNodes
      }
    } else if (UType.ATTRIBUTE.subsumes(uType)) {
      val allElements: AxisIterator =
        doc.iterateAxis(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
      val atts: MappingFunction = (item) =>
        item.asInstanceOf[NodeInfo].iterateAxis(AxisInfo.ATTRIBUTE)
      val allAttributes: SequenceIterator =
        new MappingIterator(allElements, atts)
      val selection: ItemMappingFunction = (item) =>
        if (matches(item, context)) item.asInstanceOf[NodeInfo] else null
      new ItemMappingIterator(allAttributes, selection)
    } else if (UType.NAMESPACE.subsumes(uType)) {
      val allElements: AxisIterator =
        doc.iterateAxis(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
      val atts: MappingFunction = (item) =>
        item.asInstanceOf[NodeInfo].iterateAxis(AxisInfo.NAMESPACE)
      val allNamespaces: SequenceIterator =
        new MappingIterator(allElements, atts)
      val selection: ItemMappingFunction = (item) =>
        if (matches(item, context)) item.asInstanceOf[NodeInfo] else null
      new ItemMappingIterator(allNamespaces, selection)
    } else if (UType.CHILD_NODE_KINDS.subsumes(uType)) {
      var nodeTest: NodeTest = null
      nodeTest =
        if (uType == UType.ELEMENT) NodeKindTest.ELEMENT
        else new MultipleNodeKindTest(uType)
      val allChildren: AxisIterator =
        doc.iterateAxis(AxisInfo.DESCENDANT, nodeTest)
      val selection: ItemMappingFunction = (item) =>
        if (matches(item, context)) item.asInstanceOf[NodeInfo] else null
      new ItemMappingIterator(allChildren, selection)
    } else {
      val axis: Int =
        if (uType.subsumes(UType.DOCUMENT)) AxisInfo.DESCENDANT_OR_SELF
        else AxisInfo.DESCENDANT
      val allChildren: AxisIterator = doc.iterateAxis(axis)
      val processElement: MappingFunction = (item) => {
        var mapper: AxisIterator =
          SingleNodeIterator.makeIterator(item.asInstanceOf[NodeInfo])
        if (uType.subsumes(UType.NAMESPACE)) {
          mapper = new ConcatenatingAxisIterator(
            mapper,
            item.asInstanceOf[NodeInfo].iterateAxis(AxisInfo.NAMESPACE))
        }
        if (uType.subsumes(UType.ATTRIBUTE)) {
          mapper = new ConcatenatingAxisIterator(
            mapper,
            item.asInstanceOf[NodeInfo].iterateAxis(AxisInfo.ATTRIBUTE))
        }
        mapper
      }
      val attributesOrSelf: SequenceIterator =
        new MappingIterator(allChildren, processElement)
      val test: ItemMappingFunction = (item) =>
        if (matches(item, context)) {
          item.asInstanceOf[NodeInfo]
        } else {
          null
        }
      new ItemMappingIterator(attributesOrSelf, test)
    }
  }

  def getUType: UType

  def getFingerprint: Int = -1

  def getItemType: ItemType

  def setPriority(priority: Double): Unit = {
    this.priority = priority
  }

  def getDefaultPriority: Double = priority

  override def toString: String = "pattern matching " + getItemType

  def getHostLanguage: HostLanguage = XSLT

  def convertToTypedPattern(`val`: String): Pattern = null

  override def toPattern(config: Configuration): Pattern = this

  def export(presenter: ExpressionPresenter): Unit

  def copy(rebindings: RebindingMap): Pattern

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Pattern = this

  override def toShortString: String = toString

}
