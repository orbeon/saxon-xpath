package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.expr.parser.RoleDiagnostic

import net.sf.saxon.model.AnyItemType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om.StandardNames

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.pattern.Pattern

import net.sf.saxon.s9api.Location

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trace.TraceableComponent

import net.sf.saxon.trans.UncheckedXPathException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.trans.rules.Rule

import net.sf.saxon.trans.rules.RuleTarget

import net.sf.saxon.value.SequenceType

import java.util.ArrayList

import java.util.List

import java.util.function.BiConsumer

import TemplateRule._

import scala.beans.{BeanProperty, BooleanBeanProperty}

import scala.jdk.CollectionConverters._

object TemplateRule {

  private def gatherLocalParams(exp: Expression,
                                result: List[LocalParam]): Unit = {
    if (exp.isInstanceOf[LocalParam]) {
      result.add(exp.asInstanceOf[LocalParam])
    } else {
      for (o <- exp.operands.asScala) {
        gatherLocalParams(o.getChildExpression, result)
      }
    }
  }

}

class TemplateRule
  extends RuleTarget
    with Location
    with ExpressionOwner
    with TraceableComponent {

   var body: Expression = _

   var matchPattern: Pattern = _

  var hasRequiredParams: Boolean = _

  private var bodyIsTailCallReturner: Boolean = _

  private var requiredType: SequenceType = _

  private var declaredStreamable: Boolean = _

  @BeanProperty
  var requiredContextItemType: ItemType = AnyItemType

  @BooleanBeanProperty
  var absentFocus: Boolean = _

  @BeanProperty
  var stackFrameMap: SlotManager = _

  @BeanProperty
  var packageData: PackageData = _

  @BeanProperty
  var systemId: String = _

  @BeanProperty
  var lineNumber: Int = _

  @BeanProperty
  var rules: List[Rule] = new ArrayList()

   var slaveCopies: List[TemplateRule] = new ArrayList()

  def setMatchPattern(pattern: Pattern): Unit = {
    matchPattern = pattern
  }

  def getBody(): Expression = body

  def getChildExpression(): Expression = body

  def getLocation(): Location = this

  override def gatherProperties(consumer: BiConsumer[String, Any]): Unit = {
    consumer.accept("match", getMatchPattern.toShortString)
  }

  def setContextItemRequirements(`type`: ItemType,
                                 absentFocus: Boolean): Unit = {
    requiredContextItemType = `type`
    this.absentFocus = absentFocus
  }

  def getComponentKind: Int = StandardNames.XSL_TEMPLATE

  def getMatchPattern: Pattern = matchPattern

  def setBody(body: Expression): Unit = {
    this.body = body
    bodyIsTailCallReturner = (body.isInstanceOf[TailCallReturner])
  }

  def setHasRequiredParams(has: Boolean): Unit = {
    hasRequiredParams = has
  }

  def setRequiredType(`type`: SequenceType): Unit = {
    requiredType = `type`
  }

  def getRequiredType: SequenceType =
    if (requiredType == null) {
      SequenceType.ANY_SEQUENCE
    } else {
      requiredType
    }

  def registerRule(rule: Rule): Unit = {
    rules.add(rule)
  }

  def getContainerGranularity: Int = 0

  def getPublicId(): String = null

  def getColumnNumber(): Int = -1

  def saveLocation(): Location = this

  def getLocalParams: List[LocalParam] = {
    val result: List[LocalParam] = new ArrayList[LocalParam]()
    gatherLocalParams(getInterpretedBody, result)
    result
  }

  /*def prepareInitializer(compilation: Compilation,
                         decl: ComponentDeclaration,
                         modeName: StructuredQName): Unit = ()*/ //Compilation,ComponentDeclaration does not exist

  def initialize(): Unit = ()

  def apply(output: Outputter, context: XPathContextMajor): Unit = {
    var tc: TailCall = applyLeavingTail(output, context)
    while (tc != null) tc = tc.processLeavingTail()
  }

  def applyLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    var XPathCont = context
    val th: TypeHierarchy = XPathCont.getConfiguration.getTypeHierarchy
    if (requiredContextItemType != AnyItemType &&
      !requiredContextItemType.matches(XPathCont.getContextItem, th)) {
      val role: RoleDiagnostic = new RoleDiagnostic(
        RoleDiagnostic.MISC,
        "context item for the template rule",
        0)
      val message: String = role.composeErrorMessage(requiredContextItemType,
        XPathCont.getContextItem,
        th)
      val err = new XPathException(message, "XTTE0590")
      err.setLocation(this)
      err.setIsTypeError(true)
      throw err
    }
    if (absentFocus) {
      XPathCont = XPathCont.newMinorContext()
      XPathCont.setCurrentIterator(null)
    }
    try if (bodyIsTailCallReturner) {
      body.asInstanceOf[TailCallReturner].processLeavingTail(output, XPathCont)
    } else {
      body.process(output, XPathCont)
      null
    } catch {
      case e: UncheckedXPathException => {
        val xe: XPathException = e.getXPathException
        xe.maybeSetLocation(this)
        xe.maybeSetContext(XPathCont)
        throw xe
      }

      case e: XPathException => {
        e.maybeSetLocation(this)
        e.maybeSetContext(XPathCont)
        throw e
      }

      case e2: Exception => {
        val message: String = "Internal error evaluating template rule " +
          (if (getLineNumber > 0) " at line " + getLineNumber else "") +
          (if (getSystemId != null) " in module " + getSystemId else "")
        e2.printStackTrace()
        throw new RuntimeException(message, e2)
      }

    }
  }

  def export(presenter: ExpressionPresenter): Unit = {
    throw new UnsupportedOperationException()
  }

  def setDeclaredStreamable(streamable: Boolean): Unit = ()

  def isDeclaredStreamable: Boolean = false

  def explainProperties(presenter: ExpressionPresenter): Unit = {
    if (getRequiredContextItemType != AnyItemType) {
      val st: SequenceType = SequenceType.makeSequenceType(
        getRequiredContextItemType,
        StaticProperty.EXACTLY_ONE)
      presenter.emitAttribute("cxt", st.toAlphaCode)
    }
    var flags: String = ""
    if (!absentFocus) {
      flags += "s"
    }
    presenter.emitAttribute("flags", flags)
    if (getRequiredType != SequenceType.ANY_SEQUENCE) {
      presenter.emitAttribute("as", getRequiredType.toAlphaCode)
    }
    presenter.emitAttribute("line", getLineNumber.toString)
    presenter.emitAttribute("module", getSystemId)
    if (isDeclaredStreamable) {
      presenter.emitAttribute("streamable", "1")
    }
  }

  def getInterpretedBody: Expression = body.getInterpretedExpression

  def copy(): TemplateRule = {
    val tr: TemplateRule = new TemplateRule()
    if (body == null || matchPattern == null) {
      slaveCopies.add(tr)
    } else {
      copyTo(tr)
    }
    tr
  }

  def updateSlaveCopies(): Unit = {
    for (tr <- slaveCopies.asScala) {
      copyTo(tr)
    }
  }

   def copyTo(tr: TemplateRule): Unit = {
    if (body != null) {
      tr.body = body.copy(new RebindingMap())
    }
    if (matchPattern != null) {
      tr.matchPattern = matchPattern.copy(new RebindingMap())
    }
    tr.hasRequiredParams = hasRequiredParams
    tr.bodyIsTailCallReturner = bodyIsTailCallReturner
    tr.requiredType = requiredType
    tr.declaredStreamable = declaredStreamable
    tr.requiredContextItemType = requiredContextItemType
    tr.absentFocus = absentFocus
    tr.stackFrameMap = stackFrameMap
    tr.packageData = packageData
    tr.systemId = systemId
    tr.lineNumber = lineNumber
  }

  def setChildExpression(expr: Expression): Unit = {
    this.body = expr
  }

  override def getObjectName(): StructuredQName = null

  override def getTracingTag(): String = "xsl:template"

}
