package net.sf.saxon.expr.instruct

import net.sf.saxon.event.Outputter
import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.expr._
import net.sf.saxon.expr.parser._
import net.sf.saxon.model._
import net.sf.saxon.om.FocusIterator
import net.sf.saxon.om.Item
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.om.StandardNames
import net.sf.saxon.regex.RegexIterator
import net.sf.saxon.regex.RegularExpression
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.SequenceType
import net.sf.saxon.value.StringValue
import java.util.ArrayList
import java.util.List

import AnalyzeString._
import net.sf.saxon.utils.Configuration

import scala.jdk.CollectionConverters._

object AnalyzeString {

  private val ACTION: OperandRole = new OperandRole(
    OperandRole.USES_NEW_FOCUS | OperandRole.HIGHER_ORDER,
    OperandUsage.NAVIGATION)

  private val SELECT: OperandRole = new OperandRole(OperandRole.SETS_NEW_FOCUS,
    OperandUsage.ABSORPTION,
    SequenceType.SINGLE_STRING)

}

class AnalyzeString(select: Expression,
                    regex: Expression,
                    flags: Expression,
                    matching: Expression,
                    nonMatching: Expression,
                    private var pattern: RegularExpression)
  extends Instruction
    with ContextOriginator {

  var selExp = select
  var regExp = regex
  var flagExp = flags
  private var selectOp: Operand = new Operand(this, selExp, SELECT)

  private var regexOp: Operand =
    new Operand(this, regExp, OperandRole.SINGLE_ATOMIC)

  private var flagsOp: Operand =
    new Operand(this, flagExp, OperandRole.SINGLE_ATOMIC)

  private var matchingOp: Operand = _

  private var nonMatchingOp: Operand = _

  if (matching != null) {
    matchingOp = new Operand(this, matching, ACTION)
  }

  if (nonMatching != null) {
    nonMatchingOp = new Operand(this, nonMatching, ACTION)
  }

  def getSelect(): Expression = selectOp.getChildExpression

  def setSelect(selExp: Expression): Unit = {
    selectOp.setChildExpression(selExp)
  }

  def getRegex(): Expression = regexOp.getChildExpression

  def setRegex(regExp: Expression): Unit = {
    regexOp.setChildExpression(regExp)
  }

  def getFlags(): Expression = flagsOp.getChildExpression

  def setFlags(flagExp: Expression): Unit = {
    flagsOp.setChildExpression(flagExp)
  }

  def getMatching(): Expression =
    if (matchingOp == null) null else matchingOp.getChildExpression

  def setMatching(matching: Expression): Unit = {
    if (matchingOp != null) {
      matchingOp.setChildExpression(matching)
    } else {
      matchingOp = new Operand(this, matching, ACTION)
    }
  }

  def getNonMatching(): Expression =
    if (nonMatchingOp == null) null else nonMatchingOp.getChildExpression

  def setNonMatching(nonMatching: Expression): Unit = {
    if (nonMatchingOp != null) {
      nonMatchingOp.setChildExpression(nonMatching)
    } else {
      nonMatchingOp = new Operand(this, nonMatching, ACTION)
    }
  }

  override def getInstructionNameCode(): Int = StandardNames.XSL_ANALYZE_STRING

  override def operands(): java.lang.Iterable[Operand] =
    operandSparseList(selectOp, regexOp, flagsOp, matchingOp, nonMatchingOp)

  override def getImplementationMethod(): Int =
    Expression.PROCESS_METHOD | Expression.ITERATE_METHOD

  def getPatternExpression(): RegularExpression = pattern

  override def allowExtractingCommonSubexpressions(): Boolean = false

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    selectOp.typeCheck(visitor, contextInfo)
    regexOp.typeCheck(visitor, contextInfo)
    flagsOp.typeCheck(visitor, contextInfo)
    if (matchingOp != null) {
      matchingOp.typeCheck(
        visitor,
        config.makeContextItemStaticInfo(BuiltInAtomicType.STRING, false))
    }
    if (nonMatchingOp != null) {
      nonMatchingOp.typeCheck(
        visitor,
        config.makeContextItemStaticInfo(BuiltInAtomicType.STRING, false))
    }
    val tc: TypeChecker = visitor.getConfiguration.getTypeChecker(false)
    var role: RoleDiagnostic = new RoleDiagnostic(RoleDiagnostic.INSTRUCTION,
      "analyze-string/select",
      0)
    val required: SequenceType = SequenceType.OPTIONAL_STRING
    this.selExp = tc.staticTypeCheck(getSelect, required, role, visitor)
    role =
      new RoleDiagnostic(RoleDiagnostic.INSTRUCTION, "analyze-string/regex", 0)
    this.regExp =
      tc.staticTypeCheck(getRegex, SequenceType.SINGLE_STRING, role, visitor)
    role =
      new RoleDiagnostic(RoleDiagnostic.INSTRUCTION, "analyze-string/flags", 0)
    this.flagExp =
      tc.staticTypeCheck(getFlags, SequenceType.SINGLE_STRING, role, visitor)
    this
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val config: Configuration = visitor.getConfiguration
    selectOp.optimize(visitor, contextInfo)
    regexOp.optimize(visitor, contextInfo)
    flagsOp.optimize(visitor, contextInfo)
    if (matchingOp != null) {
      matchingOp.optimize(
        visitor,
        config.makeContextItemStaticInfo(BuiltInAtomicType.STRING, false))
    }
    if (nonMatchingOp != null) {
      nonMatchingOp.optimize(
        visitor,
        config.makeContextItemStaticInfo(BuiltInAtomicType.STRING, false))
    }
    val warnings: List[String] = new ArrayList[String]()
    precomputeRegex(config, warnings)
    for (w <- warnings.asScala) {
      visitor.getStaticContext.issueWarning(w, getLocation)
    }
    this
  }

  def precomputeRegex(config: Configuration, warnings: List[String]): Unit = {
    if (pattern == null && getRegex.isInstanceOf[StringLiteral] &&
      getFlags.isInstanceOf[StringLiteral]) {
      try {
        val regex: CharSequence =
          this.getRegex.asInstanceOf[StringLiteral].getStringValue
        val flagstr: CharSequence =
          getFlags.asInstanceOf[StringLiteral].getStringValue
        val hostLang: String = "XP30"
        pattern = config.compileRegularExpression(regex,
          flagstr.toString,
          hostLang,
          warnings) // need changes in Configuration class
      } catch {
        case err: XPathException => {
          if ("XTDE1150" == err.getErrorCodeLocalPart) {
            throw err
          }
          if ("FORX0001" == err.getErrorCodeLocalPart) {
            invalidRegex("Error in regular expression flags: " + err,
              "FORX0001")
          } else {
            invalidRegex("Error in regular expression: " + err,
              err.getErrorCodeLocalPart)
          }
        }

      }
    }
  }

  private def invalidRegex(message: String, errorCode: String): Unit = {
    pattern = null
    val err: XPathException = new XPathException(message, errorCode)
    err.setLocation(getLocation)
    throw err
  }

  def copy(rm: RebindingMap): Expression = {
    val a2: AnalyzeString = new AnalyzeString(copy(getSelect, rm),
      copy(getRegex, rm),
      copy(getFlags, rm),
      copy(getMatching, rm),
      copy(getNonMatching, rm),
      pattern)
    ExpressionTool.copyLocationInfo(this, a2)
    a2
  }

  private def copy(exp: Expression, rebindings: RebindingMap): Expression =
    if (exp == null) null else exp.copy(rebindings)

  override def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = {
    if (getMatching != null) {
      getMatching.checkPermittedContents(parentType, false)
    }
    if (getNonMatching != null) {
      getNonMatching.checkPermittedContents(parentType, false)
    }
  }

  override def getItemType(): ItemType =
    if (getMatching != null) {
      if (getNonMatching != null) {
        val th: TypeHierarchy = getConfiguration.getTypeHierarchy
        Type.getCommonSuperType(getMatching.getItemType,
          getNonMatching.getItemType,
          th)
      } else {
        getMatching.getItemType
      }
    } else {
      if (getNonMatching != null) {
        getNonMatching.getItemType
      } else {
        ErrorType.getInstance
      }
    }

  override def computeDependencies(): Int = {
    var dependencies: Int = 0
    dependencies |= getSelect.getDependencies
    dependencies |= getRegex.getDependencies
    dependencies |= getFlags.getDependencies
    if (getMatching != null) {
      dependencies |= getMatching.getDependencies &
        ~(StaticProperty.DEPENDS_ON_FOCUS | StaticProperty.DEPENDS_ON_REGEX_GROUP)
    }
    if (getNonMatching != null) {
      dependencies |= getNonMatching.getDependencies &
        ~(StaticProperty.DEPENDS_ON_FOCUS | StaticProperty.DEPENDS_ON_REGEX_GROUP)
    }
    dependencies
  }

  def processLeavingTail(output: Outputter, context: XPathContext): TailCall = {
    val iter: RegexIterator = getRegexIterator(context)
    val c2: XPathContextMajor = context.newContext()
    c2.setOrigin(this)
    val focusIter: FocusIterator = c2.trackFocus(iter)
    c2.setCurrentRegexIterator(iter)
    val pipe: PipelineConfiguration = output.getPipelineConfiguration
    pipe.setXPathContext(c2)
    var it: Item = null
    while ((it = focusIter.next()) != null) if (iter.isMatching) {
      if (getMatching != null) {
        getMatching.process(output, c2)
      }
    } else {
      if (getNonMatching != null) {
        getNonMatching.process(output, c2)
      }
    }
    pipe.setXPathContext(context)
    null
  }

  private def getRegexIterator(context: XPathContext): RegexIterator = {
    val input: CharSequence = getSelect.evaluateAsString(context)
    var re: RegularExpression = pattern
    if (re == null) {
      val flagstr: String = getFlags.evaluateAsString(context).toString
      val regexString: StringValue =
        getRegex.evaluateItem(context).asInstanceOf[StringValue]
      re = context.getConfiguration.compileRegularExpression(
        getRegex.evaluateAsString(context),
        flagstr,
        "XP30",
        null) // Configuration class changes
    }
    re.analyze(input)
  }

  override def iterate(context: XPathContext): SequenceIterator = {
    val iter: RegexIterator = getRegexIterator(context)
    val c2: XPathContextMajor = context.newContext()
    c2.setOrigin(this)
    c2.trackFocus(iter)
    c2.setCurrentRegexIterator(iter)
    val fn: AnalyzeMappingFunction =
      new AnalyzeMappingFunction(iter, c2, getNonMatching, getMatching)
    new ContextMappingIterator(fn, c2)
  }

  override def getExpressionName(): String = "analyzeString"

  def export(out: ExpressionPresenter): Unit = {
    out.startElement("analyzeString", this)
    out.setChildRole("select")
    getSelect.export(out)
    out.setChildRole("regex")
    getRegex.export(out)
    out.setChildRole("flags")
    getFlags.export(out)
    if (getMatching != null) {
      out.setChildRole("matching")
      getMatching.export(out)
    }
    if (getNonMatching != null) {
      out.setChildRole("nonMatching")
      getNonMatching.export(out)
    }
    out.endElement()
  }

}
