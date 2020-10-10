package org.orbeon.saxon.expr.instruct


import java.util._
import java.util.function.BiConsumer
import java.{lang => jl, util => ju}

import org.orbeon.saxon.expr._
import org.orbeon.saxon.expr.instruct.GlobalVariable._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.lib.NamespaceConstant
import org.orbeon.saxon.model.{Affinity, AnyItemType, TypeHierarchy}
import org.orbeon.saxon.om.{GroundedValue, SequenceTool, StandardNames, StructuredQName}
import org.orbeon.saxon.query.QueryModule.MutableStack
import org.orbeon.saxon.query.{XQueryFunction, XQueryFunctionLibrary}
import org.orbeon.saxon.s9api.HostLanguage
import org.orbeon.saxon.trace.{ExpressionPresenter, TraceableComponent}
import org.orbeon.saxon.trans.{SymbolicName, Visibility, XPathException}
import org.orbeon.saxon.tree.iter.ManualIterator
import org.orbeon.saxon.utils.{Configuration, Controller}
import org.orbeon.saxon.value.{IntegerValue, SequenceType}

import scala.beans.BeanProperty
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object GlobalVariable {

  private def lookForFunctionCycles(f: XQueryFunction,
                                    referees: MutableStack[Any],
                                    globalFunctionLibrary: XQueryFunctionLibrary): Unit = {
    val body: Expression = f.getBody
    referees.push(f)
    val list: ju.List[Binding] = new ju.ArrayList[Binding](10)
    ExpressionTool.gatherReferencedVariables(body, list)
    for (b <- list.asScala if b.isInstanceOf[GlobalVariable]) {
      b.asInstanceOf[GlobalVariable]
        .lookForCycles(referees, globalFunctionLibrary)
    }
    val flist: ju.List[SymbolicName] = new ju.ArrayList[SymbolicName]()
    ExpressionTool.gatherCalledFunctionNames(body, flist)
    for (s <- flist.asScala) {
      val qf = globalFunctionLibrary.getDeclarationByKey(s)
      if (! referees.stack.contains(qf))
        lookForFunctionCycles(qf, referees, globalFunctionLibrary)
    }
    referees.pop()
  }

   def setDependencies(`var`: GlobalVariable, context: XPathContext): Unit = {
    var contxt = context
    val controller = contxt.getController
    if (! contxt.isInstanceOf[XPathContextMajor])
      contxt = getMajorCaller(contxt)
    while (contxt != null) do {
      val origin = contxt.asInstanceOf[XPathContextMajor].getOrigin
      origin match {
        case globalVar: GlobalVariable =>
          controller.registerGlobalVariableDependency(globalVar, `var`)
          return
        case _ =>
      }
      contxt = getMajorCaller(contxt)
    } while (contxt != null)
  }

  private def getMajorCaller(context: XPathContext): XPathContextMajor = {
    var caller: XPathContext = context.getCaller
    while (!(caller == null || caller.isInstanceOf[XPathContextMajor]))
      caller = caller.getCaller
    caller.asInstanceOf[XPathContextMajor]
  }
}

class GlobalVariable
  extends Actor
    with Binding
    with org.orbeon.saxon.query.Declaration
    with TraceableComponent
    with ContextOriginator {

  var references: ju.List[BindingReference] = new ju.ArrayList(10)
  @BeanProperty
  var variableQName: StructuredQName = _
  @BeanProperty
  var requiredType: SequenceType = _
  private var indexed: Boolean = _
  var isPrivate: Boolean = false
  var isAssignable: Boolean = false
  @BeanProperty
  var originalVariable: GlobalVariable = _
  var binderySlotNumber: Int = _
  var isRequiredParam: Boolean = _
  var isStatic: Boolean = _

  def init(select: Expression, qName: StructuredQName): Unit = {
    variableQName = qName
    this.body = select
  }

  override def getSymbolicName: SymbolicName =
    new SymbolicName(StandardNames.XSL_VARIABLE, variableQName)

  override def getTracingTag: String = "xsl:variable"

  override def gatherProperties(consumer: BiConsumer[String, Any]): Unit =
    consumer.accept("name", getVariableQName)

  def setStatic(declaredStatic: Boolean): Unit =
    isStatic = declaredStatic

  private def getConfiguration: Configuration =
    getPackageData.getConfiguration

  def getUltimateOriginalVariable: GlobalVariable =
    if (originalVariable == null)
      this
    else
      originalVariable.getUltimateOriginalVariable

  def setUnused(unused: Boolean): Unit = {
    this.binderySlotNumber = -9234
  }

  def getBinderySlotNumber: Int = binderySlotNumber

  def isUnused: Boolean = this.binderySlotNumber == -9234

  def setPrivate(b: Boolean): Unit = {
    isPrivate = b
  }

  def setAssignable(assignable: Boolean): Unit = {
    isAssignable = assignable
  }

  def getObjectName: StructuredQName = getVariableQName

  override def getProperty(name: String): AnyRef = null

  override def getProperties: ju.Iterator[String] =
    Collections.emptyIterator()

  def getHostLanguage: HostLanguage.HostLanguage = getPackageData.getHostLanguage

  def setIndexedVariable(): Unit =
    indexed = true

  def isIndexedVariable: Boolean = indexed

  def setContainsLocals(map: SlotManager): Unit =
    this.stackFrameMap = map

  def isGlobal: Boolean = true

  def registerReference(ref: BindingReference): Unit =
    references.add(ref)

  def iterateReferences(): ju.Iterator[BindingReference] = references.iterator

  def setBinderySlotNumber(s: Int): Unit =
    if (! isUnused)
      binderySlotNumber = s

  def setRequiredParam(requiredParam: Boolean): Unit =
    this.isRequiredParam = requiredParam

  def compile(exec: Executable, slot: Int): Unit = {
    val th = getConfiguration.getTypeHierarchy
    this.binderySlotNumber = slot
    if (this.isInstanceOf[GlobalParam]) {
      setRequiredParam(getBody == null)
    }
    var `type`: SequenceType = getRequiredType
    for (ref <- references.asScala) {
      ref.fixup(this)
      var constantValue: GroundedValue = null
      var properties: Int = 0
      val select: Expression = getBody
      select match {
        case literal: Literal if ! this.isInstanceOf[GlobalParam] =>
          val relation = th.relationship(select.getItemType, `type`.getPrimaryType)
          if (relation == Affinity.SAME_TYPE || relation == Affinity.SUBSUMED_BY) {
            constantValue = literal.value
            `type` = SequenceType.makeSequenceType(
              SequenceTool.getItemType(constantValue, th),
              SequenceTool.getCardinality(constantValue))
          }
        case _ =>
      }
      if (select != null) {
        properties = select.getSpecialProperties
      }
      properties |= StaticProperty.NO_NODES_NEWLY_CREATED
      ref.setStaticType(`type`, constantValue, properties)
    }
    if (isRequiredParam) {
      exec.registerGlobalParameter(this.asInstanceOf[GlobalParam])
    }
  }

  def typeCheck(visitor: ExpressionVisitor): Unit = {
    val value: Expression = getBody
    if (value != null) {
      value.checkForUpdatingSubexpressions()
      if (value.isUpdatingExpression) {
        throw new XPathException(
          "Initializing expression for global variable must not be an updating expression",
          "XUST0001")
      }
      val role: RoleDiagnostic = new RoleDiagnostic(
        RoleDiagnostic.VARIABLE,
        getVariableQName.getDisplayName,
        0)
      val cit: ContextItemStaticInfo = getConfiguration
        .makeContextItemStaticInfo(AnyItemType, maybeUndefined = true)
      var value2: Expression = TypeChecker.strictTypeCheck(
        value.simplify().typeCheck(visitor, cit),
        getRequiredType,
        role,
        visitor.getStaticContext)
      value2 = value2.optimize(visitor, cit)
      this.body = value2
      val map = getConfiguration.makeSlotManager
      val slots = ExpressionTool.allocateSlots(value2, 0, map)
      if (slots > 0)
        setContainsLocals(map)
      if (getRequiredType == SequenceType.ANY_SEQUENCE && ! this.isInstanceOf[GlobalParam]) {
        try {
          val itemType = value.getItemType
          val cardinality = value.getCardinality
          this.requiredType = SequenceType.makeSequenceType(itemType, cardinality)
          var constantValue: GroundedValue = null
          value2 match {
            case literal: Literal =>
              constantValue = literal.value
            case _ =>
          }
          for (reference <- references.asScala
               if reference.isInstanceOf[VariableReference]) {
            reference
              .asInstanceOf[VariableReference]
              .refineVariableType(itemType,
                cardinality,
                constantValue,
                value.getSpecialProperties)
          }
        } catch {
          case _: Exception =>
        }
      }
    }
  }

  def lookForCycles(referees: MutableStack[Any], globalFunctionLibrary: XQueryFunctionLibrary): Unit = {

    if (referees.stack.contains(this)) {
      val s = referees.stack.indexOf(this)
      referees.push(this)
      val messageBuilder= new jl.StringBuilder("Circular definition of global variable: $" + getVariableQName.getDisplayName)
      for (i <- s until referees.stack.size - 1) {
        if (i != s)
          messageBuilder.append(", which")
        referees.stack(i + 1) match {
          case next: GlobalVariable =>
            messageBuilder
              .append(" uses $")
              .append(next.getVariableQName.getDisplayName)
          case next: XQueryFunction =>
            messageBuilder
              .append(" calls ")
              .append(next.getFunctionName.getDisplayName)
              .append("#")
              .append(next.getNumberOfArguments)
              .append("()")
          case _ =>
        }
      }
      var message: String = messageBuilder.toString
      message += '.'
      val err = new XPathException(message)
      var errorCode: String = null
      errorCode =
        if (getPackageData.isXSLT)
          "XTDE0640"
        else if (s == 0 && referees.stack.size == 2)
          "XPST0008"
        else
          "XQDY0054"
      err.setErrorCode(errorCode)
      err.setIsStaticError(true)
      err.setLocation(getLocation)
      throw err
    }
    val select: Expression = getBody
    if (select != null) {
      referees.push(this)
      val list: ju.List[Binding] = new ju.ArrayList[Binding](10)
      ExpressionTool.gatherReferencedVariables(select, list)
      for (b <- list.asScala if b.isInstanceOf[GlobalVariable]) {
        b.asInstanceOf[GlobalVariable]
          .lookForCycles(referees, globalFunctionLibrary)
      }
      val flist: ju.List[SymbolicName] = new ju.ArrayList[SymbolicName]()
      ExpressionTool.gatherCalledFunctionNames(select, flist)
      for (s <- flist.asScala) {
        val f = globalFunctionLibrary.getDeclarationByKey(s)
        if (! referees.stack.contains(f))
          lookForFunctionCycles(f, referees, globalFunctionLibrary)
      }
      referees.pop()
    }
  }

  def getSelectValue(context: XPathContext, target: Component): GroundedValue = {
    val select: Expression = getBody
    if (select == null) {
      throw new AssertionError("*** No select expression for global variable $" + getVariableQName.getDisplayName + "!!")
    } else select match {
      case literal: Literal =>
        literal.value
      case _ =>
        try {
          val controller: Controller = context.getController
          val exec: Executable = controller.getExecutable
          var hasAccessToGlobalContext: Boolean = true
          /* if (exec.isInstanceOf[PreparedStylesheet]) { //PreparedStylesheet scala class does not exist
             hasAccessToGlobalContext = target == null ||
               target.getDeclaringPackage ==
                 exec.asInstanceOf[PreparedStylesheet].getTopLevelPackage //PreparedStylesheet scala class does not exist
           }*/
          val c2: XPathContextMajor = context.newCleanContext()
          c2.setOrigin(this)
          if (hasAccessToGlobalContext) {
            val mi: ManualIterator = new ManualIterator(
              context.getController.getGlobalContextItem)
            c2.setCurrentIterator(mi)
          } else {
            c2.setCurrentIterator(null)
          }
          if (getStackFrameMap != null) {
            c2.openStackFrame(getStackFrameMap)
          }
          c2.setCurrentComponent(target)
          val savedOutputState: Int = c2.getTemporaryOutputState
          c2.setTemporaryOutputState(StandardNames.XSL_VARIABLE)
          c2.setCurrentOutputUri(null)
          var result: GroundedValue = null
          result =
            if (indexed)
              c2.getConfiguration.makeSequenceExtent(select,
                FilterExpression.FILTERED,
                c2)
            else select.iterate(c2).materialize()
          c2.setTemporaryOutputState(savedOutputState)
          result
        } catch {
          case e: XPathException =>
            if (! getVariableQName.hasURI(NamespaceConstant.SAXON_GENERATED_VARIABLE))
              e.setIsGlobalError(true)
            throw e
        }
    }
  }

  def evaluateVariable(context: XPathContext): GroundedValue = {
    val controller: Controller = context.getController
    assert(controller != null)
    val b: Bindery = controller.getBindery(getPackageData)
    val v: GroundedValue = b.getGlobalVariable(getBinderySlotNumber)
    if (v != null) {
      v
    } else {
      actuallyEvaluate(context, null)
    }
  }

  def evaluateVariable(context: XPathContext,
                       target: Component): GroundedValue = {
    val controller: Controller = context.getController
    assert(controller != null)
    val b: Bindery = controller.getBindery(getPackageData)
    if (b == null) {
      throw new AssertionError()
    }
    val v: GroundedValue = b.getGlobalVariable(getBinderySlotNumber)
    if (v != null) {
      v match {
        case value: Bindery.FailureValue =>
          throw value.getObject
        case _ =>
      }
      v
    } else {
      actuallyEvaluate(context, target)
    }
  }

   def actuallyEvaluate(context: XPathContext, target: Component): GroundedValue = {
    val controller = context.getController
    assert(controller != null)
    val b = controller.getBindery(getPackageData)
     try {
      setDependencies(this, context)
      val go = b.setExecuting(this)
      if (! go)
        return b.getGlobalVariable(getBinderySlotNumber)
      var value = getSelectValue(context, target)
      if (indexed) {
        value = controller.getConfiguration
          .obtainOptimizer
          .makeIndexedValue(value.iterate())
      }
       b.saveGlobalVariableValue(this, value)
     } catch {
      case err: XPathException =>
        b.setNotExecuting(this)
        if (err.isInstanceOf[XPathException.Circularity]) {
          err.setErrorCode(if (getPackageData.isXSLT) "XTDE0640" else "XQDY0054")
          err.setXPathContext(context)
          err.setIsGlobalError(true)
          b.setGlobalVariable(this, new Bindery.FailureValue(err))
          err.setLocation(getLocation)
          throw err
        } else {
          throw err
        }
    }
  }

  def getIntegerBoundsForVariable: Array[IntegerValue] =
    if (getBody == null) null else getBody.getIntegerBounds

  def getLocalSlotNumber: Int = 0

  def getDescription: String =
    if (variableQName.hasURI(NamespaceConstant.SAXON_GENERATED_VARIABLE)) {
      "optimizer-generated global variable select=\"" + getBody
        .toShortString +
        '"'
    } else {
      "global variable " + getVariableQName.getDisplayName
    }

  def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit = ()

  override def export(presenter: ExpressionPresenter): Unit = {
    val asParam: Boolean = this.isInstanceOf[GlobalParam] && !isStatic
    presenter.startElement(if (asParam) "globalParam" else "globalVariable")
    presenter.emitAttribute("name", getVariableQName)
    presenter.emitAttribute("as", getRequiredType.toAlphaCode)
    presenter.emitAttribute("line", getLineNumber.toString)
    presenter.emitAttribute("module", getSystemId)
    if (getStackFrameMap != null)
      presenter.emitAttribute("slots", getStackFrameMap.getNumberOfVariables.toString)
    if (getDeclaringComponent != null) {
      val vis: Visibility.Visibility = getDeclaringComponent.getVisibility
      if (vis != null)
        presenter.emitAttribute("visibility", vis.toString)
    }
    val flags: String = getFlags
    if (!flags.isEmpty)
      presenter.emitAttribute("flags", flags)
    if (getBody != null)
      getBody.export(presenter)
    presenter.endElement()
  }

   def getFlags: String = {
    var flags: String = ""
    if (isAssignable)
      flags += "a"
    if (indexed)
      flags += "x"
    if (isRequiredParam)
      flags += "r"
    if (isStatic)
      flags += "s"
    flags
  }
}
