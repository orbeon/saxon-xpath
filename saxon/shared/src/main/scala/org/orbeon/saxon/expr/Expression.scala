package org.orbeon.saxon.expr

import org.orbeon.saxon.event.{Outputter, ReceiverOption}
import org.orbeon.saxon.expr.Expression._
import org.orbeon.saxon.expr.parser._
import org.orbeon.saxon.functions.{KeyFn, SuperId, SystemFunction}
import org.orbeon.saxon.lib.Logger
import org.orbeon.saxon.model.{AtomicType, ItemType, SchemaType, UType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.pattern.{NodeKindTest, NodeSetPattern, NodeTest, Pattern}
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trace.{ExpressionPresenter, Traceable}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.{EmptyIterator, SingletonIterator}
import org.orbeon.saxon.tree.jiter.MonoIterator
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value._
import org.orbeon.saxon.z.IntHashSet

import java.net.URI
import java.{lang => jl, util => ju}
import scala.beans.BeanProperty
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._


object Expression {

  val EVALUATE_METHOD         : Int    = 1
  val ITERATE_METHOD          : Int    = 2
  val PROCESS_METHOD          : Int    = 4
  val WATCH_METHOD            : Int    = 8
  val ITEM_FEED_METHOD        : Int    = 16
  val EFFECTIVE_BOOLEAN_VALUE : Int    = 32
  val UPDATE_METHOD           : Int    = 64
  val MAX_COST                : Double = 1e9

  val UNBOUNDED_LOWER: IntegerValue = IntegerValue
    .makeIntegerValue(new DoubleValue(-1e100))
    .asInstanceOf[IntegerValue]

  val UNBOUNDED_UPPER: IntegerValue = IntegerValue
    .makeIntegerValue(new DoubleValue(+1e100))
    .asInstanceOf[IntegerValue]

  val MAX_STRING_LENGTH: IntegerValue =
    Int64Value.makeIntegerValue(jl.Integer.MAX_VALUE)

  val MAX_SEQUENCE_LENGTH: IntegerValue =
    Int64Value.makeIntegerValue(jl.Integer.MAX_VALUE)

  private def gatherSlotsUsed(exp: Expression, slots: IntHashSet): Unit =
    exp.getInterpretedExpression match {
      case localVarRef: LocalVariableReference =>
        slots.add(localVarRef.getSlotNumber)
      case suppliedParameterRef: SuppliedParameterReference =>
        val slot = suppliedParameterRef.getSlotNumber
        slots.add(slot)
      case expVar =>
        for (o <- expVar.operands.asScala)
          gatherSlotsUsed(o.getChildExpression, slots)
    }
}

abstract class Expression
  extends IdentityComparable
    with ExportAgent
    with Locatable
    with Traceable {

  var staticProperties: Int = -1
  private var location: Location = Loc.NONE
  @BeanProperty
  var parentExpression: Expression = _
  private var retainedStaticContext: RetainedStaticContext = _
  private var slotsUsed: Array[Int] = _
  @BeanProperty
  var evaluationMethod: Int = _
  private var extraProperties: ju.Map[String, Any] = _
  private var cost: Double = -1
  private var cachedHashCode: Int = -1

  def getExpressionName: String = getClass.getSimpleName
  def operands: jl.Iterable[Operand] = ju.Collections.emptyList()
  def getInterpretedExpression: Expression = this

  def checkedOperands: jl.Iterable[Operand] = {
    val ops = operands
    for (o <- ops.asScala) {
      val child = o.getChildExpression
      val badOperand = o.getParentExpression ne this
      val badExpression = child.getParentExpression ne this
      if (badOperand || badExpression) {
        val message = "*** Bad parent pointer found in " + (if (badOperand)
          "operand "
        else
          "expression ") +
          child.toShortString +
          " at " +
          child.getLocation.getSystemId +
          "#" +
          child.getLocation.getLineNumber +
          " ***"
        val config: Configuration = getConfiguration
        val logger: Logger = if (config == null) null else config.getLogger
        if (logger != null)
          logger.warning(message)
        else
          throw new IllegalStateException(message)
        child.setParentExpression(Expression.this)
      }
      if (child.getRetainedStaticContext == null)
        child.setRetainedStaticContext(getRetainedStaticContext)
    }
    ops
  }

  def operandList(a: Operand*): ju.List[Operand] = ju.Arrays.asList(a: _*)

  def operandSparseList(a: Operand*): ju.List[Operand] = {
    val operanda = new ju.ArrayList[Operand]()
    for (o <- a if o != null)
      operanda.add(o)
    operanda
  }

  def verifyParentPointers(): Expression = {
    for (o <- operands.asScala) {
      val parent = o.getChildExpression.getParentExpression
      if (parent ne this) {
        throw new IllegalStateException(
          "Invalid parent pointer in " + parent.toShortString +
            " subexpression " +
            o.getChildExpression.toShortString)
      }
      if (o.getParentExpression ne this) {
        throw new IllegalStateException(
          "Invalid parent pointer in operand object " + parent
            .toShortString +
            " subexpression " +
            o.getChildExpression.toShortString)
      }
      if (ExpressionTool.findOperand(parent, o.getChildExpression) == null) {
        throw new IllegalStateException(
          "Incorrect parent pointer in " + parent.toShortString +
            " subexpression " +
            o.getChildExpression.toShortString)
      }
      o.getChildExpression.verifyParentPointers()
    }
    this
  }

  def restoreParentPointers(): Unit =
    for (o <- operands.asScala) {
      val child = o.getChildExpression
      child.setParentExpression(Expression.this)
      child.restoreParentPointers()
    }

  def getImplementationMethod: Int

  def implementsStaticTypeCheck(): Boolean = false

  def hasVariableBinding(binding: Binding): Boolean = false

  def isLiftable(forStreaming: Boolean): Boolean = {
    val p = getSpecialProperties
    val d = getDependencies
    (p & StaticProperty.NO_NODES_NEWLY_CREATED) != 0 && (p & StaticProperty.HAS_SIDE_EFFECTS) == 0 &&
      ((d & StaticProperty.DEPENDS_ON_ASSIGNABLE_GLOBALS) == 0) &&
      ((d & StaticProperty.DEPENDS_ON_POSITION) == 0) &&
      ((d & StaticProperty.DEPENDS_ON_LAST) == 0)
  }

  def getScopingExpression: Expression = {
    val d = getIntrinsicDependencies & StaticProperty.DEPENDS_ON_FOCUS
    if (d != 0) {
      if (d == StaticProperty.DEPENDS_ON_CONTEXT_DOCUMENT)
        ExpressionTool.getContextDocumentSettingContainer(this)
      else
        ExpressionTool.getFocusSettingContainer(this)
    } else {
      null
    }
  }

  def isMultiThreaded(config: Configuration): Boolean = false

  def allowExtractingCommonSubexpressions(): Boolean = true

  def simplify(): Expression = {
    simplifyChildren()
    Expression.this
  }

  def simplifyChildren(): Unit = {
    for (o <- operands.asScala if o != null) {
      val e = o.getChildExpression
      if (e != null) {
        val f = e.simplify()
        o.setChildExpression(f)
      }
    }
  }

  def setRetainedStaticContext(rsc: RetainedStaticContext): Unit = {
    if (rsc != null) {
      retainedStaticContext = rsc
      for (o <- operands.asScala if o != null) {
        val child = o.getChildExpression
        if (child != null && child.retainedStaticContext == null) {
          child.setRetainedStaticContext(rsc)
        }
      }
    }
  }

  def setRetainedStaticContextThoroughly(rsc: RetainedStaticContext): Unit = {
    var rscVar = rsc
    if (rscVar != null) {
      retainedStaticContext = rscVar
      for (o <- operands.asScala if o != null) {
        val child = o.getChildExpression
        if (child != null) {
          if (child.getLocalRetainedStaticContext == null) {
            child.setRetainedStaticContextThoroughly(rsc)
          } else {
            rscVar = child.getLocalRetainedStaticContext
            for (p <- child.operands.asScala) {
              val grandchild = p.getChildExpression
              if (grandchild != null)
                grandchild.setRetainedStaticContextThoroughly(rscVar)
            }
          }
        }
      }
    }
  }

  def setRetainedStaticContextLocally(rsc: RetainedStaticContext): Unit = {
    if (rsc != null)
      retainedStaticContext = rsc
  }

  def getRetainedStaticContext: RetainedStaticContext = {
    if (retainedStaticContext == null) {
      val parent: Expression = getParentExpression
      assert(parent != null)
      retainedStaticContext = parent.getRetainedStaticContext
      assert(retainedStaticContext != null)
    }
    retainedStaticContext
  }

  def getLocalRetainedStaticContext: RetainedStaticContext =
    retainedStaticContext

  def getStaticBaseURIString: String =
    getRetainedStaticContext.getStaticBaseUriString

  def getStaticBaseURI: URI = getRetainedStaticContext.getStaticBaseUri

  def isCallOn(function: Class[_ <: SystemFunction]): Boolean = false

  def typeCheck(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Expression = {
    typeCheckChildren(visitor, contextInfo)
    Expression.this
  }

  final def typeCheckChildren(visitor: ExpressionVisitor, contextInfo: ContextItemStaticInfo): Unit =
    for (o <- operands.asScala)
      o.typeCheck(visitor, contextInfo)

  def staticTypeCheck(req: SequenceType,
                      backwardsCompatible: Boolean,
                      role: RoleDiagnostic,
                      visitor: ExpressionVisitor): Expression =
    throw new UnsupportedOperationException("staticTypeCheck")

  def optimize(visitor: ExpressionVisitor,
               contextInfo: ContextItemStaticInfo): Expression = {
    if (visitor.incrementAndTestDepth()) {
      optimizeChildren(visitor, contextInfo)
      visitor.decrementDepth()
    }
    Expression.this
  }

  def optimizeChildren(visitor: ExpressionVisitor,
                       contextInfo: ContextItemStaticInfo): Unit =
    for (o <- operands.asScala)
      o.optimize(visitor, contextInfo)

  def prepareForStreaming(): Unit = ()

  def getCost: Double = {
    if (cost < 0) {
      var i: Double = getNetCost
      breakable {
        for (o <- operands.asScala) {
          i += o.getChildExpression.getCost
          if (i > MAX_COST)
            break()
        }
      }
      cost = i
    }
    cost
  }

  def getNetCost: Int = 1

  def unordered(retainAllNodes: Boolean, forStreaming: Boolean): Expression =
    Expression.this

  def getSpecialProperties: Int = {
    if (staticProperties == -1)
      computeStaticProperties()
    staticProperties & StaticProperty.SPECIAL_PROPERTY_MASK
  }

  def hasSpecialProperty(property: Int): Boolean =
    (getSpecialProperties & property) != 0

  def getCardinality: Int = {
    if (staticProperties == -1)
      computeStaticProperties()
    staticProperties & StaticProperty.CARDINALITY_MASK
  }

  def getItemType: ItemType

  def getStaticType: SequenceType =
    SequenceType.makeSequenceType(getItemType, getCardinality)

  def getStaticUType(contextItemType: UType): UType = UType.ANY

  def getDependencies: Int = {
    if (staticProperties == -1)
      computeStaticProperties()
    staticProperties & StaticProperty.DEPENDENCY_MASK
  }

  def getIntegerBounds: Array[IntegerValue] = null
  def setFlattened(flattened: Boolean): Unit = ()
  def setFiltered(filtered: Boolean): Unit = ()
  def evaluateItem(context: XPathContext): Item = iterate(context).next()

  def iterate(context: XPathContext): SequenceIterator = {
    val value = evaluateItem(context)
    if (value == null)
      EmptyIterator.emptyIterator
    else
      SingletonIterator.rawIterator(value)
  }

  def effectiveBooleanValue(context: XPathContext): Boolean =
    try
      ExpressionTool.effectiveBooleanValue(iterate(context))
    catch {
      case e: XPathException =>
        e.maybeSetFailingExpression(this)
        e.maybeSetContext(context)
        throw e
    }

  def evaluateAsString(context: XPathContext): CharSequence = {
    val o = evaluateItem(context)
    val value = o.asInstanceOf[StringValue]
    if (value == null)
      return ""
    value.getStringValueCS
  }

  def process(output: Outputter, context: XPathContext): Unit = {
    val m = getImplementationMethod
    val hasEvaluateMethod = (m & EVALUATE_METHOD) != 0
    val hasIterateMethod = (m & ITERATE_METHOD) != 0
    try
      if (hasEvaluateMethod && (! hasIterateMethod || !Cardinality.allowsMany(getCardinality))) {
        val item = evaluateItem(context)
        if (item != null)
          output.append(item, getLocation, ReceiverOption.ALL_NAMESPACES)
      } else if (hasIterateMethod) {
        iterate(context).forEachOrFail(it => output.append(it, getLocation, ReceiverOption.ALL_NAMESPACES))
      } else {
        throw new AssertionError("process() is not implemented in the subclass " + getClass)
      }
    catch {
      case e: XPathException =>
        e.maybeSetLocation(getLocation)
        e.maybeSetContext(context)
        throw e
    }
  }

  def evaluatePendingUpdates(context: XPathContext,
                             pul: PendingUpdateList): Unit =
    if (isVacuousExpression)
      iterate(context).next()
    else
      throw new UnsupportedOperationException("Expression " + getClass + " is not an updating expression")

  override def toString: String = {

    val buff = new FastStringBuffer(FastStringBuffer.C64)
    var className = getClass.getName

    breakable {
      while (true) {
        val dot = className.indexOf('.')
        if (dot >= 0) {
          className = className.substring(dot + 1)
        } else {
          break()
        }
      }
    }
    buff.append(className)

    var first = true
    for (o <- operands.asScala) {
      buff.append(if (first) "(" else ", ")
      buff.append(o.getChildExpression.toString)
      first = false
    }

    if (! first)
      buff.append(")")

    buff.toString
  }

  def toShortString: String = getExpressionName

  def export(out: ExpressionPresenter): Unit

  def explain(out: Logger): Unit = {
    val ep = new ExpressionPresenter(getConfiguration, out)
    val options = new ExpressionPresenter.ExportOptions
    options.explaining = true
    ep.setOptions(options)
    try
      export(ep)
    catch {
      case e: XPathException =>
        ep.startElement("failure")
        ep.emitAttribute("message", e.getMessage)
        ep.endElement()
    }
    ep.close()
  }

  def checkPermittedContents(parentType: SchemaType, whole: Boolean): Unit = ()

  final def adoptChildExpression(child: Expression): Unit = {

    if (child == null)
      return

    child.setParentExpression(Expression.this)
    if (child.retainedStaticContext == null)
      child.retainedStaticContext = retainedStaticContext

    if (getLocation == null || getLocation == Loc.NONE)
      ExpressionTool.copyLocationInfo(child, Expression.this)
    else if (child.getLocation == null || child.getLocation == Loc.NONE)
      ExpressionTool.copyLocationInfo(Expression.this, child)

    resetLocalStaticProperties()
  }

  def setLocation(id: Location): Unit =
    location = id

  def getLocation: Location = {
    var limit = 0
    var exp: Expression = this
    while (limit < 10)
      if ((exp.location == null || exp.location == Loc.NONE) && exp.getParentExpression != null) {
        exp = exp.getParentExpression
        limit = limit + 1
      } else {
        return exp.location
      }
    exp.location
  }

  def getConfiguration: Configuration =
    getRetainedStaticContext.getConfiguration

  def getPackageData: PackageData = getRetainedStaticContext.getPackageData

  def isInstruction: Boolean = false

  def computeStaticProperties(): Unit =
    staticProperties = computeDependencies() | computeCardinality() | computeSpecialProperties()

  def resetLocalStaticProperties(): Unit = {
    staticProperties = -1
    cachedHashCode = -1
  }

  def isStaticPropertiesKnown: Boolean = staticProperties != -1

  def computeCardinality(): Int

  def computeSpecialProperties(): Int = 0

  def computeDependencies(): Int = {
    var dependencies = getIntrinsicDependencies
    for (o <- operands.asScala)
      if (o.hasSameFocus)
        dependencies |= o.getChildExpression.getDependencies
      else
        dependencies |= o.getChildExpression.getDependencies & ~StaticProperty.DEPENDS_ON_FOCUS
    dependencies
  }

  def getIntrinsicDependencies: Int = 0

  def setStaticProperty(prop: Int): Unit = {
    if (staticProperties == -1)
      computeStaticProperties()
    staticProperties |= prop
  }

  def checkForUpdatingSubexpressions(): Unit =
    for (o <- operands.asScala) {
      val sub = o.getChildExpression
      if (sub == null)
        throw new NullPointerException
      sub.checkForUpdatingSubexpressions()
      if (sub.isUpdatingExpression) {
        val err = new XPathException(
          "Updating expression appears in a context where it is not permitted",
          "XUST0001")
        err.setLocation(sub.getLocation)
        throw err
      }
    }

  def isUpdatingExpression: Boolean =
    operands.asScala.exists(_.getChildExpression.isUpdatingExpression)

  def isVacuousExpression: Boolean = false

  def copy(rebindings: RebindingMap): Expression

  def suppressValidation(parentValidationMode: Int): Unit = ()

  def markTailFunctionCalls(qName: StructuredQName, arity: Int): Int =
    UserFunctionCall.NOT_TAIL_CALL

  def toPattern(config: Configuration): Pattern = {
    val `type` = getItemType
    if (((getDependencies & StaticProperty.DEPENDS_ON_NON_DOCUMENT_FOCUS) == 0) &&
      (`type`.isInstanceOf[NodeTest] || this.isInstanceOf[VariableReference]))
      new NodeSetPattern(Expression.this)
    else if (isCallOn(classOf[KeyFn]) || isCallOn(classOf[SuperId]))
      new NodeSetPattern(Expression.this)
    else
      throw new XPathException("Cannot convert the expression {" + this + "} to a pattern")
  }

  def getSlotsUsed: Array[Int] = synchronized {
    if (slotsUsed != null) {
      slotsUsed
    } else {
      val slots = new IntHashSet(10)
      gatherSlotsUsed(Expression.this, slots)
      slotsUsed = Array.ofDim[Int](slots.size)
      var i = 0
      val iter = slots.iterator
      while (iter.hasNext) {
        slotsUsed(i) = iter.next()
        i += 1
      }
      scala.util.Sorting.quickSort(slotsUsed)
      slotsUsed
    }
  }

  def dynamicError(message: String,
                   code: String,
                   context: XPathContext): Throwable = {
    val err = new XPathException(message, code, getLocation)
    err.setXPathContext(context)
    err.setFailingExpression(this)
    throw err
  }

  def typeError(message: String,
                errorCode: String,
                context: XPathContext): Unit = {
    val e = new XPathException(message, errorCode, getLocation)
    e.setIsTypeError(true)
    e.setXPathContext(context)
    e.setFailingExpression(this)
    throw e
  }

  def getTracingTag: String = getExpressionName

  def getObjectName: StructuredQName = null

  def getProperty(name: String): AnyRef =
    if (name == "expression")
      Expression.this.getLocation
    else
      null

  def getProperties: collection.Iterator[String] = new MonoIterator("expression").asScala

  def addToPathMap(
    pathMap        : PathMap,
    pathMapNodeSet : PathMap.PathMapNodeSet
  ): PathMap.PathMapNodeSet = {

    val dependsOnFocus = ExpressionTool.dependsOnFocus(Expression.this)
    var attachmentPoint: PathMap.PathMapNodeSet = null
    if (pathMapNodeSet == null) {
      attachmentPoint = pathMapNodeSet
      if (dependsOnFocus) {
        val cie = new ContextItemExpression
        ExpressionTool.copyLocationInfo(Expression.this, cie)
        attachmentPoint = new PathMap.PathMapNodeSet(pathMap.makeNewRoot(cie))
      }
    } else {
      attachmentPoint = if (dependsOnFocus) pathMapNodeSet else null
    }
    val result = new PathMap.PathMapNodeSet
    for (o <- operands.asScala) {
      val usage = o.getUsage
      val child = o.getChildExpression
      var target = child.addToPathMap(pathMap, attachmentPoint)
      if (usage == OperandUsage.NAVIGATION) {
        target = target.createArc(AxisInfo.ANCESTOR_OR_SELF, NodeKindTest.ELEMENT)
        target = target.createArc(AxisInfo.DESCENDANT, NodeKindTest.ELEMENT)
      }
      result.addNodeSet(target)
    }
    if (getItemType.isInstanceOf[AtomicType]) {
      null
    } else {
      result
    }
  }

  def isSubtreeExpression: Boolean =
    if (ExpressionTool.dependsOnFocus(Expression.this)) {
      if ((getIntrinsicDependencies & StaticProperty.DEPENDS_ON_FOCUS) != 0)
        false
      else
        operands.asScala.forall(_.getChildExpression.isSubtreeExpression)
    } else {
      true
    }

  def isEqual(other: Expression): Boolean =
    (this eq other) || (hashCode == other.hashCode && equals(other))

  override def hashCode: Int = {
    if (cachedHashCode == -1)
      cachedHashCode = computeHashCode()
    cachedHashCode
  }

  def hasCompatibleStaticContext(other: Expression): Boolean = {
    val d1 = (getIntrinsicDependencies & StaticProperty.DEPENDS_ON_STATIC_CONTEXT) != 0
    val d2 = (other.getIntrinsicDependencies & StaticProperty.DEPENDS_ON_STATIC_CONTEXT) != 0
    if (d1 != d2)
      false
    else if (d1)
      getRetainedStaticContext == other.getRetainedStaticContext
    else
      true
  }

  def computeHashCode(): Int = super.hashCode

  def isIdentical(other: IdentityComparable): Boolean = this eq other

  def identityHashCode(): Int =
    System.identityHashCode(Expression.this.getLocation)

  def setExtraProperty(name: String, value: AnyRef): Unit = {
    if (extraProperties == null) {
      if (value == null)
        return
      extraProperties = new ju.HashMap(4)
    }
    if (value == null)
      extraProperties.remove(name)
    else
      extraProperties.put(name, value)
  }

  def getExtraProperty(name: String): Any =
    if (extraProperties == null)
      null
    else
      extraProperties.get(name)

  def getStreamerName: String = null
}
