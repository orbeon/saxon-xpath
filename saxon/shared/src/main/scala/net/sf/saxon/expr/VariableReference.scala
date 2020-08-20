package net.sf.saxon.expr

import net.sf.saxon.event.Outputter

import net.sf.saxon.event.ReceiverOption

import net.sf.saxon.expr.flwor.LocalVariableBinding

import net.sf.saxon.expr.instruct.GlobalParam

import net.sf.saxon.expr.instruct.LocalParam

import net.sf.saxon.expr.instruct.LocalParamBlock

import net.sf.saxon.expr.parser._

import net.sf.saxon.lib.StandardDiagnostics

import net.sf.saxon.model._

import net.sf.saxon.om._

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.s9api.Location

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Cardinality

import net.sf.saxon.value.IntegerValue

import net.sf.saxon.value.SequenceType

import scala.beans.{BeanProperty, BooleanBeanProperty}

import Expression._

abstract class VariableReference(qnameOrBinding: StructuredQName Either Binding)
  extends Expression with BindingReference {

  @BeanProperty var variableName: StructuredQName = qnameOrBinding.fold(identity, _.getVariableQName)

  // This will be null until fixup() is called; it will also be null if the variable reference has been inlined
   var binding: Binding = null

  locally {
    qnameOrBinding foreach fixup
  }

  var staticType: SequenceType = null
  var constantValue: GroundedValue = null
  var flattened: Boolean = false
  @BooleanBeanProperty
  var inLoop: Boolean = false
  var filtered: Boolean = false

  override def setFlattened(flattened: Boolean): Unit = {
    this.flattened = flattened
  }

  def isFlattened: Boolean = flattened

  def copy(rebindings: RebindingMap): Expression

  override def getNetCost(): Int = 0

   def copyFrom(ref: VariableReference): Unit = {
    binding = ref.binding
    staticType = ref.staticType
    constantValue = ref.constantValue
    variableName = ref.variableName
    flattened = ref.flattened
    inLoop = ref.inLoop
    filtered = ref.filtered
    ExpressionTool.copyLocationInfo(ref, this)
  }

  def isFiltered: Boolean = filtered

  def setStaticType(sequenceType: SequenceType,
                    value: GroundedValue,
                    properties: Int): Unit = {
    var seqTpe = sequenceType
    if (seqTpe == null) {
      seqTpe = SequenceType.ANY_SEQUENCE
    }
    staticType = seqTpe
    constantValue = value
    val dependencies = getDependencies
    staticProperties = (properties & ~StaticProperty.CONTEXT_DOCUMENT_NODESET &
      ~StaticProperty.ALL_NODES_NEWLY_CREATED) |
      StaticProperty.NO_NODES_NEWLY_CREATED |
      seqTpe.getCardinality |
      dependencies
  }

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    if (constantValue != null) {
      binding = null
      return Literal.makeLiteral(constantValue, this)
    }
    if (binding != null) {
      recomputeInLoop()
      binding.addReference(this, inLoop)
    }
    this
  }

  def recomputeInLoop(): Unit = {
    inLoop = ExpressionTool.isLoopingReference(this, binding)
  }

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    if (binding.isInstanceOf[LetExpression] &&
      binding
        .asInstanceOf[LetExpression]
        .getSequence
        .isInstanceOf[Literal] &&
      !binding.asInstanceOf[LetExpression].isIndexedVariable) {
      val `val`: Expression = binding.asInstanceOf[LetExpression].getSequence
      Optimizer.trace(visitor.getConfiguration,
        "Replaced variable " + getDisplayName + " by its value",
        `val`)
      binding = null
      `val`.copy(new RebindingMap())
    }
    if (constantValue != null) {
      binding = null
      val result: Expression = Literal.makeLiteral(constantValue, this)
      ExpressionTool.copyLocationInfo(this, result)
      Optimizer.trace(visitor.getConfiguration,
        "Replaced variable " + getDisplayName + " by its value",
        result)
      result
    }
    if (binding.isInstanceOf[GlobalParam] && binding
      .asInstanceOf[GlobalParam]
      .isStatic) {
      val select: Expression = binding.asInstanceOf[GlobalParam].getBody
      if (select.isInstanceOf[Literal]) {
        binding = null
        Optimizer.trace(
          visitor.getConfiguration,
          "Replaced static parameter " + getDisplayName + " by its value",
          select)
        select
      }
    }
    this
  }

  def fixup(newBinding: Binding): Unit = {

    val indexed = binding.isInstanceOf[LocalBinding] && binding.asInstanceOf[LocalBinding].isIndexedVariable
    this.binding = newBinding

    if (indexed && newBinding.isInstanceOf[LocalBinding])
      newBinding.asInstanceOf[LocalBinding].setIndexedVariable()

    resetLocalStaticProperties()
  }

  def refineVariableType(`type`: ItemType,
                         cardinality: Int,
                         constantValue: GroundedValue,
                         properties: Int): Unit = {
    val th: TypeHierarchy = getConfiguration.getTypeHierarchy
    val oldItemType: ItemType = getItemType
    var newItemType: ItemType = oldItemType
    if (th.isSubType(`type`, oldItemType)) {
      newItemType = `type`
    }
    if (oldItemType.isInstanceOf[NodeTest] && `type`
      .isInstanceOf[AtomicType]) {
      newItemType = `type`
    }
    var newcard: Int = cardinality & getCardinality
    if (newcard == 0) {
      newcard = getCardinality
    }
    val seqType: SequenceType =
      SequenceType.makeSequenceType(newItemType, newcard)
    setStaticType(seqType, constantValue, properties)
  }

  def getItemType(): ItemType =
    if (staticType == null ||
      staticType.getPrimaryType == AnyItemType.getInstance) {
      if (binding != null) {
        val st: SequenceType = binding.getRequiredType
        if (st != null) {
          st.getPrimaryType
        }
      }
      AnyItemType.getInstance
    } else {
      staticType.getPrimaryType
    }

  override def getStaticUType(contextItemType: UType): UType = {
    if (binding != null) {
      if (binding.isGlobal || binding.isInstanceOf[LocalParam] ||
        (binding.isInstanceOf[LetExpression] && binding
          .asInstanceOf[LetExpression]
          .isInstruction) ||
        binding.isInstanceOf[LocalVariableBinding]) {
        val st: SequenceType = binding.getRequiredType
        if (st != null) {
          st.getPrimaryType.getUType
        } else {
          UType.ANY
        }
      } else if (binding.isInstanceOf[Assignation]) {
        binding
          .asInstanceOf[Assignation]
          .getSequence
          .getStaticUType(contextItemType)
      }
    }
    UType.ANY
  }

  override def getIntegerBounds(): Array[IntegerValue] =
    if (binding != null) {
      binding.getIntegerBoundsForVariable
    } else {
      null
    }

  def computeCardinality(): Int =
    if (staticType == null) {
      if (binding == null) {
        StaticProperty.ALLOWS_ZERO_OR_MORE
      } else if (binding.isInstanceOf[LetExpression]) {
        binding.getRequiredType.getCardinality
      } else if (binding.isInstanceOf[Assignation]) {
        StaticProperty.EXACTLY_ONE
      } else if (binding.getRequiredType == null) {
        StaticProperty.ALLOWS_ZERO_OR_MORE
      } else {
        binding.getRequiredType.getCardinality
      }
    } else {
      staticType.getCardinality
    }

  override def computeSpecialProperties(): Int = {
    var p: Int = super.computeSpecialProperties()
    if (binding == null || !binding.isAssignable) {
      p |= StaticProperty.NO_NODES_NEWLY_CREATED
    }
    if (binding.isInstanceOf[Assignation]) {
      val exp: Expression = binding.asInstanceOf[Assignation].getSequence
      if (exp != null) {
        p |= exp.getSpecialProperties & StaticProperty.NOT_UNTYPED_ATOMIC
      }
    }
    if (staticType != null && !Cardinality.allowsMany(
      staticType.getCardinality) &&
      staticType.getPrimaryType.isInstanceOf[NodeTest]) {
      p |= StaticProperty.SINGLE_DOCUMENT_NODESET
    }
    p & ~StaticProperty.ALL_NODES_NEWLY_CREATED
  }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[VariableReference] &&
      binding == other.asInstanceOf[VariableReference].binding &&
      binding != null

  override def computeHashCode(): Int =
    if (binding == null) 73619830 else binding.hashCode

  override def getIntrinsicDependencies(): Int = {
    var d: Int = 0
    if (binding == null) {
      d |= StaticProperty.DEPENDS_ON_LOCAL_VARIABLES | StaticProperty.DEPENDS_ON_ASSIGNABLE_GLOBALS |
        StaticProperty.DEPENDS_ON_RUNTIME_ENVIRONMENT
    } else if (binding.isGlobal) {
      if (binding.isAssignable) {
        d |= StaticProperty.DEPENDS_ON_ASSIGNABLE_GLOBALS
      }
      if (binding.isInstanceOf[GlobalParam]) {
        d |= StaticProperty.DEPENDS_ON_RUNTIME_ENVIRONMENT
      }
    } else {
      d |= StaticProperty.DEPENDS_ON_LOCAL_VARIABLES
    }
    d
  }

  def getImplementationMethod(): Int =
    (if (Cardinality.allowsMany(getCardinality)) 0 else EVALUATE_METHOD) |
      ITERATE_METHOD |
      PROCESS_METHOD

  override def getScopingExpression(): Expression = {
    if (binding.isInstanceOf[Expression]) {
      if (binding.isInstanceOf[LocalParam] &&
        binding
          .asInstanceOf[LocalParam]
          .getParentExpression
          .isInstanceOf[LocalParamBlock]) {
        val block: LocalParamBlock = binding
          .asInstanceOf[LocalParam]
          .getParentExpression
          .asInstanceOf[LocalParamBlock]
        block.getParentExpression
      } else {
        binding.asInstanceOf[Expression]
      }
    }
    var parent: Expression = getParentExpression
    while (parent != null) {
      if (parent.hasVariableBinding(binding)) {
        parent
      }
      parent = parent.getParentExpression
    }
    null
  }

  override def addToPathMap(
                             pathMap: PathMap,
                             pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet =
    pathMap.getPathForVariable(getBinding).asInstanceOf[PathMap.PathMapNodeSet]

  override def iterate(c: XPathContext): SequenceIterator =
    try {
      val actual: Sequence = evaluateVariable(c)
      assert(actual != null)
      actual.iterate()
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getLocation)
        throw err
      }

      case err: NullPointerException => {
        err.printStackTrace()
        val msg: String = "Internal error: no value for variable $" + getDisplayName +
          " at line " +
          getLocation.getLineNumber +
          (if (getLocation.getSystemId == null) ""
          else " of " + getLocation.getSystemId)
        new StandardDiagnostics()
          .printStackTrace(c, c.getConfiguration.getLogger, 2)
        throw new AssertionError(msg)
      }

      case err: AssertionError => {
        err.printStackTrace()
        val msg: String = err.getMessage + ". Variable reference $" + getDisplayName +
          " at line " +
          getLocation.getLineNumber +
          (if (getLocation.getSystemId == null) ""
          else " of " + getLocation.getSystemId)
        new StandardDiagnostics()
          .printStackTrace(c, c.getConfiguration.getLogger, 2)
        throw new AssertionError(msg)
      }

    }

  override def evaluateItem(c: XPathContext): Item =
    try {
      val actual: Sequence = evaluateVariable(c)
      assert(actual != null)
      actual.head()
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getLocation)
        throw err
      }

    }

  override def process(output: Outputter, c: XPathContext): Unit = {
    try {
      val iter: SequenceIterator = evaluateVariable(c).iterate()
      val loc: Location = getLocation
      iter.forEachOrFail((item) =>
        output.append(item, loc, ReceiverOption.ALL_NAMESPACES))
    } catch {
      case err: XPathException => {
        err.maybeSetLocation(getLocation)
        throw err
      }

    }
  }

  def evaluateVariable(c: XPathContext): Sequence =
    try binding.evaluateVariable(c)
    catch {
      case err: NullPointerException =>
        if (binding == null) {
          throw new IllegalStateException(
            "Variable $" + variableName.getDisplayName + " has not been fixed up")
        } else {
          throw err
        }

    }

  def getBinding(): Binding = binding

  def getDisplayName(): String =
    if (binding != null) {
      binding.getVariableQName.getDisplayName
    } else {
      variableName.getDisplayName
    }

  def getEQName(): String =
    if (binding != null) {
      val q: StructuredQName = binding.getVariableQName
      if (q.hasURI("")) {
        q.getLocalPart
      } else {
        q.getEQName
      }
    } else {
      variableName.getEQName
    }

  override def toString(): String = {
    val d: String = getEQName
    "$" + (if (d == null) "$" else d)
  }

  override def toShortString(): String = "$" + getDisplayName

  override def getExpressionName(): String = "varRef"

  def export(destination: ExpressionPresenter): Unit = {
    destination.startElement("varRef", this)
    destination.emitAttribute("name", variableName)
    if (binding.isInstanceOf[LocalBinding]) {
      destination.emitAttribute(
        "slot",
        "" +
          binding.asInstanceOf[LocalBinding].getLocalSlotNumber)
    }
    destination.endElement()
  }

  override def getStreamerName(): String = "VariableReference"

}
