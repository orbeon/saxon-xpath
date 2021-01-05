package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.expr.parser.PathMap
import org.orbeon.saxon.model.ItemType
import org.orbeon.saxon.om.{GroundedValue, NodeInfo, Sequence}
import org.orbeon.saxon.value.IntegerValue
//import scala.collection.compat._
import scala.jdk.CollectionConverters._
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.trans.XPathException
import scala.util.control.Breaks._
import org.orbeon.saxon.value.SequenceType
import java.util

object Assignation {
  private val REPEATED_ACTION_ROLE = new OperandRole(OperandRole.HIGHER_ORDER, OperandUsage.TRANSMISSION)

  private def countReferences(binding: Binding, exp: Expression, references: util.List[VariableReference], results: Array[Int]): Unit = { // results[0] = nominal reference count
    // results[1] = quota nodes visited
    exp match {
      case ref: LocalVariableReference =>
        if (ref.getBinding eq binding) {
          ref.recomputeInLoop()

          if (ref.isInLoop)
            results(0) += 10
          else
            results(0) += 1

          references.add(ref)
        }
      case _ => if ((exp.getDependencies & StaticProperty.DEPENDS_ON_LOCAL_VARIABLES) != 0) if ( {
        results(1) -= 1
        results(1)
      } <= 0) { // abandon the search
        results(0) = 100
        results(1) = 0
      }
      else {

        for (o <- exp.operands.asScala) {
          countReferences(binding, o.getChildExpression, references, results)
        }
      }
    }
  }
}

abstract class Assignation extends Expression with LocalBinding {

  private val sequenceOp = new Operand(this, null, OperandRole.NAVIGATE)

  private val actionOp   =
    new Operand(
      this,
      null,
      if (this.isInstanceOf[LetExpression])
        OperandRole.SAME_FOCUS_ACTION
      else
        Assignation.REPEATED_ACTION_ROLE
    )

  var slotNumber: Int = -999 // slot number for range variable
  // (initialized to ensure a crash if no real slot is allocated)

  var variableName: StructuredQName = null
  var requiredType: SequenceType = null
  var isIndexedVariable: Boolean = false
  var hasLoopingReference = false
  var references: util.ArrayList[VariableReference] = null

  def getSequenceOp: Operand = sequenceOp

  def getActionOp: Operand = actionOp

  override def operands: util.List[Operand] = operandList(sequenceOp, actionOp)

  /**
   * Set the required type (declared type) of the variable
   *
   * @param requiredType the required type
   */
  def setRequiredType(requiredType: SequenceType): Unit = this.requiredType = requiredType

  /**
   * Set the name of the variable
   *
   * @param variableName the name of the variable
   */
  def setVariableQName(variableName: StructuredQName): Unit = this.variableName = variableName

  /**
   * Get the name of the variable
   *
   * @return the variable name, as a QName
   */
  override def getVariableQName: StructuredQName = variableName

  override def getObjectName: StructuredQName = variableName

  /**
   * Get the declared type of the variable
   *
   * @return the declared type
   */
  def getRequiredType: SequenceType = requiredType

  /**
   * If the variable is bound to an integer, get the minimum and maximum possible values.
   * Return null if unknown or not applicable
   */
  def getIntegerBoundsForVariable: Array[IntegerValue] = getSequence.getIntegerBounds

  /**
   * If this is a local variable held on the local stack frame, return the corresponding slot number.
   * In other cases, return -1.
   */
  def getLocalSlotNumber: Int = slotNumber

  /**
   * Compute the dependencies of an expression, as the union of the
   * dependencies of its sub-expressions. (This is overridden for path expressions
   * and filter expressions, where the dependencies of a sub-expression are not all
   * propagated). This method should be called only once, to compute the dependencies;
   * after that, getDependencies should be used.
   *
   * @return the dependencies, as a bit-mask
   */
  override def computeDependencies(): Int = {
    var d = super.computeDependencies()
    // Unset the DEPENDS_ON_LOCAL_VARIABLES bit if the only dependencies are to
    // variables declared within the expression itself (typically, the variable
    // bound by this Assignation)
    if (!ExpressionTool.containsLocalVariableReference(this))
      d &= ~StaticProperty.DEPENDS_ON_LOCAL_VARIABLES
    d
  }

  /**
   * Get the value of the range variable
   */
  @throws[XPathException]
  override def evaluateVariable(context: XPathContext): Sequence = {
    var actual = context.evaluateLocalVariable(slotNumber)
    if (!(actual.isInstanceOf[GroundedValue] || actual.isInstanceOf[NodeInfo])) {
      actual = actual.materialize
      context.setLocalVariable(slotNumber, actual)
    }
    actual
  }

  /**
   * Add the "return" or "satisfies" expression, and fix up all references to the
   * range variable that occur within that expression
   *
   * @param action the expression that occurs after the "return" keyword of a "for"
   *               expression, the "satisfies" keyword of "some/every", or the ":=" operator of
   *               a "let" expression.
   */
  def setAction(action: Expression): Unit = actionOp.setChildExpression(action)

  /**
   * Indicate whether the binding is local or global. A global binding is one that has a fixed
   * value for the life of a query or transformation; any other binding is local.
   */
  override final def isGlobal = false

  /**
   * Test whether it is permitted to assign to the variable using the saxon:assign
   * extension element. This will only be for an XSLT global variable where the extra
   * attribute saxon:assignable="yes" is present.
   */
  override final def isAssignable = false

  /**
   * Check to ensure that this expression does not contain any inappropriate updating subexpressions.
   * This check is overridden for those expressions that permit updating subexpressions.
   *
   * @throws XPathException
   * if the expression has a non-permitted updateing subexpression
   */
  @throws[XPathException]
  override def checkForUpdatingSubexpressions(): Unit = {
    getSequence.checkForUpdatingSubexpressions()
    if (getSequence.isUpdatingExpression) {
      val err = new XPathException("An updating expression cannot be used to initialize a variable", "XUST0001")
      err.setLocator(getSequence.getLocation)
      throw err
    }
    getAction.checkForUpdatingSubexpressions()
  }

  /**
   * Determine whether this is an updating expression as defined in the XQuery update specification
   *
   * @return true if this is an updating expression
   */
  override def isUpdatingExpression: Boolean = getAction.isUpdatingExpression

  /**
   * Get the action expression
   *
   * @return the action expression (introduced by "return" or "satisfies")
   */
  def getAction: Expression = actionOp.getChildExpression

  /**
   * Set the "sequence" expression - the one to which the variable is bound
   *
   * @param sequence the expression to which the variable is bound
   */
  def setSequence(sequence: Expression): Unit = sequenceOp.setChildExpression(sequence)

  /**
   * Get the "sequence" expression - the one to which the variable is bound
   *
   * @return the expression to which the variable is bound
   */
  def getSequence: Expression = sequenceOp.getChildExpression

  /**
   * Set the slot number for the range variable
   *
   * @param nr the slot number to be used
   */
  def setSlotNumber(nr: Int): Unit = slotNumber = nr

  /**
   * Get the number of slots required. Normally 1, except for a FOR expression with an AT clause, where it is 2.
   *
   * @return the number of slots required
   */
  def getRequiredSlots = 1

  override def hasVariableBinding(binding: Binding): Boolean = this eq binding

  /**
   * Replace this expression by a simpler expression that delivers the results without regard
   * to order.
   *
   * @param retainAllNodes set to true if the result must contain exactly the same nodes as the
   *                       original; set to false if the result can eliminate (or introduce) duplicates.
   * @param forStreaming   set to true if optimizing for streaming
   */
  @throws[XPathException]
  override def unordered(retainAllNodes: Boolean, forStreaming: Boolean): Expression = {
    setAction(getAction.unordered(retainAllNodes, forStreaming))
    this
  }

  /**
   * Return the estimated cost of evaluating an expression. This is a very crude measure based
   * on the syntactic form of the expression (we have no knowledge of data values). We take
   * the cost of evaluating a simple scalar comparison or arithmetic expression as 1 (one),
   * and we assume that a sequence has length 5. The resulting estimates may be used, for
   * example, to reorder the predicates in a filter expression so cheaper predicates are
   * evaluated first.
   *
   * @return the estimated cost
   */
  override def getCost: Double = getSequence.getCost + 5 * getAction.getCost

  /**
   * Suppress validation on contained element constructors, on the grounds that the parent element
   * is already performing validation. The default implementation does nothing.
   */
  override def suppressValidation(validationMode: Int): Unit = getAction.suppressValidation(validationMode)

  /**
   * Add a representation of this expression to a PathMap. The PathMap captures a map of the nodes visited
   * by an expression in a source tree.
   * <p>The default implementation of this method assumes that an expression does no navigation other than
   * the navigation done by evaluating its subexpressions, and that the subexpressions are evaluated in the
   * same context as the containing expression. The method must be overridden for any expression
   * where these assumptions do not hold. For example, implementations exist for AxisExpression, ParentExpression,
   * and RootExpression (because they perform navigation), and for the doc(), document(), and collection()
   * functions because they create a new navigation root. Implementations also exist for PathExpression and
   * FilterExpression because they have subexpressions that are evaluated in a different context from the
   * calling expression.</p>
   *
   * @param pathMap        the PathMap to which the expression should be added
   * @param pathMapNodeSet the PathMapNodeSet to which the paths embodied in this expression should be added
   * @return the pathMapNodeSet representing the points in the source document that are both reachable by this
   *         expression, and that represent possible results of this expression. For an expression that does
   *         navigation, it represents the end of the arc in the path map that describes the navigation route. For other
   *         expressions, it is the same as the input pathMapNode.
   */
  override def addToPathMap(pathMap: PathMap, pathMapNodeSet: PathMap.PathMapNodeSet): PathMap.PathMapNodeSet = {
    val varPath = getSequence.addToPathMap(pathMap, pathMapNodeSet)
    pathMap.registerPathForVariable(this, varPath)
    getAction.addToPathMap(pathMap, pathMapNodeSet)
  }

  /**
   * Get the display name of the range variable, for diagnostics only
   *
   * @return the lexical QName of the range variable. For system allocated
   *         variables, the conventional namespace prefix "zz" is used.
   */
  def getVariableName: String =
    if (variableName == null)
      "zz:var" + computeHashCode
    else
      variableName.getDisplayName

  /**
   * Get the name of the range variable as a Name or EQName.
   *
   * @return the name of the range variable. For system allocated
   *         variables, the namespace "http://ns.saxonica.com/anonymous-var"
   *         is used. For names in no namespace, the local name alone is used
   */
  def getVariableEQName: String =
    if (variableName == null)
      "Q{http://ns.saxonica.com/anonymous-var}var" + computeHashCode
    else if (variableName.hasURI(""))
      variableName.getLocalPart
    else
      variableName.getEQName

  /**
   * Refine the type information associated with this variable declaration. This is useful when the
   * type of the variable has not been explicitly declared (which is common); the variable then takes
   * a static type based on the type of the expression to which it is bound. The effect of this call
   * is to update the static expression type for all references to this variable.
   *
   * @param type              the inferred item type of the expression to which the variable is bound
   * @param cardinality       the inferred cardinality of the expression to which the variable is bound
   * @param constantValue     the constant value to which the variable is bound (null if there is no constant value)
   * @param properties        other static properties of the expression to which the variable is bound
   * @param currentExpression the expression that binds the variable
   * @throws XPathException if things go wrong
   */
  @throws[XPathException]
  def refineTypeInformation(`type`: ItemType, cardinality: Int, constantValue: GroundedValue, properties: Int, currentExpression: Assignation): Boolean =
    ExpressionTool.processExpressionTree(currentExpression.getAction, null, (exp: Expression, result: Any) => {
    def foo(exp: Expression, result: Any) = {
      exp match {
        case varRef: VariableReference if varRef.getBinding eq currentExpression =>
          varRef.refineVariableType(`type`, cardinality, constantValue, properties)
        case _ =>
      }
      false
    }

    foo(exp, result)
  })

  /**
   * Register a variable reference that refers to the variable bound in this expression
   *
   * @param ref                the variable reference
   * @param isLoopingReference - true if the reference occurs within a loop, such as the predicate
   *                           of a filter expression
   */
  override def addReference(ref: VariableReference, isLoopingReference: Boolean): Unit = {
    hasLoopingReference |= isLoopingReference
    if (references == null)
      references = new util.ArrayList[VariableReference]()

    for (vr <- references.asScala)
      if (vr eq ref)
        return

    references.add(ref)
  }

  /**
   * Get the (nominal) count of the number of references to this variable
   *
   * @return zero if there are no references, one if there is a single reference that is not in
   *         a loop, some higher number if there are multiple references (or a single reference in a loop),
   *         or the special value @link RangeVariable#FILTERED} if there are any references
   *         in filter expressions that require searching.
   */
  def getNominalReferenceCount: Int =
    if (isIndexedVariable)
      FilterExpression.FILTERED
    else if (references == null || hasLoopingReference)
      10
    else
      references.size

  /**
   * Remove dead references from the reference list of the variable; at the same time, check whether
   * any of the variable references is in a loop, and return true if so. References are considered dead
   * if they do not have this Binding as an ancestor in the expression tree; this typically occurs because
   * they are in a part of the tree that has been rewritten or removed.
   *
   * @return true if any of the references in the reference list occurs within a looping construct.
   */
  def removeDeadReferences(): Boolean = {
    var inLoop = false
    if (references != null)
      for (i <- references.size - 1 to 0 by -1) {
        // Check whether the reference still has this Assignation as an ancestor in the expression tree
        var found = false
        inLoop |= references.get(i).isInLoop
        var parent = references.get(i).getParentExpression
        breakable {
          while (parent != null)
            if (parent eq this) {
              found = true
              break()
            } else
              parent = parent.getParentExpression
        }
        if (!found)
          references.remove(i)
      }
    inLoop
  }

  /**
   * This method recomputes the reference list by scanning the subtree rooted at this variable binding.
   * If the scan proves expensive, or if more than two references are found, or if a looping reference is found,
   * then the scan is abandoned. On completion the reference list for the variable is either accurate, or
   * is null.
   */
  def verifyReferences(): Unit = rebuildReferenceList(false)

  /**
   * Rebuild the reference list to guide subsequent optimization.
   *
   * @param force if true, the search is exhaustive. If false, the search (and therefore the attempt to
   *              inline variables) is abandoned after a while to avoid excessive cost. This happens when
   *              a stylesheet contains very large templates or functions.
   */
  def rebuildReferenceList(force: Boolean): Unit = {
    val results = Array[Int](0, if (force) Integer.MAX_VALUE
    else 500)
    val references = new util.ArrayList[VariableReference]
    Assignation.countReferences(this, getAction, references, results)
    this.references = if (results(1) <= 0) null
    else references
  }

  /**
   * Replace all references to the variable bound by this let expression,
   * that occur within the action expression, with the given expression
   *
   * @param seq the expression
   * @return true if the variable was successfully inlined. (Returns false, for example,
   *         if a variable reference occurs inside a try/catch, which inhibits inlining).
   */
  def replaceVariable(seq: Expression): Boolean = {
    val done = ExpressionTool.inlineVariableReferences(getAction, this, seq)
    if (done && isIndexedVariable && seq.isInstanceOf[VariableReference]) {
      val newBinding = seq.asInstanceOf[VariableReference].getBinding
      newBinding match {
        case assignation: Assignation => assignation.setIndexedVariable()
        case _ =>
      }
    }
    done
  }

  /**
   * Indicate that the variable bound by this let expression should be indexable
   * (because it is used in an appropriate filter expression)
   */
  def setIndexedVariable(): Unit = isIndexedVariable = true
}